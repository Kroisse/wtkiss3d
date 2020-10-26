use std::borrow::Cow;
use std::cell::RefCell;
use std::io::Read;
use std::rc::Rc;

use kiss3d::{
    light::Light,
    nalgebra::{self as na, Point2, UnitQuaternion, Vector3},
    ncollide3d::math::Point,
    resource::{Mesh, MeshManager, TextureManager},
    scene::SceneNode,
    window::{State, Window},
};
use url::Url;

type Error = Box<dyn std::error::Error>;

struct AppState {
    c: SceneNode,
    rot: UnitQuaternion<f32>,
}

impl State for AppState {
    fn step(&mut self, _: &mut Window) {
        self.c.prepend_to_local_rotation(&self.rot);
    }
}

fn main() {
    let uri = std::env::var("GLTF_URL").unwrap();

    let mut window = Window::new("Hi");
    window.set_light(Light::StickToCamera);
    let rot = UnitQuaternion::from_axis_angle(&Vector3::y_axis(), 0.014);

    let c = load_resource(&uri).unwrap();
    window.scene_mut().add_child(c.clone());

    let state = AppState { c, rot };
    window.render_loop(state)
}

fn load_resource(uri: &str) -> Result<SceneNode, Error> {
    let base_url = Url::parse(uri)?;
    let options = Url::options().base_url(Some(&base_url));
    let resp = ureq::get(uri).call();
    if resp.error() {
        return Err(resp.status_text().into());
    }
    let mut buf = vec![];
    resp.into_reader().read_to_end(&mut buf)?;
    let reader = std::io::Cursor::new(&buf[..]);
    let gltf = gltf::Gltf::from_reader(reader)?;

    let buffers = gltf
        .buffers()
        .map(|buf| {
            println!("{} {:?}", buf.length(), buf.source());
            Ok(match buf.source() {
                gltf::buffer::Source::Uri(path) => {
                    let uri = options.parse(path)?;
                    let resp = ureq::get(uri.as_str()).call();
                    if resp.error() {
                        return Err(resp.status_text().into());
                    }
                    let mut buf = vec![];
                    resp.into_reader().read_to_end(&mut buf)?;
                    Cow::Owned(buf)
                }
                gltf::buffer::Source::Bin => Cow::Borrowed(gltf.blob.as_deref().unwrap()),
            })
        })
        .collect::<Result<Vec<_>, Error>>()?;

    let textures = TextureManager::get_global_manager(|manager| {
        gltf.textures()
            .map(|texture| {
                let img = load_image(&texture, &options, &buffers)?;
                Ok(manager.add_image(img, texture.name().unwrap_or("")))
            })
            .collect::<Result<Vec<_>, Error>>()
    })?;

    let meshes = MeshManager::get_global_manager(|manager| {
        gltf.meshes()
            .map(|m| {
                let mesh = load_mesh(&m, &buffers);
                manager.add(Rc::clone(&mesh), m.name().unwrap_or(""));
                mesh
            })
            .collect::<Vec<_>>()
    });

    let nodes = gltf
        .nodes()
        .map(|node| load_node(&node, &meshes))
        .collect::<Vec<_>>();

    if let Some(scene) = gltf.scenes().next() {
        let scene = nodes[scene.index()].clone();
        Ok(scene)
    } else {
        Err("no scenes".into())
    }
}

fn parse_mime_type(mime_type: &str) -> image::ImageFormat {
    match mime_type {
        "image/jpeg" => image::ImageFormat::JPEG,
        "image/png" => image::ImageFormat::PNG,
        otherwise => {
            panic!(format!("Unsupported: {}", otherwise));
        }
    }
}

fn load_node(node: &gltf::Node, meshes: &[Rc<RefCell<Mesh>>]) -> SceneNode {
    let mut scene = SceneNode::new_empty();
    if let Some(mesh) = node.mesh() {
        scene.add_mesh(
            Rc::clone(&meshes[mesh.index()]),
            Vector3::from_element(0.01),
        );
    }
    for child in node.children() {
        scene.add_child(load_node(&child, meshes));
    }
    match node.transform() {
        gltf::scene::Transform::Matrix { matrix } => {
            // scene.set_local_transformation(matrix);
        }
        gltf::scene::Transform::Decomposed {
            translation: [tx, ty, tz],
            rotation: [ri, rj, rk, rw],
            scale: [sx, sy, sz],
        } => {
            scene.set_local_translation(na::Translation3::new(tx, ty, tz));
            scene.set_local_rotation(na::Unit::new_normalize(na::Quaternion::new(rw, ri, rj, rk)));
            scene.set_local_scale(sx, sy, sz);
        }
    }

    scene
}

fn load_image(
    texture: &gltf::Texture,
    options: &url::ParseOptions,
    buffers: &[impl AsRef<[u8]>],
) -> Result<image::DynamicImage, Error> {
    let image = texture.source();
    let img = match image.source() {
        gltf::image::Source::View { view, mime_type } => {
            let buffer_data = buffers[view.buffer().index()].as_ref();
            let begin = view.offset();
            let buf = &buffer_data[begin..begin + view.length()];
            let format = parse_mime_type(mime_type);
            image::load_from_memory_with_format(buf, format)?
        }
        gltf::image::Source::Uri { uri, mime_type } => {
            let uri = options.parse(uri)?;
            let resp = ureq::get(uri.as_str()).call();
            let format = if let Some(m) = mime_type.or_else(|| resp.header("Content-Type")) {
                parse_mime_type(m)
            } else {
                panic!();
            };
            let mut buf = vec![];
            resp.into_reader().read_to_end(&mut buf)?;
            let reader = std::io::Cursor::new(&buf[..]);

            image::load(reader, format)?
        }
    };
    Ok(img)
}

fn load_mesh(mesh: &gltf::Mesh, buffers: &[impl AsRef<[u8]>]) -> Rc<RefCell<Mesh>> {
    let prim = mesh.primitives().next().unwrap();

    let reader = prim.reader(|b| buffers.get(b.index()).map(AsRef::as_ref));
    let mode = prim.mode();
    let coords = reader
        .read_positions()
        .expect("Primitive must have positions")
        .map(Point::from)
        .collect::<Vec<_>>();
    let normals = reader
        .read_normals()
        .map(|normals| normals.map(Vector3::from).collect());
    let faces = if let Some(indices) = reader.read_indices() {
        let mut indices = indices.into_u32();
        let mut faces = vec![];
        while let (Some(a), Some(b), Some(c)) = (indices.next(), indices.next(), indices.next()) {
            faces.push(Point::new(a as u16, b as u16, c as u16));
        }
        faces
    } else {
        (0..coords.len() / 3)
            .map(|i| {
                let n = i as u16 * 3;
                Point::new(n, n + 1, n + 2)
            })
            .collect()
    };
    let uvs = reader
        .read_tex_coords(0)
        .map(|tex| tex.into_f32().map(Point2::from).collect());

    let m = Mesh::new(coords, faces, normals, uvs, false);
    let m = Rc::new(RefCell::new(m));
    m
}
