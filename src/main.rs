use std::cell::RefCell;
use std::io;
use std::rc::Rc;

use futures_util::future::try_join_all;
use gltf::Gltf;
use image::DynamicImage;
use kiss3d::{
    light::Light,
    nalgebra::{self as na, Point2, UnitQuaternion, Vector3},
    ncollide3d::math::Point,
    resource::{Mesh, MeshManager, TextureManager},
    scene::SceneNode,
    window::{State, Window},
};
use url::Url;
use wasm_bindgen::prelude::*;

#[global_allocator]
static ALLOC: wee_alloc::WeeAlloc = wee_alloc::WeeAlloc::INIT;

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

// #[async_std::main]
#[wasm_bindgen(start)]
pub async fn main() {
    let uri = std::env::var("GLTF_URL").unwrap();

    let (gltf, buffers, images) = load_gltf(&uri).await.unwrap();

    let mut window = Window::new("Hi");
    window.set_light(Light::StickToCamera);
    let rot = UnitQuaternion::from_axis_angle(&Vector3::y_axis(), 0.014);

    let c = load_scene(&gltf, &buffers, &images).unwrap();
    window.scene_mut().add_child(c.clone());

    let state = AppState { c, rot };
    window.render_loop(state)
}

async fn load_gltf(uri: &str) -> Result<(Gltf, Vec<Vec<u8>>, Vec<DynamicImage>), Error> {
    let base_url = Url::parse(uri)?;
    let options = Url::options().base_url(Some(&base_url));
    let mut resp = surf::get(uri).await?;
    let buf = resp.body_bytes().await?;
    let reader = io::Cursor::new(&buf);
    let gltf = gltf::Gltf::from_reader(reader)?;

    let buffers = gltf.buffers().map(|buf| async move {
        println!("{} {:?}", buf.length(), buf.source());
        Ok::<_, Error>(match buf.source() {
            gltf::buffer::Source::Uri(path) => {
                let uri = options.parse(path)?;
                let mut resp = surf::get(uri.as_str()).await?;
                let buf = resp.body_bytes().await?;
                Some(buf)
            }
            gltf::buffer::Source::Bin => None,
        })
    });
    let buffers = try_join_all(buffers)
        .await?
        .into_iter()
        .map(|b| {
            if let Some(buf) = b {
                buf
            } else {
                gltf.blob.as_deref().unwrap().to_owned()
            }
        })
        .collect::<Vec<_>>();
    let images = gltf
        .textures()
        .map(|texture| load_image(texture, &options, &buffers));
    let images = try_join_all(images).await?;

    Ok((gltf, buffers, images))
}

fn load_scene(
    gltf: &Gltf,
    buffers: &[impl AsRef<[u8]>],
    images: &[DynamicImage],
) -> Result<SceneNode, Error> {
    let textures = TextureManager::get_global_manager(|manager| {
        gltf.textures()
            .zip(images)
            .map(|(texture, image)| {
                Ok(manager.add_image(image.clone(), texture.name().unwrap_or("")))
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

async fn load_image<'a>(
    texture: gltf::Texture<'a>,
    options: &'a url::ParseOptions<'a>,
    buffers: &'a [impl AsRef<[u8]>],
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
            let mut resp = surf::get(uri.as_str()).await?;
            let format = if let Some(m) =
                mime_type.or_else(|| resp.header("Content-Type").map(|v| v.as_str()))
            {
                parse_mime_type(m)
            } else {
                panic!();
            };
            let reader = io::Cursor::new(resp.body_bytes().await?);
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