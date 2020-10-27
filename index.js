async function main({ init, load_gltf }) {
    const engine = init();
    const gltf = await load_gltf('');
    engine.add_gltf(gltf);
}

import('./pkg')
    .then(main)
    .catch(console.error);
