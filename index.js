async function main({ init, load_gltf }) {
    const engine = init();

    const inputBox = document.getElementById('gltf-url');
    const gltfForm = document.getElementById('load-gltf');

    gltfForm.addEventListener('submit', async e => {
        e.preventDefault();
        e.target.disabled = true;
        try {
            const gltf = await load_gltf(inputBox.value);
            engine.add_gltf(gltf);
        } finally {
            e.target.disabled = false;
        }
    });
}

import('./pkg')
    .then(main)
    .catch(console.error);
