import('./pkg')
    .then(({ init }) => {
        const engine = init();
    })
    .catch(console.error);
