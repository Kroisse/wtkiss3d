export function fetch_image(uri) {
    const image = document.createElement('img');
    const canvas = document.createElement('canvas');

    image.crossOrigin = 'anonymous';

    return new Promise((resolve, reject) => {
        image.addEventListener('load', () => {
            try {
                const { width, height } = image;
                canvas.width = width;
                canvas.height = height;
                const ctx = canvas.getContext('2d');
                ctx.drawImage(image, 0, 0);

                const imageData = ctx.getImageData(0, 0, width, height);
                resolve(imageData);
            } catch (e) {
                reject(e);
            }
        });

        image.addEventListener('error', event => {
            reject(event.error);
        });

        image.src = uri;
    });
}
