const path = require('path');

module.exports = {
    entry: './index-dev.js',
    output: {
        filename: 'main.js',
        path: path.resolve(__dirname, 'dist'),
    },
};
