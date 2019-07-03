const Webpack = require("webpack");
const Merge = require("webpack-merge");
const Path = require('path');
const CopyWebpackPlugin = require('copy-webpack-plugin');

var rootDir = Path.resolve(__dirname, '../../../..');
var resourcesDir = Path.resolve(rootDir, 'src/main/resources');

var generatedConfig = require('./scalajs.webpack.config');
var commonConfig = require('./common.webpack.config.js');

module.exports = Merge(generatedConfig, commonConfig, {
  mode: 'production',
  output: {
    filename: 'concussion.js'
  },
  plugins: [
    new CopyWebpackPlugin([
      {
        from: Path.resolve(resourcesDir, './favicon.ico'),
        to: '.',
      },
    ]),
  ],
});
