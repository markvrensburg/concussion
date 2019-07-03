const Webpack = require("webpack");
const Merge = require('webpack-merge');
const Path = require('path');
const HtmlWebpackPlugin = require('html-webpack-plugin');

var rootDir = Path.resolve(__dirname, '../../../..');
var resourcesDir = Path.resolve(rootDir, 'src/main/resources');

var generatedConfig = require('./scalajs.webpack.config');
var commonConfig = require('./common.webpack.config.js');

module.exports = Merge(generatedConfig, commonConfig, {
  mode: 'development',
  plugins: [
    new HtmlWebpackPlugin({
      title: 'Concussion',
      filename: "index.html",
      template: Path.resolve(resourcesDir, './index.html'),
      favicon: Path.resolve(resourcesDir, './favicon.ico')
    })
  ]
});