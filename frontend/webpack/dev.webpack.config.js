var Webpack = require("webpack");
var HtmlWebpackPlugin = require('html-webpack-plugin');
var Path = require('path');
var Merge = require('webpack-merge');

var rootDir = Path.resolve(__dirname, '../../../..');
var resourcesDir = Path.resolve(rootDir, 'src/main/resources');

var generatedConfig = require('./scalajs.webpack.config');
var commonConfig = require('./common.webpack.config.js');

module.exports = Merge(generatedConfig, commonConfig, {
  plugins: [
    new HtmlWebpackPlugin({
      title: 'Concussion',
      favicon: Path.resolve(resourcesDir, './favicon.ico'),
    })
  ]
});