const Webpack = require("webpack");
const Merge = require("webpack-merge");

var generatedConfig = require('./scalajs.webpack.config');
var commonConfig = require('./common.webpack.config.js');

module.exports = Merge(generatedConfig, commonConfig, {
  mode: 'production',
  output: {
    filename: 'concussion.js'
  }
});
