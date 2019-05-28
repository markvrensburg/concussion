const Webpack = require("webpack");
const Merge = require("webpack-merge");
const UglifyWebpackPlugin = require("uglifyjs-webpack-plugin");

var generatedConfig = require('./scalajs.webpack.config');
var commonConfig = require('./common.webpack.config.js');

module.exports = Merge(generatedConfig, commonConfig, {
  mode: 'production',
  output: {
    filename: 'concussion.js'
  },
  optimization: {
      minimize: true,
      minimizer: [
        new UglifyWebpackPlugin({
          parallel: true,
          uglifyOptions: { mangle: true },
          sourceMap: false
        })
      ]
    }
});
