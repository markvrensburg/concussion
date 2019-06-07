var globalModules = {
  //"brace": "brace"
};

//const importRule = {
//  // Force require global modules
//  test: /.*-(fast|full)opt\.js$/,
//  loader:
//    "imports-loader?" +
//    Object.keys(globalModules)
//      .map(function(modName) {
//        return modName + "=" + globalModules[modName];
//      })
//      .join(",")
//};
//
//const exposeRules = Object.keys(globalModules).map(function(modName) {
//  // Expose global modules
//  return {
//    test: require.resolve(modName),
//    loader: "expose-loader?" + globalModules[modName]
//  };
//});
//
//const cssRule = {
//    test: /\.css$/,
//    use: ["style-loader", "css-loader"]
//}

//const allRules = exposeRules.concat(importRule).concat(cssRule);

module.exports = {
  performance: {
    hints: false
  },
  module: {
    rules: [
       { test: /\.css$/, use: ["style-loader", "css-loader"]},
       { test: /\.ttf$/, loader: 'ignore-loader' },
       { test: /\.woff/, loader: 'ignore-loader' },
       { test: /\.woff2/, loader: 'ignore-loader' },
       { test: /\.eot/, loader: 'ignore-loader' },
       { test: /\.svg/, loader: 'ignore-loader' },
       { test: /\.png/, loader: 'ignore-loader' },
    ]
  }
};