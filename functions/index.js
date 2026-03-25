// Unified entry point for all Cloud Functions.
// Merges ClojureScript game functions with the JS billing cap function.

const gameExports = require("./dist/index.js");
const billingExports = require("./billing-cap.js");

module.exports = {
  ...gameExports,
  ...billingExports,
};
