/**
 * This file is responsible for figuring out where the SNAPP executable lives depending
 * if it's been packaged or not so it can be called upon successfully to generate a SNAPP
 * proof. This tactic has been modified from https://stackoverflow.com/a/52991116/4160498
 *
 * By using Electron Bundler, we can target where the SNAPP executable will live before
 * bundling it. We specify different paths to look for Linux and macOS. If the application is not
 * bundled, we assume it lives in /resources/bin to use while in development.
 */

const path = require("path");
const { app } = require("electron");
const { isPackaged } = app;
const isMac = process.platform === "darwin" ? true : false;

const linuxPath = path.join(
  path.dirname(app.getAppPath()),
  "..",
  "resources",
  "Resources",
  "bin"
);

const macPath = path.join(
  path.dirname(app.getAppPath()),
  "..",
  "..",
  "Contents",
  "Resources",
  "Resources",
  "bin"
);

let binariesPath;
if (isPackaged) {
  binariesPath = isMac ? macPath : linuxPath;
} else {
  binariesPath = path.join(process.cwd(), "resources", "bin");
}

const execPath = path.resolve(path.join(binariesPath, "credit_score_demo.exe"));
exports.execPath = execPath;
