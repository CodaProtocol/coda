const fs = require("fs");
const { exec } = require("child_process");
const { execPath } = require("./binaries");
const {
  PROOF_SUCCESS,
  PROOF_FAIL,
  CREDIT_SCORE,
  CREDIT_FAIL,
} = require("../constants");

/**
 * Generates the command that will be used to generate a SNAPP proof. This calls upon the SNAPP executable that is bundled
 * with the application. The `SNAPP_PUBLICKEY`, `RECEIVER_PUBLICKEY`, `FEE`, and `AMOUNT` bindings are pre-defined by the running
 * Snappnet network and should not be changed unless deploying to a new network.
 * @param   {String}   execPath    The path to the SNAPP proof generator executable
 * @param   {String}   ethAddress  The ETH address of the user
 * @param   {Number}   creditScore The web scraped credit score of the user
 * @returns {String}               Returns a string that's the command to feed to node exec() to generate a SNAPP proof
 */
const execSnappCommand = (execPath, ethAddress, creditScore) => {
  // Defined previously when setting up Snappnet
  const SNAPP_PUBLICKEY =
    "B62qpeeaJV6jm3FZL9hvApQ7CjbwiLL8TXbZBWZwJwSm3rqM7yUmRLC";
  const RECEIVER_PUBLICKEY =
    "B62qnW5wYBhf9zCKSpyp3Q9bhEBgx47aP3iLAVbWHi2diSHFNG6Nwtw";
  const FEE = "10000000";
  const AMOUNT = "10000000";

  return `
    ${execPath} prove --score ${creditScore}\
    --eth-address ${ethAddress}\
    --snapp-public-key ${SNAPP_PUBLICKEY}\
    --receiver-public-key ${RECEIVER_PUBLICKEY}\
    --fee ${FEE}\
    --amount ${AMOUNT}`;
};

/**
 *  Write the SNAPP output to a file destination specified by the user. The SNAPP proof executable can
 *  have unwanted output so we parse out anything before what we need (anything after `mutation`).
 * @param   {String}    output      The output of the SNAPP executable
 * @param   {String}    outputPath  The destination save directory specified by the user
 */
const writeOutputToFile = (output, outputPath) => {
  const proofOutput = output.substr(output.indexOf("mutation"));
  fs.writeFileSync(outputPath, proofOutput);
};

/**
 * Call on the SNAPP executable to generate a SNAPP proof. The execution will check for all
 * necessary fields to be present, otherwise it will return early and not generate any output.
 * If proof generation is successful, write the output to disk and show a success message to the user.
 * Otherwise, send a failure message to the user that output could not be generated.
 * @param  {Object}   mainWindow    The Electron `mainWindow` binding. Used to show messages to the user
 * @param  {String}   ethAddress    The ETH address of the user
 * @param  {Number}   creditScore   The credit score of the user
 */
const generateSnapp = async (mainWindow, ethAddress, creditScore) => {
  let outputPath = await mainWindow.webContents.executeJavaScript(
    'localStorage.getItem("output-path");',
    true
  );

  // Return if output or credit score are undefined
  if (!outputPath || !creditScore) {
    mainWindow.webContents.send(PROOF_FAIL);
    return;
  }

  // Return if credit score is too low
  if (creditScore < CREDIT_SCORE) {
    mainWindow.webContents.send(CREDIT_FAIL);
    return;
  }

  // Generate SNAPP proof output to save to disk
  exec(execSnappCommand(execPath, ethAddress, creditScore), (error, stdout) => {
    if (error) {
      /*
        Snapp executable returns an error code of 1 on Mac even if it's successful
        so we skip over that. Otherwise consider it an error.
      */
      const isMac = process.platform === "darwin" ? true : false;
      if (isMac && error.code === 1) {
        writeOutputToFile(stdout, outputPath);
        mainWindow.webContents.send(PROOF_SUCCESS);
      } else {
        mainWindow.webContents.send(PROOF_FAIL);
      }
    } else {
      writeOutputToFile(stdout, outputPath);
      mainWindow.webContents.send(PROOF_SUCCESS);
    }
  });
};

exports.generateSnapp = generateSnapp;
