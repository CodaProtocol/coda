const path = require("path");
const { ipcRenderer, remote } = require("electron");
const { dialog } = remote;
const {
  LOGIN,
  LOGIN_COM,
  LOGIN_CA,
  VALID_LOGIN,
  INVALID_LOGIN,
  PROOF_SUCCESS,
  PROOF_FAIL,
  CREDIT_FAIL,
  COM_DOMAIN,
  CA_DOMAIN,
} = require("../constants");

/**
 * Toggle visibility of the progress spinner.
 * @param   {Boolean}  toggle  Show if true, hide if false
 */
const toggleProgressSpinnerVisibility = (toggle) => {
  toggle
    ? document.getElementById("progress-status-spinner").classList.add("active")
    : document
        .getElementById("progress-status-spinner")
        .classList.remove("active");
};

/**
 * Set the text of the progress status field.
 * @param   {String}  status  The string message to show to the user
 */
const setProgressStatusText = (status) => {
  document.getElementById("progress-status-text").innerText = status;
};

/**
 * Open an Electron dialog window to let the user select the destination save directory for the SNAPP proof.
 * @returns   {Promise}  A Promise with the destination save directory if successful, otherwise an error message
 */
const chooseFolder = () => {
  return new Promise((resolve) => {
    dialog
      .showOpenDialog(remote.getCurrentWindow(), {
        properties: ["openDirectory"],
      })
      .then((result) => {
        if (result.canceled === false) {
          resolve(result.filePaths[0]);
        }
      })
      .catch((err) => {
        console.error(err);
      });
  });
};

/**
 * Attach a click event listener on the `BROWSE` button to open a dialog window and then store the destination
 * save directory to local storage for later use.
 */
document
  .getElementById("output-path-input")
  .addEventListener("click", async (e) => {
    e.preventDefault();
    const folderLocation = await chooseFolder();
    const outputFilePath = path.join(folderLocation, "snapp-credit-score");
    document.getElementById("output-path-display").value = outputFilePath;
    localStorage.setItem("output-path", outputFilePath);
  });

/**
 * Attach a submit event listener on the SNAPP form UI. On form submittal, the execution will check
 * if the `loading` binding is set to true and exit early if it's the case. Otherwise, we parse the
 * form fields and send an Electron event to the main process to initiate the web scrape + SNAPP
 * proof generation. If the fields are not all present, we do not allow the user to continue.
 */
document.getElementById("snapp-form").addEventListener("submit", (e) => {
  e.preventDefault();
  if (localStorage.getItem("loading") === "true") {
    return;
  }

  const ethAddress = document.getElementById("eth-address")?.value;
  const email = document.getElementById("email")?.value;
  const password = document.getElementById("password")?.value;
  const outputPath = document.getElementById("output-path-display")?.value;
  const usaRadioBtn = document.getElementById("radio-usa")?.checked;
  const domain = usaRadioBtn ? COM_DOMAIN : CA_DOMAIN;

  localStorage.setItem("loading", true);

  // Kick off Electron event if form fields are present, otherwise issue an error message to the user
  if (ethAddress && email && password && outputPath) {
    ipcRenderer.send(LOGIN, {
      ethAddress,
      email,
      password,
      domain,
    });
  } else {
    setProgressStatusText(
      "All fields are required, please fill them before continuing"
    );
    localStorage.setItem("loading", false);
  }
});

ipcRenderer.on(VALID_LOGIN, () => {
  toggleProgressSpinnerVisibility(true);
  setProgressStatusText(
    "Login successful. Attempting to generate SNAPP proof, this can take up to a few minutes. Please be patient..."
  );
});

ipcRenderer.on(INVALID_LOGIN, () => {
  toggleProgressSpinnerVisibility(false);
  setProgressStatusText(
    "Login unsuccessful. Please check your credentials or restart the application and try again."
  );
  localStorage.setItem("loading", false);
});

ipcRenderer.on(LOGIN_COM, () => {
  toggleProgressSpinnerVisibility(true);
  setProgressStatusText(
    `Attempting to log into creditkarma.com, please do not close the application...`
  );
});

ipcRenderer.on(LOGIN_CA, () => {
  toggleProgressSpinnerVisibility(true);
  setProgressStatusText(
    `Attempting to log into creditkarma.ca, please do not close the application...`
  );
});

ipcRenderer.on(PROOF_SUCCESS, () => {
  toggleProgressSpinnerVisibility(false);
  const outputPath = localStorage.getItem("output-path");
  setProgressStatusText(
    `Credit score is above 700, SNAPP proof succesfully saved to: ${outputPath}`
  );
  localStorage.setItem("loading", false);
});

ipcRenderer.on(PROOF_FAIL, () => {
  toggleProgressSpinnerVisibility(false);
  const outputPath = localStorage.getItem("output-path");
  setProgressStatusText(`Failed to generate proof to: ${outputPath}`);
  localStorage.setItem("loading", false);
});

ipcRenderer.on(CREDIT_FAIL, () => {
  toggleProgressSpinnerVisibility(false);
  const outputPath = localStorage.getItem("output-path");
  setProgressStatusText(
    `Credit score is less than 700, cannot produce SNAPP proof to: ${outputPath}`
  );
  localStorage.setItem("loading", false);
});
