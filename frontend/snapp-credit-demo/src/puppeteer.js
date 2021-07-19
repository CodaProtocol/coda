const puppeteer = require("puppeteer");
const { LOGIN_CA, LOGIN_COM, COM_DOMAIN, CA_DOMAIN } = require("../constants");

/**
 * Log into the credit provider with the specified domain (.COM or .CA) and attempt
 * to web scrape the credit score on the page using puppeteer to control a headless chrome window.
 * We use puppeteer as there are security measures on the credit provider website and this
 * is the easiest way to get around them.
 * @param   {String}   email       The users specified credit provider account email
 * @param   {String}   password    The users specified credit provider account password
 * @param   {Object}   mainWindow  The Electron `mainWindow` binding. Used to show messages to the user
 * @param   {String}   domain      The specified domain of the acccount the user is logging into (.COM or .CA)
 * @returns {Promise}              Returns a Promise with the credit score if successful, otherwise an error message
 */
let scrape = ({ email, password, mainWindow, domain }) => {
  return new Promise(async (resolve, reject) => {
    let browser;
    try {
      browser = await puppeteer.launch();
      let creditScore = 0;
      if (domain === COM_DOMAIN) {
        mainWindow.webContents.send(LOGIN_COM);
        creditScore = await scrapeSpecifiedDomain({
          email,
          password,
          browser,
          url: COM_DOMAIN,
        });
      } else if (domain === CA_DOMAIN) {
        mainWindow.webContents.send(LOGIN_CA);
        creditScore = await scrapeSpecifiedDomain({
          email,
          password,
          browser,
          url: CA_DOMAIN,
        });
      }
      resolve(creditScore);
    } catch (err) {
      reject(err);
    } finally {
      await browser.close();
    }
  });
};

/**
 * Attempt to log in and web scrape the credit score of a puppeteer controlled browser.
 * The execution will wait until the initial content of the credit provider website is loaded and then
 * fill in the email and password fields. Then log the user in and wait for an HTML element of .score-dial
 * to render. The credit score is nested inside this element so we parse it out and return the value.
 * @param   {String}   email       The users specified credit provider account email
 * @param   {String}   password    The users specified credit provider account password
 * @param   {Object}   browser     A puppeteer `Browser` binding. Used to create a `Page` to interact with the web page
 * @param   {String}   url         The URL of the credit provider we are logging into
 * @returns {Promise}              Returns a Promise with the credit score if successful, otherwise an error message
 */
let scrapeSpecifiedDomain = ({ email, password, browser, url }) => {
  return new Promise(async (resolve, reject) => {
    let page;
    try {
      page = await browser.newPage();
      await page.goto(url, {
        waitUntil: "domcontentloaded",
      });

      // Fill in credential fields
      await page.waitForSelector("[type=submit]");
      await page.type("[type=email]", email);
      await page.type("[type=password]", password);

      // Wait for 750ms due to a timing issue with initiating a press right after filling in credential fields
      await page.waitForTimeout(750);
      await page.click("[type=submit]");

      await page.waitForSelector(".score-dial", { timeout: 20000 });
      const creditScore = await page.evaluate(() => {
        const creditContainer = document.querySelector(".score-dial");
        // Look through nested HTML elements to find credit score
        for (let i = 0; i < creditContainer.children.length; i++) {
          const child = creditContainer.children[i];
          // If we find a number between 0 and 850, we can assume it's the credit score
          if (child.textContent > 0 && child.textContent < 850) {
            return child.textContent;
          }
        }
        return 0;
      });
      return resolve(creditScore);
    } catch (err) {
      return reject(err);
    } finally {
      await page.close();
    }
  });
};

exports.scrape = scrape;
