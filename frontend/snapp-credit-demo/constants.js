/**
 * This object holds all shared constants used in the application lifecycle.
 */

module.exports = Object.freeze({
  // Electron Events
  WEBSCRAPE: "web-scrape",
  LOGIN: "login",
  LOGIN_COM: "login-com",
  LOGIN_CA: "login-CA",
  VALID_LOGIN: "valid-login",
  INVALID_LOGIN: "invalid-login",
  PROOF_SUCCESS: "proof-success",
  PROOF_FAIL: "proof-fail",
  CREDIT_FAIL: "credit-fail",

  // Constants used by puppeteer
  CREDIT_SCORE: 700,
  COM_DOMAIN: "https://www.creditkarma.com/auth/logon",
  CA_DOMAIN: "https://www.creditkarma.ca/login",
});
