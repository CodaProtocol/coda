// test_encodings.js -- print Rosetta encodings of a couple of public keys

var mina = require("../../../../_build/default/src/app/client_sdk/client_sdk.bc.js").minaSDK;

var pk1 = "B62qrcFstkpqXww1EkSGrqMCwCNho86kuqBd4FrAAUsPxNKdiPzAUsy";
var pk2 = "B62qkfHpLpELqpMK6ZvUTJ5wRqKDRF3UHyJ4Kv3FU79Sgs4qpBnx5RR";

var enc1 = mina.rawPublicKeyOfPublicKey(pk1)
var enc2 = mina.rawPublicKeyOfPublicKey(pk2)

console.log(enc1)
console.log(enc2)
