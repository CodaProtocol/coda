const snarkette = require('./snarkette_bindings.bc');
const plonkCircuit = require('./plonk_circuit');

export const Fields = {
  PastaFp: plonkCircuit.mkField(snarkette.Pasta.fp),
  PastaFq: plonkCircuit.mkField(snarkette.Pasta.fp),
};

export const Circuit = plonkCircuit.mkCircuit(Fields.PastaFp);

export const Field = Fields.PastaFp;
export const Bool = Fields.PastaFp.Bool;
export const ofFieldElements = function (xs, fields) {
  var start = 0;
  var end;
  var res = [];
  for (var i = 0, l = xs.length; i < l; i++) {
    end = start + xs[i].sizeInFieldElements();
    res.push(xs[i].ofFieldElements(fields.slice(start, end)));
  }
  return res;
}
export const toFieldElements = function (xs, data) {
  var res = [];
  for (var i = 0, l = xs.length; i < l; i++) {
    res.push(xs[i].toFieldElements(data[i]));
  }
  return res;
}
export const sizeInFieldElements = function (xs) {
  var sum = 0;
  for (var i = 0, l = xs.length; i < l; i++) {
    sum += xs[i].sizeInFieldElements();
  }
  return sum;
}
export const NumberAsField = {
  toFieldElements: function () { throw new Error("TODO"); },
  ofFieldElements: function () { throw new Error("TODO"); },
  sizeInFieldElements: function () { throw new Error("TODO"); }
}
export const array = Circuit.prototype.array;
