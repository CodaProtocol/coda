import { Field, Circuit } from './bindings/plonk';
import { Snapp } from './mina';

exports.Field = Field;
exports.Circuit = Circuit;
exports.Snapp = Snapp;

// make sure examples compile
import { circuit as matrix_mul_circuit } from './examples/matrix_mul';
exports.matrix_mul_circuit = matrix_mul_circuit;

import { main as schnorr_sign } from './examples/schnorr_sign';
exports.schnorr_sign = schnorr_sign;

// for testing tests
export const five : number = 5;
