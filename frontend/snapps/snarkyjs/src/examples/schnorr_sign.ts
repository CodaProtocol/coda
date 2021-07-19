import { Circuit, Field, Bool } from '../bindings/plonk';
import { poseidon } from '../bindings/poseidon';
import { Group, Scalar } from '../bindings/group';
import { prop, CircuitValue } from '../circuit_value';

class Signature extends CircuitValue {
  @prop r: Field;
  @prop s: Scalar;

  constructor(r: Field, s: Scalar) {
    super();
    this.r = r;
    this.s = s;
  }

  verify(this: this, pubKey: Group, msg: Field[]): Bool {
    let e = poseidon(msg).unpack();
    let r = pubKey.scale(e).neg().add(Group.generator()).scale(this.s);
    return Bool.and(
      r.x.equals(this.r),
      r.y.unpack()[0].equals(false)
    )
  }
};

class Witness extends CircuitValue {
  @prop signature: Signature;
  @prop acc: Group;
  @prop r: Scalar;

  constructor(sig: Signature, acc: Group, r: Scalar) {
    super();
    this.signature = sig;
    this.acc = acc;
    this.r = r;
  }
};

// Public input:
//  [newAcc: curve_point]
// Prove:
// I know [prevAcc: curve_point] and [signature : Signature] such that
// the signature verifies against Brave's public key and [newAcc] is
// a re-randomization of [prevAcc]

export function main(c: Circuit, w: Witness, newAcc: Group) {
  let H = new Group({ x: -1, y: 2 });
  let r: Scalar = c.witness(Scalar, () => w.r);
  let mask = H.scale(r);
  let prevAcc: Group = c.witness(Group, () => w.acc);
  let pubKey = Group.ofString("..."); // some literal group element
  let signature = c.witness<Signature>(Signature, () => w.signature);
  signature.verify(pubKey, [prevAcc.x, prevAcc.y, signature.r]).assertEqual(true);
  prevAcc.add(mask).assertEqual(newAcc);
}

