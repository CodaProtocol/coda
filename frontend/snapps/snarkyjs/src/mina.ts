// This is for an account where any of a list of public keys can update the state

import { prop, CircuitValue } from './circuit_value';
import { Field, Bool } from './bindings/plonk';

// TODO. Also, don't make user ever talk about distinction b/w compressed and non-compressed keys
type PublicKey = void;

class SetOrKeep<_A> extends CircuitValue {
}

export class OrIgnore<A> extends CircuitValue {
  @prop value: A;
  @prop shouldIgnore: Field;

  assertEqual(x: A) {
    (this.shouldIgnore as any).assertEqual(false);
    (this.shouldIgnore as any).fillValue(() => false);
    (this.value as any).assertEqual(x);
  }

  ignore() {
    (this.shouldIgnore as any).assertEqual(true);
    (this.shouldIgnore as any).fillValue(() => true);
  }

  constructor(value: A, ignore: Field) {
    super();
    this.value = value;
    this.shouldIgnore = ignore;
  }
}

class Signed<A> extends CircuitValue {
  @prop sign: Bool;
  @prop magnitude: A;

  constructor(sign: Bool, magnitude: A) {
    super();
    this.sign = sign;
    this.magnitude = magnitude;
  }
}

class Delta<A> extends CircuitValue {
  @prop value: Signed<A>;

  constructor(value: Signed<A>) {
    super();
    this.value = value;
  }

  addInPlace(this: this, x: A) {
    const prev = this.value;
    this.value = (prev as any).add(x);
  }

  subInPlace(this: this, x: A) {
    const prev = this.value;
    this.value = (prev as any).sub(x);
  }
}

class TokenId extends CircuitValue { };

class Predicate extends CircuitValue {
};

class Party extends CircuitValue {
  @prop publicKey: PublicKey;
  @prop state: Array<SetOrKeep<Field>>;
  @prop delegate: SetOrKeep<PublicKey>;
  @prop verificationKey: SetOrKeep<VerificationKey>;
  @prop permissions: SetOrKeep<Permissions>;
  @prop tokenId: TokenId;
  @prop balance: Delta<Amount>;
  @prop predicate: Predicate;

  constructor(
    publicKey: PublicKey,
    state: Array<SetOrKeep<Field>>,
    delegate: SetOrKeep<PublicKey>,
    verificationKey: SetOrKeep<VerificationKey>,
    permissions: SetOrKeep<Permissions>,
    tokenId: TokenId,
    balance: Delta<Amount>,
    predicate: Predicate
  ) {
    super();
    this.publicKey = publicKey;
    this.state = state;
    this.delegate = delegate;
    this.verificationKey = verificationKey;
    this.permissions = permissions;
    this.tokenId = tokenId;
    this.balance = balance;
    this.predicate = predicate;
  }
}

class Parties {
  get(this: this, _index: number): Party {
    throw 'unimplemented';
  }
}

class Transaction {
  parties: Parties = undefined as any;

  commitment(): Field {
    throw 'unimplemented';
  }

  self(): Party {
    throw 'unimplemented';
  }
}

export class UInt32 extends CircuitValue {
  sub(this: UInt32, _other: UInt32): UInt32 {
    throw 'unimplemented';
  }
}

export type Amount = UInt32;
export type Nonce = UInt32;

export class MerkleProof extends CircuitValue { }

export class MerkleCollection<T extends CircuitValue> {
  get(_p: MerkleProof): T {
    throw 'unimplemented';
  }
  

  constructor(xs: () => T[]) {
    console.log(xs);
  }
}

export enum Permission {
  NoAuthRequired,
  Signature,
  Proof,
}

export type Permissions = {
  receive: Permission;
  send: Permission;
  setDelegate: Permission;
  modifyState: Permission;
};

export class StateSlot extends CircuitValue {
  @prop isSet: Field;
  @prop value: Field;

  set(this: this, x: Field) {
    (this.isSet as any).fillValue(true);
    (this.value as any).fillValue(() => (x as any)._value());
    (this.value as any).assertEqual(x);
  }

  constructor(isSet: Field, value: Field) {
    super();
    this.isSet = isSet;
    this.value = value;
  }
}

export class VerificationKey extends CircuitValue {
}

export abstract class Snapp {
  state: StateSlot[] = [];

  permissions: Permissions = {
    receive: Permission.Proof,
    send: Permission.Proof,
    setDelegate: Permission.Proof,
    modifyState: Permission.Proof,
  };
  
  self(): VerificationKey {
    throw 'unimplemented';
  }

  transaction(): Transaction {
    throw 'unimplemented';
  }
}

export function method(this: any, target: any, key: string) {
  // TODO: init method is special
  console.log(this, target, key);
}
