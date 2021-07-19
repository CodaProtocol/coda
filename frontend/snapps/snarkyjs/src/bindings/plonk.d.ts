export type AsField<F> = F | number | string | boolean;

export class Field {
  constructor(x: AsField<Field>);

  neg(this: AsField<Field>): Field;
  inv(this: AsField<Field>): Field;

  add(this: AsField<Field>, y: AsField<Field>): Field;
  sub(this: AsField<Field>, y: AsField<Field>): Field;
  mul(this: AsField<Field>, y: AsField<Field>): Field;
  div(this: AsField<Field>, y: AsField<Field>): Field;

  square(this: AsField<Field>): Field;
  sqrt(this: AsField<Field>): Field;

  toString(this: AsField<Field>): string;

  sizeInFieldElements(): number;
  toFieldElements(this: AsField<Field>): Field[];

  assertEqual(this: AsField<Field>, y: AsField<Field>): void;
  assertBoolean(this: AsField<Field>): void;
  isZero(this: AsField<Field>): Bool;

  toBool(this: AsField<Field>): Bool;

  unpack(this: AsField<Field>): Bool[];

  equals(this: AsField<Field>, y: AsField<Field>): Bool;

  value(this: AsField<Field>): Field;

  /* Self members */
  static one: Field;
  static zero: Field;
  static random(): Field;

  static neg(x: AsField<Field>): Field;
  static inv(x: AsField<Field>): Field;

  static add(x: AsField<Field>, y: AsField<Field>): Field;
  static sub(x: AsField<Field>, y: AsField<Field>): Field;
  static mul(x: AsField<Field>, y: AsField<Field>): Field;
  static div(x: AsField<Field>, y: AsField<Field>): Field;

  static square(x: AsField<Field>): Field;
  static sqrt(x: AsField<Field>): Field;

  static toString(x: AsField<Field>): string;

  static sizeInFieldElements(): number;
  static toFieldElements(x: AsField<Field>): Field[];
  static ofFieldElements(fields: Field[]): Field;

  static assertEqual(x: AsField<Field>, y: AsField<Field>): Field;
  static assertBoolean(x: AsField<Field>): void;
  static isZero(x: AsField<Field>): Bool;

  static toBool(x: AsField<Field>): Bool;

  static pack(x: AsBool<Bool>[]): Field;
  static unpack(x: AsField<Field>): Bool[];

  static equals(x: AsField<Field>, y: AsField<Field>): Bool;
}

export type AsBool<B> = B | boolean;

export class Bool {
  constructor(x: AsBool<Bool>);

  toField(this: AsBool<Bool>): Field;

  not(this: AsBool<Bool>): Bool;
  and(this: AsBool<Bool>, y: AsBool<Bool>): Bool;
  or(this: AsBool<Bool>, y: AsBool<Bool>): Bool;

  assertEqual(this: AsBool<Bool>, y: AsBool<Bool>): void;

  equals(this: AsBool<Bool>, y: AsBool<Bool>): Bool;
  isTrue(this: AsBool<Bool>): Bool;
  isFalse(this: AsBool<Bool>): Bool;

  sizeInFieldElements(): number;
  toFieldElements(this: AsBool<Bool>): Field[];

  /* static members */
  static toField(x: AsBool<Bool>): Field;

  static Unsafe: {
    ofField(x: AsField<Field>): Bool;
  };

  static not(x: AsBool<Bool>): Bool;
  static and(x: AsBool<Bool>, y: AsBool<Bool>): Bool;
  static or(x: AsBool<Bool>, y: AsBool<Bool>): Bool;

  static assertEqual(x: AsBool<Bool>, y: AsBool<Bool>): void;

  static equals(x: AsBool<Bool>, y: AsBool<Bool>): Bool;
  static isTrue(x: AsBool<Bool>): Bool;
  static isFalse(x: AsBool<Bool>): Bool;

  static count(x: AsBool<Bool>[]): Field;

  static sizeInFieldElements(): number;
  static toFieldElements(x: AsBool<Bool>): Field[];
  static ofFieldElements(fields: Field[]): Bool;
}

export interface AsFieldElements<T> {
  toFieldElements(x: T): Field[];
  ofFieldElements(x: Field[]): T;
  sizeInFieldElements(): number;
}

export class Circuit {
  constructor();

  addConstraint(
    this: Circuit,
    kind: 'multiply',
    x: Field,
    y: Field,
    z: Field
  ): void;
  addConstraint(this: Circuit, kind: 'add', x: Field, y: Field, z: Field): void;
  addConstraint(
    this: Circuit,
    kind: 'equal',
    x: Field,
    y: Field,
    z: Field
  ): void;
  addConstraint(
    this: Circuit,
    kind: 'boolean',
    x: Field,
    y: Field,
    z: Field
  ): void;

  newVariable(): Field;
  newVariable(x: AsField<Field>): Field;
  newVariable(f: () => AsField<Field>): Field;

  newPublicVariable(): Field;
  newPublicVariable(x: AsField<Field>): Field;
  newPublicVariable(f: () => AsField<Field>): Field;

  setVariable(x: Field, value: AsField<Field>): void;
  setVariable(x: Field, f: () => AsField<Field>): void;

  witness<T>(
    ctor: { toFieldElements(x: T): Field[]; ofFieldElements(x: Field[]): T; sizeInFieldElements(): number },
    f: () => T
  ): T;
  witness<Value, Var>(
    valCtor: { toFieldElements(x: Value): Field[]; sizeInFieldElements(): number },
    varCtor: { ofFieldElements(x: Field[]): Var; sizeInFieldElements(): number },
    f: () => Value
  ): Var;

  array<T>(
    ctor: AsFieldElements<T>,
    length: number
  ): AsFieldElements<T[]>;

  assertEqual<T>(
    ctor: { toFieldElements(x: T): Field[] },
    x: T,
    y: T
  ): void;

  assertEqual<T extends { toFieldElements(this: T): Field[] }>(
    x: T,
    y: T
  ): void;

  equal<T>(
    ctor: { toFieldElements(x: T): Field[] },
    x: T,
    y: T
  ): Bool;

  equal<T extends { toFieldElements(this: T): Field[] }>(
    x: T,
    y: T
  ): Bool;

  if<T>(
    b: AsBool<Bool>,
    ctor: AsFieldElements<T>,
    x: T,
    y: T
  ): T;

  if<T extends AsFieldElements<T>>(
    b: AsBool<Bool>,
    x: T,
    y: T
  ): T;
}

/* TODO: Figure out types for these. */
export const ofFieldElements: (x: any[], y: any[]) => any[];
export const toFieldElements: (x: any[], y: any[]) => any[];
export const sizeInFieldElements: (x: any[]) => number;

export const NumberAsField: AsFieldElements<Number>;

export const array: <T>(x: AsFieldElements<T>, length: number) => AsFieldElements<T[]>;
