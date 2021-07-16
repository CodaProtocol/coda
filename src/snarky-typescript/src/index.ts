interface ToFieldElements<Field, Type> {
  toFieldElements(this: Type): Field[];
}

interface OfFieldElements<Field, Type> {
  ofFieldElements(elts: Field[]): Type;
}

interface AsFieldElements<Field, Type>
  extends ToFieldElements<Field, Type>,
    OfFieldElements<Field, Type> {}

/* NB: Do not combine this with other types using |, otherwise the error
 * message is an unhelpful 'cannot convert to string'.
 */
type FieldConvertible = number | string | boolean;

interface FieldElement<F> {
  neg(this: F): F;
  inv(this: F): F;
  add(this: F, x: FieldConvertible): F;
  add(this: F, x: F): F;
  add(this: F, x: FieldConvertible): F;
  add(this: F, x: F): F;
  sub(this: F, x: FieldConvertible): F;
  sub(this: F, x: F): F;
  mul(this: F, x: FieldConvertible): F;
  mul(this: F, x: F): F;
  div(this: F, x: FieldConvertible): F;
  div(this: F, x: F): F;
  toString(this: F): string;
  toBits(this: F): boolean[];
  toFieldElements(this: F): [F];
  ofFieldElements(x: F[]): F;
}

interface Field<F> {
  random(): F;
  zero: F;
  one: F;

  neg(x: FieldConvertible): F;
  neg(x: F): F;
  inv(x: FieldConvertible): F;
  inv(x: F): F;
  add(x: FieldConvertible, y: FieldConvertible): F;
  add(x: F, y: FieldConvertible): F;
  add(x: FieldConvertible, y: F): F;
  add(x: F, y: F): F;
  sub(x: FieldConvertible, y: FieldConvertible): F;
  sub(x: F, y: FieldConvertible): F;
  sub(x: FieldConvertible, y: F): F;
  sub(x: F, y: F): F;
  mul(x: FieldConvertible, y: FieldConvertible): F;
  mul(x: F, y: FieldConvertible): F;
  mul(x: FieldConvertible, y: F): F;
  mul(x: F, y: F): F;
  div(x: FieldConvertible, y: FieldConvertible): F;
  div(x: F, y: FieldConvertible): F;
  div(x: FieldConvertible, y: F): F;
  div(x: F, y: F): F;

  toString(x: FieldConvertible): string;
  toString(x: F): string;
  ofString(str: string): FieldElement<F>;

  toBits(x: FieldConvertible): boolean[];
  toBits(x: F): boolean[];
  ofBits(bits: boolean[]): F;

  toFieldElements(val: F): [F];
  ofFieldElements(val: F[]): F;

  new (x: FieldConvertible): F;
  new (x: F): F;
}

abstract class CircuitVar<F, Circuit> {
  abstract circuit(this: this): Circuit;

  abstract neg(this: this): this;
  abstract inv(this: this): this;
  abstract add(this: this, y: FieldConvertible): this;
  abstract add(this: this, y: this | F): this;
  abstract sub(this: this, y: FieldConvertible): this;
  abstract sub(this: this, y: this | F): this;
  abstract mul(this: this, y: FieldConvertible): this;
  abstract mul(this: this, y: this | F): this;
  abstract div(this: this, y: FieldConvertible): this;
  abstract div(this: this, y: this | F): this;
  abstract scale(this: this, scalar: FieldConvertible): this;
  abstract scale(this: this, scalar: F): this;

  abstract equals(this: this, y: FieldConvertible): BoolVar<F, Circuit, this>;
  abstract equals(this: this, y: this | F): BoolVar<F, Circuit, this>;
  abstract assertBoolean(this: this): BoolVar<F, Circuit, this>;
  abstract assertEquals(this: this, y: FieldConvertible): void;
  abstract assertEquals(this: this, y: this | F): void;

  abstract toBits(this: this): this[];
  abstract toFieldElements(this: this): [this];
  abstract ofFieldElements(x: this[]): this;

  abstract store(this: this, val: FieldConvertible): void;
  abstract store(this: this, val: F): void;
  abstract store(this: this, f: () => F): void;
  abstract store(this: this, f: () => FieldConvertible): void;
  abstract read(this: this): F;
}

interface CircuitVarCtor<F, Circuit, Var extends CircuitVar<F, Circuit>> {
  neg(x: FieldConvertible): Var;
  neg(x: Var | F): Var;

  inv(x: FieldConvertible): Var;
  inv(x: Var | F): Var;

  add(x: FieldConvertible, y: FieldConvertible): Var;
  add(x: FieldConvertible, y: Var | F): Var;
  add(x: Var | F, y: FieldConvertible): Var;
  add(x: Var | F, y: Var | F): Var;

  sub(x: FieldConvertible, y: FieldConvertible): Var;
  sub(x: FieldConvertible, y: F): Var;
  sub(x: FieldConvertible, y: Var): Var;
  sub(x: Var | F, y: FieldConvertible): Var;
  sub(x: Var | F, y: F): Var;
  sub(x: Var | F, y: Var): Var;

  mul(x: FieldConvertible, y: FieldConvertible): Var;
  mul(x: FieldConvertible, y: Var | F): Var;
  mul(x: Var | F, y: FieldConvertible): Var;
  mul(x: Var | F, y: Var | F): Var;

  div(x: FieldConvertible, y: FieldConvertible): Var;
  div(x: FieldConvertible, y: Var | F): Var;
  div(x: Var | F, y: FieldConvertible): Var;
  div(x: Var | F, y: Var | F): Var;

  scale(x: Var | F, scalar: FieldConvertible): Var;
  scale(x: Var | F, scalar: F): Var;

  equals(x: FieldConvertible, y: FieldConvertible): Var;
  equals(x: FieldConvertible, y: Var | F): Var;
  equals(x: Var | F, y: FieldConvertible): Var;
  equals(x: Var | F, y: Var | F): Var;

  assertBoolean(x: FieldConvertible): BoolVar<F, Circuit, Var>;
  assertBoolean(x: Var | F): BoolVar<F, Circuit, Var>;
  assertBoolean(x: Var): BoolVar<F, Circuit, Var>;

  assertEquals(x: FieldConvertible, y: FieldConvertible): void;
  assertEquals(x: FieldConvertible, y: Var | F): void;
  assertEquals(x: Var | F, y: FieldConvertible): void;
  assertEquals(x: Var | F, y: Var | F): void;

  toBits(x: FieldConvertible): BoolVar<F, Circuit, Var>[];
  toBits(x: Var | F): BoolVar<F, Circuit, Var>[];

  ofBits(x: (boolean | BoolVar<F, Circuit, Var>)[]): Var;

  toFieldElements(x: FieldConvertible): [Var];
  toFieldElements(x: Var | F): [Var];

  ofFieldElements(x: (FieldConvertible | F | Var)[]): Var;

  store(x: Var, val: FieldConvertible): void;
  store(x: Var, val: F): void;
  store(x: Var, f: () => F): void;
  store(x: Var, f: () => FieldConvertible): void;

  read(x: FieldConvertible): F;
  read(x: F): F;
  read(x: Var): F;

  new (): Var;
  new (options: {isPublic: boolean}): Var;

  new (x: FieldConvertible): Var;
  new (x: F): Var;
  new (x: Var): Var;
  new (f: () => FieldConvertible | F | Var): Var;

  new (x: FieldConvertible, options: {isPublic: boolean}): Var;
  new (x: F, options: {isPublic: boolean}): Var;
  new (x: Var, options: {isPublic: boolean}): Var;
  new (f: () => F, options: {isPublic: boolean}): Var;
  new (f: () => FieldConvertible, options: {isPublic: boolean}): Var;

  new (x: FieldConvertible, isPublic: boolean): Var;
  new (x: F, isPublic: boolean): Var;
  new (x: Var, isPublic: boolean): Var;
  new (f: () => F, isPublic: boolean): Var;
  new (f: () => FieldConvertible, isPublic: boolean): Var;
}

/* TODO: Fill this in with the default implementation */
abstract class BoolVar<F, Circuit, V extends CircuitVar<F, Circuit>> {
  abstract not(this: this): this;
  abstract and(this: this, y: boolean | this): this;
  abstract or(this: this, y: boolean | this): this;

  abstract equals(this: this, y: boolean | this): this;
  abstract assertEquals(this: this, y: boolean | this): void;
  abstract assertTrue(this: this): void;
  abstract assertFalse(this: this): void;

  abstract if_<Var extends AsFieldElements<V, Var>>(
    this: this,
    then_: Var,
    else_: Var
  ): Var;
  abstract if_<Var extends AsFieldElements<V, Var>>(
    this: this,
    args: {
      then_: Var;
      else_: Var;
    }
  ): Var;

  abstract toField(this: this): V;
  abstract toFieldElements(this: this): [V];
  abstract ofFieldElements(x: V[]): this;

  abstract read(this: this): boolean;
}

interface BoolVarCtor<
  F,
  Circuit,
  V extends CircuitVar<F, Circuit>,
  BVar extends BoolVar<F, Circuit, V>
> {
  not(x: BVar | boolean): BVar;
  and(x: BVar | boolean, y: BVar | boolean): BVar;
  or(x: BVar | boolean, y: BVar | boolean): BVar;

  countTrue(xs: (BVar | boolean)[]): V;
  countFalse(xs: (BVar | boolean)[]): V;

  allTrue(xs: (BVar | boolean)[]): BVar;
  allFalse(xs: (BVar | boolean)[]): BVar;
  anyTrue(xs: (BVar | boolean)[]): BVar;
  anyFalse(xs: (BVar | boolean)[]): BVar;

  equals(x: BVar | boolean, y: BVar | boolean): BVar;

  assertTrue(xs: BVar | boolean): BVar;
  assertFalse(xs: BVar | boolean): BVar;
  assertAllTrue(xs: (BVar | boolean)[]): BVar;
  assertAllFalse(xs: (BVar | boolean)[]): BVar;
  assertAnyTrue(xs: (BVar | boolean)[]): BVar;
  assertAnyFalse(xs: (BVar | boolean)[]): BVar;
  assertEquals(x: BVar | boolean, y: BVar | boolean): void;

  toField(x: BVar | boolean): V;

  ofField(x: FieldConvertible): BVar;
  ofField(x: F): BVar;
  ofField(x: V): BVar;

  toFieldElements(x: BVar | boolean): [V];

  ofFieldElements(x: (FieldConvertible | F | V)[]): BVar;

  read(x: BVar | boolean): boolean;

  Unsafe: {
    ofField(x: FieldConvertible): BVar;
    ofField(x: F): BVar;
    ofField(x: V): BVar;
  };

  new (x: BVar | boolean): BVar;
  new (f: () => boolean): BVar;
}

abstract class CircuitData<
  F,
  Circuit,
  V extends CircuitVar<F, Circuit>,
  BVar extends BoolVar<F, Circuit, V>,
  Type extends AsFieldElements<F, Type>
> {
  abstract toFieldElements(this: this): V[];
  abstract ofFieldElements(
    fieldElements: V[]
  ): CircuitData<F, Circuit, V, BVar, Type>;

  abstract equals(this: this, that: this | Type): BVar;

  assertEquals(this: this, y: this): void {
    this.equals(y).assertTrue();
  }

  abstract if_(
    this: this,
    cond: BVar | boolean,
    that: this | Type
  ): CircuitData<F, Circuit, V, BVar, Type>;

  abstract read(this: this): Type;
}

abstract class Circuit<F extends FieldElement<F>, ConstraintKind, Gadget, Result> {
  abstract Field: Field<F>;
  abstract Var: CircuitVarCtor<F, this, CircuitVar<F, this>>;
  abstract Bool: BoolVarCtor<
    F,
    this,
    CircuitVar<F, this>,
    BoolVar<F, this, CircuitVar<F, this>>
  >;

  abstract finish(this: this): Result;

  /* Run some code while proving; skipped during constraint generation. */
  abstract run(this: this, f: () => void): void;

  abstract addConstraint(kind: ConstraintKind, ...data: any): void;

  abstract runGadget(gadget: Gadget): void;

  abstract constant<Type extends AsFieldElements<F, Type>>(
    this: this,
    x: Type
  ): CircuitData<
    F,
    this,
    CircuitVar<F, this>,
    BoolVar<F, this, CircuitVar<F, this>>,
    Type
  >;

  abstract compute<Type extends AsFieldElements<F, Type>>(
    this: this,
    x: Type
  ): CircuitData<
    F,
    this,
    CircuitVar<F, this>,
    BoolVar<F, this, CircuitVar<F, this>>,
    Type
  >;
  abstract compute<Type extends AsFieldElements<F, Type>>(
    this: this,
    f: () => Type
  ): CircuitData<
    F,
    this,
    CircuitVar<F, this>,
    BoolVar<F, this, CircuitVar<F, this>>,
    Type
  >;

  abstract if_<Var extends AsFieldElements<CircuitVar<F, this>, Var>>(
    this: this,
    cond: BoolVar<F, this, CircuitVar<F, this>> | boolean,
    then_: Var,
    else_: Var
  ): Var;
  abstract if_<Var extends AsFieldElements<CircuitVar<F, this>, Var>>(
    this: this,
    cond: BoolVar<F, this, CircuitVar<F, this>> | boolean,
    args: {
      then_: Var;
      else_: Var;
    }
  ): Var;

  abstract equals<Var extends AsFieldElements<CircuitVar<F, this>, Var>>(
    this: this,
    x: Var,
    y: Var
  ): BoolVar<F, this, CircuitVar<F, this>>;

  abstract assertEqual<Var extends AsFieldElements<CircuitVar<F, this>, Var>>(
    this: this,
    x: Var,
    y: Var
  ): void;
}

type StandardConstraints = 'multiply' | 'equal' | 'boolean';

function test<
  F extends FieldElement<F>,
  C extends Circuit<F, StandardConstraints, undefined, F[]>
>(Circuit: C): F[] {
  /* Field interface test. */
  const f1 = Circuit.Field.one;
  const f2 = new Circuit.Field(1);
  const f3 = new Circuit.Field('1');
  const f4 = f1.mul(2);
  const f5 = f4.mul(f2);
  const f6 = Circuit.Field.mul(f2, f3).add(f5);

  /* Circuit interface test. */
  const knownPublicInput = new Circuit.Var(f6, {isPublic: true});
  const unknownPublicInput = new Circuit.Var({isPublic: true});
  const knownPrivateInput = new Circuit.Var(0);
  const unknownPrivateInput = new Circuit.Var();

  unknownPublicInput.store(() => {
    const val = knownPublicInput.read().mul(knownPrivateInput.read());
    console.log(val.toString());
    return val;
  });

  Circuit.run(() => {
    console.log(unknownPublicInput.read());
  });

  unknownPrivateInput.store(15);

  const res = knownPublicInput
    .inv()
    .mul(unknownPublicInput)
    .add(knownPrivateInput)
    .sub(unknownPrivateInput);

  Circuit.addConstraint('equal', res, res);

  const eq = res.equals(unknownPrivateInput);

  /* More complicated types. */

  class ECPoint {
    x: F;
    y: F;
    constructor(x: F, y: F) {
      this.x = x;
      this.y = y;
    }
    toFieldElements(this: this): F[] {
      return [this.x, this.y];
    }
    ofFieldElements([x, y]: F[]): ECPoint {
      return new ECPoint(x, y);
    }
  }

  /* This ugly extends will be replaced with a proper CircuitData class for
   * concrete proof system classes. */
  class VarECPoint extends CircuitData<
    F,
    C,
    CircuitVar<F, C>,
    BoolVar<F, C, CircuitVar<F, C>>,
    ECPoint
  > {
    x: CircuitVar<F, C>;
    y: CircuitVar<F, C>;
    constructor(x: CircuitVar<F, C>, y: CircuitVar<F, C>) {
      super();
      this.x = x;
      this.y = y;
    }

    /* TODO: These should be implemented in the abstract class with a generic
     * implementation over toFieldElements.. */

    toFieldElements(this: this): CircuitVar<F, C>[] {
      return [this.x, this.y];
    }
    ofFieldElements([x, y]: CircuitVar<F, C>[]): VarECPoint {
      return new VarECPoint(x, y);
    }
    equals(this: this, that: this | ECPoint): BoolVar<F, C, CircuitVar<F, C>> {
      return this.x.equals(that.x).and(this.y.equals(that.y));
    }
    assertEquals(this: this, that: this): void {
      Circuit.Bool.assertTrue(this.equals(that));
    }

    if_(
      this: this,
      cond: BoolVar<F, C, CircuitVar<F, C>> | boolean,
      that: this | ECPoint
    ): VarECPoint {
      let b: BoolVar<F, C, CircuitVar<F, C>>;
      if (cond instanceof BoolVar) {
        b = cond;
      } else {
        b = new Circuit.Bool(cond);
      }
      let x, y;
      if (that instanceof ECPoint) {
        x = b.if_(this.x, new Circuit.Var(that.x));
        y = b.if_(this.y, new Circuit.Var(that.y));
      } else {
        x = b.if_(this.x, that.x);
        y = b.if_(this.y, that.y);
      }
      return new VarECPoint(x, y);
    }

    read(this: this) {
      return new ECPoint(this.x.read(), this.y.read());
    }
  }

  const pt1 = new ECPoint(f6, new Circuit.Field(1));
  const pt2 = new VarECPoint(res, eq.toField());
  const pt3 = Circuit.if_(eq, Circuit.constant(pt1), pt2);
  const pt4 = Circuit.if_(eq, Circuit.compute(pt1), pt2);
  const pt5 = Circuit.if_(
    eq,
    Circuit.compute(() => {
      return pt1;
    }),
    pt2
  );

  /* Return the witness from the circuit. */
  return Circuit.finish();
}
