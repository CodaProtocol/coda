let mkField = function (baseField) {
    let toFieldPrimitive = function (x) {
        switch (typeof x) {
            case "number":
                return baseField.ofInt(x);
            case "string":
                return baseField.ofString(x);
            case "boolean":
                if (x) {
                    return baseField.ofInt(1);
                } else {
                    return baseField.ofInt(0);
                }
            default:
                return x;
        }
    };
    var Field;
    Field = function () {
        if (arguments.length == 1) {
            let x = arguments[0];
            if (x instanceof Field) {
                /* Copy fields */
                this._constant = x._constant;
                this._circuit = x._circuit;
                this._variable = x._variable;
                this._scale = x._scale;
            } else {
                this._constant = toFieldPrimitive(arguments[0]);
            }
        } else if (arguments.length == 2) {
            this._circuit = arguments[0];
            this._variable = arguments[1];
        }
    };
    Field._baseField = baseField;
    let toField = function (x) {
        if (x instanceof Field) {
            return x;
        } else {
            return new Field(x);
        }
    };
    let chooseCircuit = function (x, y) {
        if (x._circuit) {
            if (y._circuit && x._circuit !== y._circuit) {
                throw new Error("Cannot combine variables from different circuits");
            } else {
                return x._circuit;
            }
        } else {
            return y._circuit;
        }
    };
    Field.prototype._value = function () {
        if (this._circuit) {
            return this._circuit._read(this);
        } else {
            return this._constant;
        }
    };
    let negOne = baseField.neg(baseField.one);

    /* Self properties */
    Field.one = new Field(baseField.one);
    Field.zero = new Field(baseField.zero);
    Field.random = function () {
        return new Field(baseField.random());
    }

    /* Self methods */
    Field.neg = function (x) {
        var x = toField(x);
        if (x._circuit) {
            let res = new Field(x._circuit, x._variable);
            let scale =
                x._scale ? baseField.mul(x._scale, negOne) : negOne;
            let constant =
                x._constant ? baseField.mul(x._constant, negOne) : negOne;
            res._scale = scale;
            res._constant = constant;
            return res;
        } else {
            return new Field(baseField.mul(x._constant, negOne));
        }
    };
    Field.inv = function (x) {
        var x = toField(x);
        if (x._circuit) {
            let res = x._circuit.newVariable(() => {
                return new Field(baseField.inv(x._value()));
            });
            x._circuit.addConstraint("multiply", x, res, Field.one);
            return res;
        } else {
            return new Field(baseField.inv(toField(x)._value()));
        }
    };
    Field.add = function (x, y) {
        var x = toField(x);
        var y = toField(y);
        if (y._circuit && !x._circuit) {
            /* Swap order so that the first argument has the circuit. */
            return Field.add(y, x);
        } else if (x._circuit) {
            if (y._circuit) {
                if (x._circuit !== y._circuit) {
                    throw new Error("Cannot add variables from different circuits");
                }
                /* Adding 2 variables. */
                let res = x._circuit.newVariable(() => {
                    return new Field(baseField.add(x._value(), y._value()));
                });
                x._circuit.addConstraint("add", x, y, res);
                return res;
            } else {
                /* Adding a constant to a variable. */
                let res = new Field(x._circuit, x._variable);
                res._scale = x._scale;
                res._constant =
                    x._constant ? baseField.add(x._constant, y._value())
                        : y._value();
                return res;
            }
        } else {
            return new Field(baseField.add(x._value(), y._value()));
        }
    };
    Field.sub = function (x, y) {
        var x = toField(x);
        var y = toField(y);
        return Field.add(x, Field.neg(y));
    };
    Field.mul = function (x, y) {
        var x = toField(x);
        var y = toField(y);
        if (y._circuit && !x._circuit) {
            /* Swap order so that the first argument has the circuit. */
            return Field.mul(y, x);
        } else if (x._circuit) {
            if (y._circuit) {
                if (x._circuit !== y._circuit) {
                    throw new Error("Cannot multiply variables from different circuits");
                }
                /* Adding 2 variables. */
                let res = x._circuit.newVariable(() => {
                    return new Field(baseField.mul(x._value(), y._value()));
                });
                x._circuit.addConstraint("multiply", x, y, res);
                return res;
            } else {
                /* Scaling a variable by a constant. */
                let res = new Field(x._circuit, x._variable);
                res._scale =
                    x._scale ? baseField.mul(x._scale, y._value())
                        : y._value();
                ;
                res._constant =
                    x._constant ? baseField.mul(x._constant, y._value())
                        : undefined;
                return res;
            }
        } else {
            return new Field(baseField.mul(x._value(), y._value()));
        }
    };
    Field.div = function (x, y) {
        var x = toField(x);
        var y = toField(y);
        if (x._circuit && y._circuit) {
            if (x._circuit !== y._circuit) {
                throw new Error("Cannot multiply variables from different circuits");
            }
            /* Adding 2 variables. */
            let res = x._circuit.newVariable(() => {
                return new Field(baseField.div(x._value(), y._value()));
            });
            x._circuit.addConstraint("multiply", res, y, x);
            return res;
        } else {
            return Field.mul(x, Field.inv(y));
        }
    };
    Field.square = function (x) {
        var x = toField(x);
        return Field.mul(x, x);
    };
    Field.sqrt = function (x) {
        var x = toField(x);
        if (x._circuit) {
            let res = x._circuit.newVariable(() => {
                return new Field(baseField.sqrt(x._value()));
            });
            x._circuit.addConstraint("multiply", res, res, x);
            return res;
        } else {
            return new Field(baseField.sqrt(x._value()));
        }
    };
    Field.toString = function (x) {
        if (x._circuit) {
            return "[CircuitVariable]"
        } else {
            return (baseField.toString(x._value()));
        }
    }

    Field.sizeInFieldElements = function () {
        return 1;
    }
    Field.toFieldElements = function (x) {
        return [x];
    }
    Field.ofFieldElements = function (fields) {
        if (fields.length != Field.sizeInFieldElements()) {
            throw new Error("Field.ofFieldElements expected exactly 1 field element");
        }
        return fields[0];
    }

    /* Prototype methods */
    Field.prototype.neg = function () { return Field.neg(this); };
    Field.prototype.inv = function () { return Field.inv(this); };
    Field.prototype.add = function (y) { return Field.add(this, y); };
    Field.prototype.sub = function (y) { return Field.sub(this, y); };
    Field.prototype.mul = function (y) { return Field.mul(this, y); };
    Field.prototype.div = function (y) { return Field.div(this, y); };
    Field.prototype.square = function () { return Field.square(this); };
    Field.prototype.sqrt = function () { return Field.sqrt(this); };
    Field.prototype.value = function () { return new Field(this._value()); };
    Field.prototype.toString = function () { return Field.toString(this); };
    Field.prototype.sizeInFieldElements = function () { return Field.sizeInFieldElements(this); };
    Field.prototype.toFieldElements = function () { return Field.toFieldElements(this); };

    var allowCreateBool = false;
    var Bool;
    Bool = function (x) {
        if (allowCreateBool && x === undefined) {
            /* Do nothing, will be initialized elsewhere */
        } else if (typeof x === "boolean") {
            this._field = new Field(x);
        } else if (x instanceof Bool) {
            /* Copy fields */
            this._field = x._field;
        } else {
            throw new Error("Attempted to pass a non-boolean to the Bool constructor");
        }
    };
    Field.Bool = Bool;

    let toBool = function (x) {
        if (typeof x === "boolean") {
            return new Bool(x);
        } else {
            return x;
        }
    }

    Bool.toField = function (x) { return this._field; };
    Bool.Unsafe = {
        ofField: function (x) {
            allowCreateBool = true;
            var res = new Bool();
            allowCreateBool = false;
            res._field = x;
            return res;
        }
    };
    Bool.sizeInFieldElements = function () {
        return 1;
    }
    Bool.toFieldElements = function (x) {
        return [Bool.toField(x)];
    };
    Bool.ofFieldElements = function (fields) {
        if (fields.length != Bool.sizeInFieldElements()) {
            throw new Error("Bool.ofFieldElements expected exactly 1 field element");
        }
        return Bool.Unsafe.ofField(fields[0]);
    }

    Bool.not = function (x) {
        var x = toBool(x);
        return Bool.Unsafe.ofField(x._field.neg().add(1));
    };
    Bool.and = function (x, y) {
        var x = toBool(x);
        var y = toBool(y);
        return Bool.Unsafe.ofField(x._field.mul(y._field));
    };
    Bool.or = function (x, y) {
        var x = toBool(x);
        var y = toBool(y);
        return Bool.not(Bool.and(Bool.not(x), Bool.not(y)));
    };

    Bool.prototype.toField = Bool.toField;
    Bool.prototype.sizeInFieldElements = Bool.sizeInFieldElements;
    Bool.prototype.toFieldElements = Bool.toFieldElements;
    Bool.prototype.not = Bool.not;
    Bool.prototype.and = Bool.and;
    Bool.prototype.or = Bool.or;

    Field.assertEqual = function (x, y) {
        var x = toField(x);
        var y = toField(y);
        let circuit = x._circuit ? x._circuit : y._circuit;
        if (circuit) {
            if (y._circuit && circuit !== y._circuit) {
                throw new Error("Cannot combine variables from different circuits");
            }
            circuit.addConstraint("equal", x, y);
        } else if (baseField.equal(x._value(), y._value())) {
            // Do nothing.
        } else {
            throw new Error("Constant field elements are not equal");
        }
    };
    Bool.assertEqual = function (x, y) {
        var x = toBool(x);
        var y = toBool(y);
        Field.assertEqual(x._field, y._field);
    };

    Field.assertBoolean = function (x) {
        var x = toField(x);
        if (x._circuit) {
            x._circuit.addConstraint("boolean", x);
        } else {
            let x_ = x._value();
            if (!baseField.equal(x_, baseField.zero) && !baseField.equal(x_, baseField.one)) {
                throw new Error("Non-boolean constant field element");
            }
        }
    };
    Field.toBool = function (x) {
        var x = toField(x);
        Field.assertBoolean(x);
        return Bool.Unsafe.ofField(x);
    };

    Field.pack = function () {
        throw new Error("TODO");
    };
    Field.unpack = function () {
        throw new Error("TODO");
    };

    Field.isZero = function (x) {
        var x = toField(x);
        if (x._circuit) {
            let xInv = x._circuit.newVariable(() => {
                let x_ = x._value();
                if (baseField.equal(x_, baseField.zero)) {
                    return new Field(baseField.inv(x_));
                } else {
                    return Field.zero;
                }
            });
            let res = x._circuit.newVariable(() => {
                if (baseField.equal(x._value(), baseField.zero)) {
                    return new Field(0);
                } else {
                    return new Field(1);
                }
            });
            x._circuit.addConstraint("multiply", xInv, x, Field.sub(1, res));
            x._circuit.addConstraint("multiply", res, x, Field.zero);
            return Bool.Unsafe.ofField(res);
        } else {
            return new Bool(baseField.equal(x._constant, baseField.zero));
        }
    };

    Field.equals = function (x, y) {
        var x = toField(x);
        var y = toField(y);
        return Field.isZero(Field.sub(x, y));
    };
    Bool.equals = function (x, y) {
        var x = toBool(x);
        var y = toBool(y);
        return Field.equals(x._field, y._field);
    };
    Bool.isTrue = function (x) { return Bool.equals(x, true); };
    Bool.isFalse = function (x) { return Bool.equals(x, false); };
    Bool.count = function (xs) {
        let res = Field.zero;
        for (var i = 0, l = xs.length; i < l; i++) {
            res = Field.add(res, Field.toBool(xs));
        }
        return res;
    }

    Field.prototype.assertEqual = function (y) {
        return Field.assertEqual(this, y);
    };
    Field.prototype.isZero = function () {
        return Field.isZero(this);
    };
    Field.prototype.equals = function (y) {
        return Field.equals(this, y);
    };
    Field.prototype.assertBoolean = function () {
        return Field.assertBoolean(this);
    };
    Field.prototype.toBool = function () { return Field.toBool(this); };
    Field.prototype.unpack = function () { return Field.unpack(this); };

    Bool.prototype.assertEqual = function (y) {
        return Bool.assertEqual(this, y);
    };
    Bool.prototype.equals = function (y) {
        return Bool.equals(this, y);
    }
    Bool.prototype.isTrue = function () { return Bool.isTrue(this); }
    Bool.prototype.isFalse = function () { return Bool.isFalse(this); }

    return Field;
};

let mkCircuit = function (Field) {
    let Circuit = function () {
        this._nextVariable = 0;
        this._nextPublicVariable = 0; /* Uses negative indices. */
        this._variables = [];
        this._publicVariables = [];
        this._constraints = [];
        this._isProver = false;
    };
    Circuit.prototype.addConstraint = function (kind) {
        /* Generic gate is ql * l + qr * r + qo * o + qm * l * r + qc = 0 */
        switch (kind) {
            case "multiply":
                /* (qx * x + cx) * (qy * y + cy) = (qz * z + cz)
                   qx * qy * (x * y) + (qx * cy) * x + (qy * cx) * y + cx * cy = qz * z + cz
                */
                var x = arguments[1];
                var y = arguments[2];
                var z = arguments[3];
                var qx = new Field(x._scale ? x._scale : 1);
                var cx = new Field(x._constant ? x._constant : 0);
                var qy = new Field(y._scale ? y._scale : 1);
                var cy = new Field(y._constant ? y._constant : 0);
                var qz = new Field(z._scale ? z._scale : 1);
                var cz = new Field(z._constant ? z._constant : 0);
                if (!x._variable) {
                    qx = Field.zero;
                }
                if (!y._variable) {
                    qy = Field.zero;
                }
                if (!z._variable) {
                    qz = Field.zero;
                }
                var ql = qx.mul(cy);
                var qr = qy.mul(cx);
                var qo = qz.neg();
                var qm = qx.mul(qy);
                var qc = cx.mul(cy).sub(cz);
                var l = x._variable ? x._variable : 0;
                var r = y._variable ? y._variable : 0;
                var o = z._variable ? z._variable : 0;

                this._constraints.push({
                    kind: "generic",
                    ql: ql.toString(),
                    qr: qr.toString(),
                    qo: qo.toString(),
                    qm: qm.toString(),
                    qc: qc.toString(),
                    l: l,
                    r: r,
                    o: o
                });
                break;
            case "add":
                /* (qx * x + cx) + (qy * y + cy) = (qz * z + cz)
                   qx * x + qy * y + (cx + cy) = qz * z + cz
                */
                var x = arguments[1];
                var y = arguments[2];
                var z = arguments[3];
                var qx = new Field(x._scale ? x._scale : 1);
                var cx = new Field(x._constant ? x._constant : 0);
                var qy = new Field(y._scale ? y._scale : 1);
                var cy = new Field(y._constant ? y._constant : 0);
                var qz = new Field(z._scale ? z._scale : 1);
                var cz = new Field(z._constant ? z._constant : 0);
                if (!x._variable) {
                    qx = Field.zero;
                }
                if (!y._variable) {
                    qy = Field.zero;
                }
                if (!z._variable) {
                    qz = Field.zero;
                }
                var ql = qx;
                var qr = qy;
                var qo = qz;
                var qm = Field.zero;
                var qc = cx.add(cy).sub(cz);
                var l = x._variable ? x._variable : 0;
                var r = y._variable ? y._variable : 0;
                var o = z._variable ? z._variable : 0;

                this._constraints.push({
                    kind: "generic",
                    ql: ql.toString(),
                    qr: qr.toString(),
                    qo: qo.toString(),
                    qm: qm.toString(),
                    qc: qc.toString(),
                    l: l,
                    r: r,
                    o: o
                });
                break;
            case "equal":
                /* qx * x + cx = qy * y + cy */
                var x = arguments[1];
                var y = arguments[2];
                var qx = new Field(x._scale ? x._scale : 1);
                var cx = new Field(x._constant ? x._constant : 0);
                var qy = new Field(y._scale ? y._scale : 1);
                var cy = new Field(y._constant ? y._constant : 0);
                if (!x._variable) {
                    qx = Field.zero;
                }
                if (!y._variable) {
                    qy = Field.zero;
                }
                var ql = qx;
                var qr = qy;
                var qo = Field.zero;
                var qm = Field.zero;
                var qc = cx.sub(cy);
                var l = x._variable ? x._variable : 0;
                var r = y._variable ? y._variable : 0;
                var o = 0;

                this._constraints.push({
                    kind: "generic",
                    ql: ql.toString(),
                    qr: qr.toString(),
                    qo: qo.toString(),
                    qm: qm.toString(),
                    qc: qc.toString(),
                    l: l,
                    r: r,
                    o: o
                });
                break;
            case "boolean":
                /* (qx * x + cx) * (qx * x + cx) = (qx * x + cx)
                   qx * qx * (x * x) + qx * cx * x + qx * cx * x + cx * cx = qx * x + cx
                   qx * qx * (x * x) + (2 * qx * cx - qx) * x + (cx * cx - cx) = 0
                */
                var x = arguments[1];
                var qx = new Field(x._scale ? x._scale : 1);
                var cx = new Field(x._constant ? x._constant : 0);
                if (!x._variable) {
                    qx = Field.zero;
                }
                var ql = qx.mul(2).mul(cx);
                var qr = Field.zero;
                var qo = Field.zero;
                var qm = qx.mul(qx);
                var qc = cx.mul(cx).sub(cx);
                var l = x._variable ? x._variable : 0;
                var r = x._variable ? x._variable : 0;
                var o = 0;

                this._constraints.push({
                    kind: "generic",
                    ql: ql.toString(),
                    qr: qr.toString(),
                    qo: qo.toString(),
                    qm: qm.toString(),
                    qc: qc.toString(),
                    l: l,
                    r: r,
                    o: o
                });
                break;
        }
    };
    Circuit.prototype.newVariable = function (arg) {
        if (this._isProver) {
            if (arg !== undefined) {
                var value;
                if (typeof arg === "function") {
                    value = new Field(arg.call(this));
                } else if (arg instanceof Field) {
                    value = arg;
                } else {
                    value = new Field(arg);
                }
                value = value.value();
                return new Field(value);
            } else {
                throw new Error("Cannot allocate new unfilled variables in prover-only code");
            }
        } else {
            /* Pre-increment to allow space for the constant at variable 0. */
            let variable = ++this._nextVariable;
            if (arg !== undefined) {
                var value;
                this._isProver = true;
                if (typeof arg === "function") {
                    value = new Field(arg.call(this));
                } else if (arg instanceof Field) {
                    value = arg;
                } else {
                    value = new Field(arg);
                }
                value = value.value();
                this._isProver = false;
                this._variables[variable] = value;
            }
            return new Field(this, variable);
        }
    };
    Circuit.prototype.newPublicVariable = function (arg) {
        if (this._isProver) {
            throw new Error("Cannot allocate new public variables in prover-only code");
        } else {
            /* Public variables are given negative indices, so that we can add
               arbitrarily without disrupting the numbers of normal variables.
            */
            let variable = -(++this._nextPublicVariable);
            if (arg !== undefined) {
                var value;
                this._isProver = true;
                if (typeof arg === "function") {
                    value = new Field(arg.call(this));
                } else if (arg instanceof Field) {
                    value = arg;
                } else {
                    value = new Field(arg);
                }
                value = value.value();
                this._isProver = false;
                this._Publicvariables[-variable] = value;
            }
            return new Field(this, variable);
        }
    };
    Circuit.prototype._read = function (arg) {
        var x;
        var variable = arg._variable;
        if (variable) {
            if (!this._isProver) {
                console.warn("Reading a variable outside of prover-only code");
            }

            /* NB: Can't be 0, because 0 is falsy. */
            if (variable > 0) {
                x = this._variables[variable]
            } else {
                x = this._publicVariables[-variable]
            }

            if (!x) {
                throw new Error("Attempted to read a variable before a value was stored in it");
            }

            if (arg._scale) {
                x = x.mul(arg._scale);
            }

            if (arg._constant) {
                x = x.add(arg._constant);
            }
            return x._constant;
        } else {
            return (new Field(arg))._constant;
        }
    };
    Circuit.prototype.setVariable = function (x, arg) {
        if (!x instanceof Field) {
            throw new Error("Called setVariable with an invalid argument");
        } else if (!x._variable) {
            throw new Error("Called setVariable with a constant argument");
        } else if (x._constant || x._scale) {
            throw new Error("Called setVariable with a non-variable argument");
        }
        var value;
        let oldIsProver = this._isProver;
        this._isProver = true;
        if (typeof arg === "function") {
            value = new Field(arg.call(this));
        } else if (arg instanceof Field) {
            value = arg;
        } else {
            value = new Field(arg);
        }
        value = value.value();
        this._isProver = oldIsProver;
        if (x._variable > 0) {
            if (this._variables[variable]) {
                throw new Error("Attempted to set a variable with an existing value");
            }
            this._variables[variable] = value;
        } else {
            if (this._publicVariables[-variable]) {
                throw new Error("Attempted to set a variable with an existing value");
            }
            this._publicVariables[-variable] = value;
        }
    };
    Circuit.prototype.witness = function (valCtor, arg2, arg3) {
        var varCtor;
        var f;
        if (arg3 === undefined) {
            varCtor = valCtor;
            f = arg2;
        } else {
            varCtor = arg2;
            f = arg3;
        }
        var value;
        let oldIsProver = this._isProver;
        this._isProver = true;
        if (typeof f === "function") {
            value = f.call(this);
        } else {
            value = f;
        }
        this._isProver = oldIsProver;
        var fields = valCtor.toFieldElements(value);
        let vars = fields.map((x) => {
            let x_ = new Field(x);
            return this.newVariable(x_);
        });
        varCtor.ofFieldElements(vars);
    };
    Circuit.prototype.array = function (ctor, length) {
        var sizeInFieldElements = function () {
            return ctor.sizeInFieldElements() * length;
        }
        var toFieldElements = function (x) {
            assert(x.length === length);
            var res = [];
            for (var i = 0; i < length; i++) {
                res.concat(ctor.toFieldElements(x[i]));
            }
            return res;
        }
        var ofFieldElements = function (fields) {
            assert(fields.length === sizeInFieldElements());
            var size = ctor.sizeInFieldElements();
            var res = []
            for (var i = 0; i < length; i++) {
                res.push(ctor.ofFieldElements(fields.slice(i * size, (i + 1) * size)));
            }
            return res;
        };
        return { toFieldElements, ofFieldElements, sizeInFieldElements };
    };
    return Circuit;
};

export const mkField = mkField;
export const mkCircuit = mkCircuit;
