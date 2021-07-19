import { Field, AsField } from './plonk';

export class Scalar {
    static toFieldElements(x: Scalar): Field[]
    static ofFieldElements(fields: Field[]): Scalar;
    static sizeInFieldElements(): number;
}

export class Group {
    x: Field;
    y: Field;

    add(this: Group, y: Group): Group;
    neg(this: Group): Group;
    scale(this: Group, y: Scalar): Group;
    endoScale(this: Group, y: Scalar): Group;

    assertEqual(this: Group, y: Group): void;

    constructor(args: { x: AsField<Field>, y: AsField<Field> })
    constructor(x: AsField<Field>, y: AsField<Field>)

    static generator(): Group;
    static add(x: Group, y: Group): Group;
    static neg(x: Group): Group;
    static scale(x: Group, y: Scalar): Group;
    static endoScale(x: Group, y: Scalar): Group;

    static ofString(x: string): Group;

    static assertEqual(x: Group, y: Group): void;

    static toFieldElements(x: Group): Field[]
    static ofFieldElements(fields: Field[]): Group;
    static sizeInFieldElements(): number;
}
