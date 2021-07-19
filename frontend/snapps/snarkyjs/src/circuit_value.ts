import 'reflect-metadata';
import { Field } from './bindings/plonk';

type Constructor<T> = {new (...args: any[]): T};

export type Tuple<A, _N extends number> = Array<A>;

export abstract class CircuitValue {
  static sizeInFieldElements(): number {
    return (this as any).prototype._sizeInFieldElements || 0;
  }

  static toFieldElements<T>(this: Constructor<T>, v: T): Field[] {
    const res: Field[] = [];
    const fields = (this as any).prototype._fields;
    if (fields === undefined || fields === null) {
      return res;
    }

    for (let i = 0; i < fields.length; ++i) {
      const [key, propType] = fields[i];
      const subElts: Field[] = propType.toFieldElements((v as any)[key]);
      subElts.forEach(x => res.push(x));
    }
    return res;
  }

  static ofFieldElements<T>(this: Constructor<T>, xs: Field[]): T {
    const fields = (this as any).prototype._fields;
    let offset = 0;
    const props: any[] = [];
    for (let i = 0; i < fields.length; ++i) {
      const propType = fields[i][1];
      const propSize = propType.sizeInFieldElements();
      const propVal = propType.ofFieldElements(
        xs.slice(offset, offset + propSize)
      );
      props.push(propVal);
      offset += propSize;
    }
    // TODO: this is wrong
    console.log(this);
    return new this(...props);
  }
}

export function prop(this: any, target: any, key: string) {
  const fieldType = Reflect.getMetadata('design:type', target, key);

  if (target._fields === undefined || target._fields === null) {
    target._fields = [];
  }

  if (target._sizeInFieldElements === undefined || target._sizeInFieldElements === null) {
    target._sizeInFieldElements = 0;
  }

  if (fieldType.prototype.hasOwnProperty('toFieldElements') && fieldType.hasOwnProperty('ofFieldElements')) {
    target._fields.push([key, fieldType]);
    target._sizeInFieldElements += fieldType.sizeInFieldElements();
  } else {
    throw `property ${key} missing field element conversion methods`;
  }
}
