
type Z<a> = {Z: a}
type NZ<a> = {NZ: a}
type OptZero<a> = Z<a> | NZ<a>;
function getProperty<Type, Key extends keyof Type>(obj: Type, key: Key) {
    return obj[key];
}


// base syntax: literals and addition

interface Lit<a> {
    lit(i: number): a;
}

interface Add<a> {
    add(a: a, b: a): a;
}

class LiteralNumber implements Lit<number> {
    lit(i: number): number {
        return i
    }
}

function sum<Z extends {Z: number}, NZ extends {NZ: number}>(x: Z, y: NZ) {
    return y
}
const zsum = sum({Z: 0}, {NZ: 2});
console.log(zsum)


class LiteralOptionZero implements Lit<OptZero<number>> {
    lit(i: number): OptZero<number> {
        return i === 0 ? {Z: 0} : {NZ: i}
    }
}
const lit = (i: number): Z<number> | NZ<number> => i === 0 ? {Z: 0} : {NZ: i}

class Integer implements Add<OptZero<number>> {
    // add(x: number, y: number): number
    //add<T extends {suc: Nat}>(x: T, y: number): number
    // add(x: OptZero<number>, y: number): number
    // add(x: OptZero<number>, y: OptZero<number>): number
    add(x: OptZero<number>, y: OptZero<number>): OptZero<number> {
        const x_is_zero = x['Z'] !== undefined ? true : false;
        const y_is_zero = y['Z'] !== undefined ? true : false;

        let res = lit(x['NZ'] + y['NZ']);
        if (x_is_zero) {
            res = lit(y['NZ'])
        }
        if (y_is_zero) {
            res = lit(x['NZ'])
        }
        console.log(x, "+", y, "=", res);
        return res
    }
}
const add = (x: OptZero<number>, y: OptZero<number>) => new Integer().add(x,y)

const a: OptZero<number> = lit(0);
const b: OptZero<number> = lit(2);
sum(a, b)

const res = add(add(lit(0), lit(2)), add(lit(2), lit(8)))
console.log(`${JSON.stringify(res)}`);

type ReplaceNumberPropertiesWithNull<T> = {
    [K in keyof T]: T[K] extends number ? null : T[K]
};

const user = {
    name: 'Amir',
    age: 36,
  };
type Person = {name: string, age: number};
type PropertyTypes1 = Person['name' | 'age'];
type PropertyTypes2 = number | Person['age'];
type NameOrAge = (typeof user)['name' | 'age'];
const nameOrAge: NameOrAge = 'Betty';


type User = {
    email: string
    age: number
};

type UserAggregate = {
    [K in keyof User]: Array<string>
};

const userAggregate: UserAggregate = {
    email: ['amir@example.com'],
    age: ['36'],
};

console.log(userAggregate);