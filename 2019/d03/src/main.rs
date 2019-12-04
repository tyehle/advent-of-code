use num_complex::Complex;
use std::collections::{HashMap, HashSet};
use std::fs;
use std::iter::FromIterator;

struct Strand {
    dir: Complex<i32>,
    len: i32,
}

fn parse_strand(s: &str) -> Strand {
    let dir = match &s[..1] {
        "U" => Complex::new(0, 1),
        "D" => Complex::new(0, -1),
        "R" => Complex::new(1, 0),
        "L" => Complex::new(-1, 0),
        bad => panic!("Unknown direction: {}", bad),
    };

    let len = &s[1..].parse().unwrap();

    Strand { dir, len: *len }
}

fn parse() -> Vec<Vec<Strand>> {
    fs::read_to_string("input.txt")
        .unwrap()
        .trim()
        .lines()
        .map(|l| l.trim().split(',').map(parse_strand).collect())
        .collect()
}

fn ocupied<'a, I>(wire: I) -> HashMap<Complex<i32>, i32>
where
    I: IntoIterator<Item = &'a Strand>,
{
    let mut map = HashMap::new();
    let mut len = 0;
    let mut loc = Complex::new(0, 0);

    for op in wire {
        for _i in 1..=op.len {
            loc += op.dir;
            len += 1;
            map.entry(loc).or_insert(len);
        }
    }

    map
}

fn main() {
    let wires = parse();
    let a = &wires[0];
    let b = &wires[1];

    let oa = ocupied(a);
    let ob = ocupied(b);

    let aset: HashSet<Complex<i32>> = HashSet::from_iter(oa.keys().cloned());
    let bset = HashSet::from_iter(ob.keys().cloned());

    let mut inter: Vec<&Complex<i32>> = aset.intersection(&bset).collect();

    inter.sort_by_key(|c| c.re.abs() + c.im.abs());

    println!("Closest jump: {:?}", inter[0]);

    inter.sort_by_key(|k| oa.get(k).unwrap() + ob.get(k).unwrap());
    let k = inter[0];

    println!("Closest: {}", oa.get(k).unwrap() + ob.get(k).unwrap());
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_ocupied() {
        let wire: Vec<Strand> = "R8,U5,L5,D6".split(",").map(parse_strand).collect();
        let o = dbg!(ocupied(&wire));
        assert_eq!(*o.get(&Complex::new(3, 0)).unwrap(), 3);
    }

    #[test]
    fn test_len() {
        let a: Vec<Strand> = "R75,D30,R83,U83,L12,D49,R71,U7,L72"
            .split(",")
            .map(parse_strand)
            .collect();
        let b: Vec<Strand> = "U62,R66,U55,R34,D71,R55,D58,R83"
            .split(",")
            .map(parse_strand)
            .collect();

        let oa = ocupied(&a);
        let ob = ocupied(&b);

        let aset: HashSet<Complex<i32>> = HashSet::from_iter(oa.keys().cloned());
        let bset = HashSet::from_iter(ob.keys().cloned());

        let mut inter: Vec<&Complex<i32>> = aset.intersection(&bset).collect();

        inter.sort_by_key(|c| c.l1_norm());

        assert_eq!(inter[0].l1_norm(), 159);

        println!("Closest jump: {:?}", inter[0]);

        inter.sort_by_key(|k| oa.get(k).unwrap() + ob.get(k).unwrap());
        let k = inter[0];

        dbg!(inter);

        assert_eq!(610, oa.get(k).unwrap() + ob.get(k).unwrap());
    }
}
