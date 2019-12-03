use std::fs;
use std::collections::{HashMap, HashSet};
use std::iter::FromIterator;

fn parse() -> Vec<Vec<String>> {
    fs::read_to_string("input.txt")
        .unwrap()
        .trim()
        .lines()
        .map(|l| l.trim().split(",").map(String::from).collect())
        .collect()
}

fn parse_part(part: &String) -> (char, i32) {
    (part.chars().next().unwrap(), part[1..].parse().unwrap())
}

fn ocupied(wire: &Vec<String>) -> HashMap<(i32, i32), i32> {
    let mut map = HashMap::new();
    let mut x = 0;
    let mut y = 0;
    let mut len = 1;

    fn update(x: i32, y: i32, map: &mut HashMap<(i32, i32), i32>, len: &mut i32) {
        if !map.contains_key(&(x, y)) {
            map.insert((x, y), len.clone());
        }
        *len += 1
    }

    for op in wire {
        match parse_part(op) {
            ('U', n) => {
                for i in 1..n+1 {
                    update(x, y+i, &mut map, &mut len);
                }
                y += n
            },

            ('D', n) => {
                for i in 1..n+1 {
                    update(x, y-i, &mut map, &mut len);
                }
                y -= n
            },

            ('R', n) => {
                for i in 1..n+1 {
                    update(x+i, y, &mut map, &mut len);
                }
                x += n
            },

            ('L', n) => {
                for i in 1..n+1 {
                    update(x-i, y, &mut map, &mut len);
                }
                x -= n
            }

            (bad, _) => panic!("Bad direction: {}", bad),
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

    let aset: HashSet<(i32, i32)> = HashSet::from_iter(oa.keys().cloned());
    let bset = HashSet::from_iter(ob.keys().cloned());

    let mut inter: Vec<&(i32, i32)> = aset.intersection(&bset).collect();

    inter.sort_by_key(|(a, b)| a.abs() + b.abs());

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
        let o = dbg!(ocupied(&vec!["R8".to_owned(), "U5".to_owned(), "L5".to_owned(), "D6".to_owned()]));
        assert_eq!(*o.get(&(3, 0)).unwrap(), 3);
    }

    #[test]
    fn test_len() {
        let a = &"R75,D30,R83,U83,L12,D49,R71,U7,L72".split(",").map(String::from).collect();
        let b = &"U62,R66,U55,R34,D71,R55,D58,R83".split(",").map(String::from).collect();

        let oa = ocupied(a);
        let ob = ocupied(b);

        let aset: HashSet<(i32, i32)> = HashSet::from_iter(oa.keys().cloned());
        let bset = HashSet::from_iter(ob.keys().cloned());

        let mut inter: Vec<&(i32, i32)> = aset.intersection(&bset).collect();

        inter.sort_by_key(|(a, b)| a.abs() + b.abs());

        println!("Closest jump: {:?}", inter[0]);

        inter.sort_by_key(|k| oa.get(k).unwrap() + ob.get(k).unwrap());
        let k = inter[0];

        dbg!(inter);

        println!("Closest: {}", oa.get(k).unwrap() + ob.get(k).unwrap());

        assert!(false);
    }
}
