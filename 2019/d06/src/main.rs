use std::collections::{HashMap, HashSet};
use std::fs;
use std::iter::FromIterator;

fn read() -> String {
    fs::read_to_string("input.txt").unwrap()
}

fn parse(s: &str) -> HashMap<String, Vec<String>> {
    let mut out = HashMap::new();

    for line in s.trim().lines() {
        let parts: Vec<&str> = line.trim().split(')').collect();

        out.entry(parts[0].to_string())
            .and_modify(|v: &mut Vec<String>| v.push(parts[1].to_string()))
            .or_insert_with(|| vec![parts[1].to_string()]);
    }

    out
}

fn count_orbits(map: &HashMap<String, Vec<String>>) -> i32 {
    let mut stack = Vec::new();
    let mut counts = HashMap::new();

    stack.push("COM");
    counts.insert("COM", 0);

    while stack.len() > 0 {
        let body = stack.pop().unwrap();
        let count = *counts.get(body).unwrap();

        match map.get(body) {
            None => {}
            Some(children) => {
                for child in children {
                    stack.push(child);
                    counts.insert(child, count + 1);
                }
            }
        }
    }

    counts.values().sum()
}

fn path(rev_map: &HashMap<String, String>, source: &str) -> Vec<String> {
    let mut out = Vec::new();
    let mut cur = source;

    while cur != "COM" {
        out.push(cur.to_string());
        cur = rev_map.get(cur).unwrap();
    }

    out
}

fn min_transfers(source: &str, target: &str, map: &HashMap<String, Vec<String>>) -> usize {
    let mut rev_map = HashMap::new();
    for (parent, children) in map.iter() {
        for child in children {
            rev_map.insert(child.clone(), parent.clone());
        }
    }

    let s_path: HashSet<String> = HashSet::from_iter(path(&rev_map, source).iter().cloned());
    let t_path = HashSet::from_iter(path(&rev_map, target).iter().cloned());

    s_path.symmetric_difference(&t_path).count() - 2
}

fn main() {
    let input = parse(&read());
    println!("{}", count_orbits(&input));
    println!("{}", min_transfers("YOU", "SAN", &input));
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_transfer() {
        let input = parse(
            "COM)B
            B)C
            C)D
            D)E
            E)F
            B)G
            G)H
            D)I
            E)J
            J)K
            K)L
            K)YOU
            I)SAN",
        );

        assert_eq!(4, min_transfers("YOU", "SAN", &input));
    }
}
