use num_complex::Complex;
use std::collections::{HashMap, HashSet};
use std::convert::TryFrom;
use im::Vector;
use itertools::Itertools;

mod intcode;
use intcode::*;

type C = Complex<i64>;

fn output_string(controller: &mut State) -> String {
    let mut result = String::with_capacity(1500);

    while let Some(next) = controller.pop_lazy() {
        result.push(char::from(
            u8::try_from(next).unwrap_or_else(|_| panic!("Invalid character {}", next)),
        ));
    }

    result
}

fn get_initial_state(input: &[i64]) -> (HashMap<C, bool>, C, C) {
    fn dir_of(c: char) -> C {
        match c {
            '>' => Complex::new(1, 0),
            '^' => Complex::new(0, -1),
            '<' => Complex::new(-1, 0),
            'v' => Complex::new(0, 1),
            _ => panic!("Not a direction annotation {}", c),
        }
    }

    let mut controller = State::new(input);

    let mut map = HashMap::new();
    let mut pos = Complex::new(-1, -1);
    let mut dir = Complex::new(0, 0);

    let mut x = 0;
    let mut y = 0;

    for c in output_string(&mut controller).chars() {
        match c {
            '.' => {
                map.insert(Complex::new(x, y), false);
                x += 1;
            }

            '#' => {
                map.insert(Complex::new(x, y), true);
                x += 1;
            },

            '>' | '^' | '<' | 'v' => {
                pos = Complex::new(x, y);
                dir = dir_of(c);
                map.insert(pos, true);
                x += 1;
            }

            '\n' => {
                x = 0;
                y += 1;
            }

            _ => panic!("Unknown character {} at position ({}, {})", c, x, y),
        }
    }

    (map, pos, dir)
}

fn has_scaffold(map: &HashMap<C, bool>, pos: &C) -> bool {
    *map.get(pos).unwrap_or(&false)
}

fn alignment_sum(input: &[i64]) -> i64 {
    let (map, _, _) = get_initial_state(input);

    fn adjacent(pos: C) -> Vec<C> {
        let Complex { re: x, im: y } = pos;
        vec![
            pos,
            Complex::new(x + 1, y),
            Complex::new(x, y - 1),
            Complex::new(x - 1, y),
            Complex::new(x, y + 1),
        ]
    }

    let mut sum = 0;
    for &loc in map.keys() {
        if adjacent(loc).iter().all(|pos| has_scaffold(&map, pos)) {
            sum += loc.re * loc.im
        }
    }

    sum
}

#[derive(Eq, PartialEq, Hash, Debug, Clone)]
enum Command {
    Left,
    Right,
    Forward(u64),
}

impl Command {
    fn size(&self) -> usize {
        use Command::*;

        match self {
            Left | Right => 1,
            Forward(n) if *n < 10 => 1,
            Forward(n) if *n < 100 => 2,
            Forward(n) => (*n as f64).log10().ceil() as usize,
        }
    }

    fn serialize(&self) -> String {
        use Command::*;

        match self {
            Left => String::from("L"),
            Right => String::from("R"),
            Forward(n) => format!("{}", n),
        }
    }
}

fn build_path(map: HashMap<C, bool>, start_pos: C, start_dir: C) -> Vec<Command> {
    let left = Complex::new(0, -1);
    let right = Complex::new(0, 1);

    let mut pos = start_pos;
    let mut dir = start_dir;
    let mut steps = 0;

    let mut path = Vec::new();

    loop {
        if has_scaffold(&map, &(pos + dir)) {
            // we can move forward
            steps += 1;
            pos += dir;
        } else if has_scaffold(&map, &(pos + (dir * left))) {
            // we can turn left
            path.push(Command::Forward(steps));
            steps = 0;
            path.push(Command::Left);
            dir *= left;
        } else if has_scaffold(&map, &(pos + (dir * right))) {
            // we can turn right
            path.push(Command::Forward(steps));
            steps = 0;
            path.push(Command::Right);
            dir *= right;
        } else {
            // we are at the end of the road
            path.push(Command::Forward(steps));
            break;
        }
    }

    path.remove(0);
    path
}

fn compress_path(path: &[Command], num_subroutines: usize, max_subroutine_size: usize) -> Option<(Vec<usize>, Vec<Vec<Command>>)> {
    // consider all possible subroutines

    let mut all_subs = HashSet::new();

    for start in 0..path.len()-1 {
        let mut end = start + 2;
        let mut budget = path[start].size() + path[start+1].size();

        while end < path.len() {
            let size = path[end].size();
            if budget+size > max_subroutine_size {
                break;
            }
            all_subs.insert(&path[start..=end]);
            budget += size;
            end += 1;
        }
    }

    let mut best: Option<(Vec<usize>, Vec<&[Command]>)> = None;

    for subroutines in all_subs.iter().cloned().combinations(num_subroutines) {
        if let Some(candidate) = compress(path, &subroutines) {
            if best.as_ref().map(|(p, _)| candidate.len() < p.len()).unwrap_or(true) {
                best = Some((candidate, subroutines));
            }
        }
    }

    best.map(|(main, subs)| (main, subs.iter().map(|&sub| Vec::from(sub)).collect()))
}

fn compress(path: &[Command], subroutines: &[&[Command]]) -> Option<Vec<usize>> {
    fn go<'a>(path: &[Command], subroutines: &[&[Command]], cache: &'a mut HashMap<usize, Option<Vector<usize>>>, pos: usize) -> Option<Vector<usize>> {
        if let Some(result) = cache.get(&pos) {
            return result.clone();
        }

        if pos == path.len() {
            return Some(Vector::new());
        }

        let mut best: Option<Vector<usize>> = None;

        // compare best to any possible subroutine replacement
        for (i, subroutine) in subroutines.iter().enumerate() {
            // avoid a potential infinite loop
            if subroutine.is_empty() {
                continue;
            }

            if path[pos..].starts_with(subroutine) {
                // we got a match, now check if doing the sub is shorter
                if let Some(mut candidate) = go(path, subroutines, cache, pos + subroutine.len()) {
                    candidate.push_front(i);

                    if best.as_ref().map(|p| candidate.len() < p.len()).unwrap_or(true) {
                        best = Some(candidate);
                    }
                }
            }
        }

        cache.insert(pos, best.clone());
        best
    }

    let mut cache = HashMap::new();
    go(path, subroutines, &mut cache, 0).map(|p| p.iter().cloned().collect())
}

fn traverse_scaffold(input: &[i64]) -> Option<i64> {
    fn push_string(controller: &mut State, string: String) {
        for c in string.chars() {
            controller.push(c as i64);
        }
    }

    fn push_list<I>(controller: &mut State, elements: I) where I: IntoIterator<Item=String> {
        let mut iter = elements.into_iter();

        // push the first one without a comma
        iter.next().map(|e| push_string(controller, e));

        for e in iter {
            push_string(controller, String::from(","));
            push_string(controller, e);
        }

        push_string(controller, String::from("\n"));
    }

    let (map, pos, dir) = get_initial_state(&input);
    let path = build_path(map, pos, dir);

    let (main, subroutines) = compress_path(&path, 3, 10).expect("No path found!");

    let mut controller = State::new(&input);

    // wake up
    controller.set(0, 2);
    // input the main
    push_list(&mut controller, main.iter().map(|call| String::from_utf8(vec![('A' as usize + call) as u8]).unwrap()));
    // input the subroutines
    for sub in subroutines {
        push_list(&mut controller, sub.iter().map(|i| i.serialize()));
    }
    // turn off video
    push_list(&mut controller, vec![String::from("n")]);

    // get the output, ignoring all the preceding other ascii values
    while let Some(result) = controller.pop_lazy() {
        if result >= 256 {
            return Some(result);
        }
    }

    None
}

fn main() {
    let input = parse();

    println!("{}", alignment_sum(&input));

    println!("{:?}", traverse_scaffold(&input));
}
