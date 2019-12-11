use num_complex::Complex;
use std::collections::{HashMap, VecDeque};
use std::iter::FromIterator;

mod intcode;
use intcode::*;

struct Robot {
    controller: State,
    pos: Complex<i64>,
    dir: Complex<i64>,
}

impl Robot {
    fn new(prog: Vec<i64>) -> Robot {
        Robot {
            controller: State {
                pc: 0,
                rel_base: 0,
                mem: HashMap::from_iter(prog.into_iter().enumerate()),
                input: VecDeque::new(),
                output: VecDeque::new(),
            },
            pos: Complex::new(0, 0),
            dir: Complex::i(),
        }
    }
}

fn paint(robot: &mut Robot, tiles: &mut HashMap<Complex<i64>, i64>) -> bool {
    // see if we are done
    while step(&mut robot.controller) {}
    if robot.controller.is_done() {
        return false;
    }

    // input color
    robot.controller.push(*tiles.entry(robot.pos).or_insert(0));

    // paint a color
    let color = robot.controller.pop_lazy().expect("No output color!");
    tiles.insert(robot.pos, color);

    // move
    match robot.controller.pop_lazy() {
        None => panic!("No direction!"),
        Some(0) => robot.dir *= Complex::i(),
        Some(1) => robot.dir *= -Complex::i(),
        Some(bad) => panic!("Bad direction: {}", bad),
    }
    robot.pos += robot.dir;

    true
}

fn main() {
    let input = parse();

    let mut robot = Robot::new(input.clone());
    let mut tiles = HashMap::new();
    while paint(&mut robot, &mut tiles) {}
    println!("{}", tiles.values().count());

    robot = Robot::new(input);
    tiles = HashMap::new();
    tiles.insert(Complex::new(0, 0), 1);
    while paint(&mut robot, &mut tiles) {}
    for y in (-10..5).rev() {
        for x in -10..50 {
            let c = match tiles.get(&Complex::new(x, y)).unwrap_or(&0) {
                0 => " ",
                1 => "#",
                _ => "!",
            };
            print!("{}", c);
        }
        println!();
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_thing() {
        unimplemented!();
    }
}
