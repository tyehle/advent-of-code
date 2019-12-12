use num_complex::Complex;
use std::collections::{HashMap, VecDeque};
use std::io;
use std::io::{Read, Write};
use std::iter::FromIterator;
use std::thread;
use std::time::Duration;

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

fn tile_glyph(color: &i64) -> &'static str {
    match color {
        0 => "\x1B[90;40m \x1B[0m",
        1 => "\x1B[90;47m \x1B[0m",
        _ => "\x1B[31;m!\x1B[0m",
    }
}

fn robot_glyph(robot: &Robot) -> String {
    let glyph = match robot.dir {
        Complex { re: 0, im: 1 } => '^',
        Complex { re: 0, im: -1 } => 'v',
        Complex { re: 1, im: 0 } => '>',
        Complex { re: -1, im: 0 } => '<',
        bad => panic!("Bad robot orientation: {}", bad),
    };
    format!("\x1B[96m{}\x1B[0m", glyph)
}

fn draw_at(what: &str, width: i16, pos: &Complex<i64>) {
    let to_pos = pos - Complex::new(-10, -11);
    print!(
        "{}{}{}",
        // move the cursor there
        ansi_escapes::CursorMove::XY(to_pos.re as i16, -to_pos.im as i16),
        // draw the thing
        what,
        // replace the cursor
        ansi_escapes::CursorMove::XY(-to_pos.re as i16 - width, to_pos.im as i16)
    );
}

fn draw_all(robot: &Robot, tiles: &HashMap<Complex<i64>, i64>) {
    for y in (-10..5).rev() {
        for x in -10..50 {
            print!(
                "{}",
                tile_glyph(tiles.get(&Complex::new(x, y)).unwrap_or(&0))
            );
        }
        println!();
    }
    draw_at(&robot_glyph(robot), 1, &robot.pos);
    io::stdout().flush().expect("Failed to flush stdout");
}

fn update_screen(robot: &Robot, tiles: &HashMap<Complex<i64>, i64>, prev: &Complex<i64>) {
    draw_at(tile_glyph(tiles.get(prev).unwrap_or(&0)), 1, prev);
    draw_at(&robot_glyph(robot), 1, &robot.pos);
    io::stdout().flush().expect("Failed to flush stdout");
}

fn block() {
    // io::stdin().bytes().next().expect("Failed to read user input").expect("Failed to read user input");
    let mut buffer = [0; 1];
    io::stdin().read_exact(&mut buffer).expect("Failed to read user input");
    print!("{}", ansi_escapes::CursorUp(1));
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
    draw_all(&robot, &tiles);
    let mut prev = robot.pos;
    while paint(&mut robot, &mut tiles) {
        update_screen(&robot, &tiles, &prev);
        prev = robot.pos;
        thread::sleep(Duration::from_millis(50));
        // block();
    }
}
