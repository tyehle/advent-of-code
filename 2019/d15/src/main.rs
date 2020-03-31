use console::{style, Term};
use num_complex::Complex;
use num_traits::identities::Zero;
use std::collections::HashMap;
use std::fs;

mod intcode;
use intcode::*;

#[derive(Debug)]
enum Direction {
    North,
    South,
    West,
    East,
}

impl Direction {
    fn iter() -> std::slice::Iter<'static, Direction> {
        [
            Direction::North,
            Direction::East,
            Direction::South,
            Direction::West,
        ]
        .iter()
    }

    fn code(&self) -> i64 {
        match self {
            Direction::North => 1,
            Direction::South => 2,
            Direction::West => 3,
            Direction::East => 4,
        }
    }

    fn unit(&self) -> Complex<i64> {
        match self {
            Direction::North => Complex::new(0, 1),
            Direction::South => Complex::new(0, -1),
            Direction::West => Complex::new(-1, 0),
            Direction::East => Complex::new(1, 0),
        }
    }

    fn from_unit(unit: Complex<i64>) -> Option<Direction> {
        match unit {
            Complex { re: 0, im: 1 } => Some(Direction::North),
            Complex { re: 0, im: -1 } => Some(Direction::South),
            Complex { re: -1, im: 0 } => Some(Direction::West),
            Complex { re: 1, im: 0 } => Some(Direction::East),
            _ => None,
        }
    }
}

enum Tile {
    Blank,
    Wall,
    Oxygen,
}

struct Droid {
    controller: State,
    o2_len: Option<usize>,
    path: Vec<Complex<i64>>,
    pos: Complex<i64>,
    map: HashMap<Complex<i64>, Tile>,
}

impl Droid {
    fn step(&mut self, direction: &Direction) {
        self.controller.push(direction.code());
        let new_pos = match self.controller.pop_lazy() {
            None => panic!("No response for input {:?}", direction),

            Some(0) => {
                // Hit a wall
                self.map.insert(self.pos + direction.unit(), Tile::Wall);
                self.pos
            }

            Some(1) => {
                // Moved
                let new_pos = self.pos + direction.unit();
                self.map.insert(new_pos, Tile::Blank);
                new_pos
            }

            Some(2) => {
                // Found the thing
                if self.o2_len.is_none() {
                    // Don't update this again once we know
                    // If we step back through it things will break
                    self.o2_len = Some(self.path.len() + 1);
                }
                let new_pos = self.pos + direction.unit();
                self.map.insert(new_pos, Tile::Oxygen);
                new_pos
            }

            Some(other) => panic!("Unknown response: {}", other),
        };

        if new_pos == self.pos {
            // Don't need to do anything to the path cause we didn't move
        } else if self.path.last() == Some(&new_pos) {
            self.path.pop();
        } else {
            self.path.push(self.pos);
        }

        self.pos = new_pos;
    }

    fn auto_step(&mut self, count: u32) {
        for _ in 0..count {
            // are there places to explore around us?
            let unexplored =
                Direction::iter().find(|dir| !self.map.contains_key(&(self.pos + dir.unit())));

            match unexplored {
                Some(dir) => self.step(dir),
                None => {
                    match self.path.last() {
                        None => return, // The space is fully epxlored
                        Some(new_pos) => {
                            self.step(&Direction::from_unit(new_pos - self.pos).unwrap())
                        }
                    }
                }
            }
        }
    }
}

fn parse() -> Vec<i64> {
    parse_program(fs::read_to_string("input.txt").unwrap())
}

fn init_output() {
    // Clear the screen
    print!("\u{1B}[2J\u{1B}[?25l");
}

fn finish_output() {
    print!("\u{1B}[?25h");
}

fn print_world(droid: &Droid) {
    let spaces = droid.map.keys().collect::<Vec<_>>();

    let min_y = spaces.iter().min_by(|a, b| a.im.cmp(&b.im)).unwrap().im - 2;
    let max_y = spaces.iter().max_by(|a, b| a.im.cmp(&b.im)).unwrap().im + 2;

    let min_x = spaces.iter().min_by(|a, b| a.re.cmp(&b.re)).unwrap().re - 5;
    let max_x = spaces.iter().max_by(|a, b| a.re.cmp(&b.re)).unwrap().re + 5;

    fn tile_glyph(tile: Option<&Tile>) -> console::StyledObject<&'static str> {
        match tile {
            None => style(" "),
            Some(Tile::Blank) => style(".").black(),
            Some(Tile::Wall) => style("#").bold().black(),
            Some(Tile::Oxygen) => style("O").cyan(),
        }
    }

    print!("\u{1B}[H");
    for row in (min_y..=max_y).rev() {
        for col in min_x..=max_x {
            let loc = Complex::new(col, row);
            let glyph = {
                if droid.pos == loc {
                    style("D").red().bold()
                } else {
                    tile_glyph(droid.map.get(&loc))
                }
            };
            print!("{}", glyph);
        }
        print!("\n");
    }
}

fn interact(instructions: &[i64]) {
    let mut droid = Droid {
        controller: State::new(instructions),
        o2_len: None,
        path: Vec::new(),
        pos: Complex::zero(),
        map: HashMap::new(),
    };

    droid.map.insert(droid.pos, Tile::Blank);

    let console = Term::stdout();

    init_output();
    loop {
        match console.read_char().unwrap() {
            // 'w' => droid.step(&Direction::North),
            // 'a' => droid.step(&Direction::West),
            // 's' => droid.step(&Direction::South),
            // 'd' => droid.step(&Direction::East),
            ' ' => droid.auto_step(1),
            'n' => droid.auto_step(10),
            '\n' => droid.auto_step(100),
            '\u{4}' | 'q' => break, // eof
            _ => continue,
        };

        print_world(&droid);
    }
    finish_output();

    println!("Path to O2: {:?}", droid.o2_len);
}

fn main() {
    let input = parse();

    interact(&input);
}
