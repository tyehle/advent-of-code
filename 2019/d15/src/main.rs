use console::{style, Term};
use num_complex::Complex;
use num_traits::identities::Zero;
use std::collections::{HashMap, HashSet};
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


#[derive(PartialEq, Eq)]
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

    fn auto_step(&mut self, count: u32) -> bool {
        for _ in 0..count {
            // are there places to explore around us?
            let unexplored =
                Direction::iter().find(|dir| !self.map.contains_key(&(self.pos + dir.unit())));

            match unexplored {
                Some(dir) => self.step(dir),
                None => {
                    match self.path.last() {
                        None => return true, // The space is fully epxlored
                        Some(new_pos) => {
                            self.step(&Direction::from_unit(new_pos - self.pos).unwrap())
                        }
                    }
                }
            }
        }

        false
    }
}


fn spread_o2(map: &mut HashMap<Complex<i64>, Tile>, fringe: &mut HashSet<Complex<i64>>, n: u32) -> u32 {
    fn emtpy_adjacent(map: &HashMap<Complex<i64>, Tile>, loc: Complex<i64>) -> Vec<Complex<i64>> {
        Direction::iter().map(|d| loc+d.unit()).filter(|adj| map.get(adj) == Some(&Tile::Blank)).collect()
    }

    for i in 0..n {
        if fringe.is_empty() {
            return i;
        }

        let elements = fringe.iter().cloned().collect::<Vec<_>>();

        fringe.clear();

        for loc in elements {
            for turned in emtpy_adjacent(map, loc) {
                map.insert(turned, Tile::Oxygen);
                fringe.insert(turned);
            }
        }
    }

    n
}


fn parse() -> Vec<i64> {
    parse_program(fs::read_to_string("input.txt").unwrap())
}


fn init_output() {
    // Clear the screen
    println!("\u{1B}[2J\u{1B}[?25l");
}

fn finish_output() {
    print!("\u{1B}[?25h");
}


fn print_world<D, V>(map: &HashMap<Complex<i64>, V>, glyph: impl Fn(Complex<i64>) -> console::StyledObject<D>)
where D: std::fmt::Display {
    let spaces = map.keys().collect::<Vec<_>>();

    let ys = spaces.iter().map(|c| c.im);
    let min_y = ys.clone().min().unwrap() - 2;
    let max_y = ys.max().unwrap() + 2;

    let xs = spaces.iter().map(|c| c.re);
    let min_x = xs.clone().min().unwrap() - 3;
    let max_x = xs.max().unwrap() + 3;

    print!("\u{1B}[H"); // reset cursor
    for row in (min_y..=max_y).rev() {
        for col in min_x..=max_x {
            print!("{}", glyph(Complex::new(col, row)));
        }
        print!("\n");
    }
}


fn print_droid(droid: &Droid) {
    let glyph = |loc| {
        let droid_style = {
            if droid.pos == loc {
                console::Style::new().on_red()
            } else {
                console::Style::new()
            }
        };

        let tile = match droid.map.get(&loc) {
            None => style("  "),
            Some(Tile::Blank) => style("..").black(),
            Some(Tile::Wall) => style("▓▓").bold().black(),
            Some(Tile::Oxygen) => style("O2").cyan().bold(),
        };

        droid_style.apply_to(tile)
    };

    print_world(&droid.map, glyph);
}


fn print_o2(map: &HashMap<Complex<i64>, Tile>, fringe: &HashSet<Complex<i64>>) {
    let glyph = |loc| {
        match map.get(&loc) {
            None => style("  "),
            Some(Tile::Blank) => style("..").black(),
            Some(Tile::Wall) => style("▓▓").bold().black(),
            Some(Tile::Oxygen) => {
                if fringe.contains(&loc) {
                    style("O2").cyan().bold()
                } else {
                    style("O2").cyan()
                }
            },
        }
    };

    print_world(map, glyph);
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

    print_droid(&droid);

    loop {
        let done = match console.read_char().unwrap() {
            ' ' => droid.auto_step(1),
            'n' => droid.auto_step(10),
            '\n' => droid.auto_step(100),
            '\u{4}' | 'q' => return, // eof
            _ => continue,
        };

        print_droid(&droid);

        if done {
            break;
        }
    }

    // Everything shold be fully explored by now
    match droid.map.iter().find(|(_, v)| **v == Tile::Oxygen).map(|(k, _)| *k) {
        None => (),

        Some(o2_loc) => {
            let mut fringe = HashSet::new();
            fringe.insert(o2_loc);

            let mut n = 0;

            loop {
                let steps = match console.read_char().unwrap() {
                    ' ' => spread_o2(&mut droid.map, &mut fringe, 1),
                    'n' => spread_o2(&mut droid.map, &mut fringe, 10),
                    '\n' => spread_o2(&mut droid.map, &mut fringe, 100),
                    '\u{4}' | 'q' => return, // eof
                    _ => continue,
                };

                print_o2(&droid.map, &fringe);

                if steps == 0 {
                    break;
                }

                n += steps;
            }

            println!("Path to O2: {}", droid.o2_len.unwrap());
            println!("O2 time: {}", n-1);
        }
    }
}

fn main() {
    let input = parse();

    init_output();
    interact(&input);
    finish_output();
}
