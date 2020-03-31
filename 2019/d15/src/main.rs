use num_complex::Complex;
use num_traits::identities::Zero;
use std::collections::HashMap;
use std::fs;
use console::{Term, style};

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
}


enum Tile {
    Blank,
    Wall,
    Oxygen,
}


struct Droid {
    controller: State,
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
            },

            Some(1) => {
                // Moved
                let new_pos = self.pos + direction.unit();
                self.map.insert(new_pos, Tile::Blank);
                new_pos
            }

            Some(2) => {
                // Found the thing
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
            // TODO
        }
    }
}


fn parse() -> Vec<i64> {
    parse_program(fs::read_to_string("input.txt").unwrap())
}


fn init_output() {
    // Clear the screen
    print!("\u{1B}[2J");
}


fn print_world(droid: &Droid) {
    let spaces = droid.map.keys().collect::<Vec<_>>();

    let min_y = spaces.iter().min_by(|a, b| a.im.cmp(&b.im)).unwrap().im;
    let max_y = spaces.iter().max_by(|a, b| a.im.cmp(&b.im)).unwrap().im + 5;

    let min_x = spaces.iter().min_by(|a, b| a.re.cmp(&b.re)).unwrap().re - 5;
    let max_x = spaces.iter().max_by(|a, b| a.re.cmp(&b.re)).unwrap().re;

    fn tile_glyph(tile: Option<&Tile>) -> console::StyledObject<&'static str> {
        match tile {
            None => style(" "),
            Some(Tile::Blank) => style("."),
            Some(Tile::Wall) => style("#"),
            Some(Tile::Oxygen) => style("X").blue(),
        }
    }

    print!("\u{1B}[H");
    for row in (min_y..=max_y).rev() {
        for col in min_x..=max_x {
            let loc = Complex::new(col, row);
            let glyph = {
                if droid.pos == loc {
                    style("D").red()
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
        path: Vec::new(),
        pos: Complex::zero(),
        map: HashMap::new(),
    };

    droid.map.insert(droid.pos, Tile::Blank);

    let console = Term::stdout();

    init_output();
    loop {
        let direction = match console.read_char().unwrap() {
            'w' => Direction::North,
            'a' => Direction::West,
            's' => Direction::South,
            'd' => Direction::East,
            '\n' => {droid.auto_step(10); continue},
            '\u{4}' => break, // eof
            _ => continue,
        };

        droid.step(&direction);
        print_world(&droid);
    }

    println!("{}", droid.path.len());
}

fn main() {
    let input = parse();

    interact(&input);
}
