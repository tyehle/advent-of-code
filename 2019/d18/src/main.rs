use num_complex::Complex;
use std::collections::HashMap;

type C = Complex<i32>;

#[derive(PartialEq, Eq, Hash, Debug)]
enum Tile {
    Open,
    Wall,
    Entrance,
    Door(char),
    Key(char),
}

fn parse() -> (HashMap<C, Tile>, HashMap<Tile, C>) {
    let input = std::fs::read_to_string("input.txt").expect("Failed to open input file");

    let mut map = HashMap::new();
    let mut objects = HashMap::new();
    let mut x = 0;
    let mut y = 0;

    for c in input.chars() {
        // add to the map
        let pos = Complex::new(x, y);
        match c {
            '.' => {
                map.insert(pos, Tile::Open);
            }

            '#' => {
                map.insert(pos, Tile::Wall);
            }

            '@' => {
                map.insert(pos, Tile::Entrance);
                objects.insert(Tile::Entrance, pos);
            }

            '\n' => {}

            c if c.is_ascii_uppercase() => {
                let name = c.to_lowercase().next().unwrap();
                map.insert(pos, Tile::Door(name));
                objects.insert(Tile::Door(name), pos);
            }

            c if c.is_ascii_lowercase() => {
                map.insert(pos, Tile::Key(c));
                objects.insert(Tile::Key(c), pos);
            }

            bad => panic!("Unrecognized character {}", bad),
        };

        // update our position
        if c == '\n' {
            x = 0;
            y += 1;
        } else {
            x += 1;
        }
    }

    (map, objects)
}

fn main() {
    let (map, objects) = parse();

    println!("{:?}", objects);
}
