use im::{HashSet, Vector};
use num_complex::Complex;
use std::collections::{HashMap, VecDeque};

type C = Complex<i32>;

#[derive(PartialEq, Eq, Hash, Clone, Debug)]
enum Tile {
    Open,
    Wall,
    Entrance,
    Door(char),
    Key(char),
}

struct World {
    tiles: HashMap<C, Tile>,
    keys: HashMap<char, C>,
    entrances: Vec<C>,
}

fn parse_input() -> World {
    parse(std::fs::read_to_string("input.txt").expect("Failed to open input file"))
}

fn parse(input: String) -> World {
    let mut tiles = HashMap::new();
    let mut keys = HashMap::new();
    let mut entrances = Vec::new();

    let mut x = 0;
    let mut y = 0;

    for c in input.chars() {
        // add to the map
        let pos = Complex::new(x, y);
        match c {
            '.' => {
                tiles.insert(pos, Tile::Open);
            }

            '#' => {
                tiles.insert(pos, Tile::Wall);
            }

            '@' => {
                tiles.insert(pos, Tile::Entrance);
                entrances.push(pos);
            }

            '\n' => {}

            c if c.is_ascii_uppercase() => {
                let name = c.to_lowercase().next().unwrap();
                tiles.insert(pos, Tile::Door(name));
            }

            c if c.is_ascii_lowercase() => {
                tiles.insert(pos, Tile::Key(c));
                keys.insert(c, pos);
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

    World { tiles, keys, entrances }
}

fn adjacent(pos: C) -> Vec<C> {
    let Complex { re: x, im: y } = pos;
    vec![
        Complex::new(x + 1, y),
        Complex::new(x, y + 1),
        Complex::new(x - 1, y),
        Complex::new(x, y - 1),
    ]
}

fn traversable(world: &HashMap<C, Tile>, keys: &HashSet<char>, pos: C) -> bool {
    world
        .get(&pos)
        .map(|tile| match tile {
            Tile::Open | Tile::Entrance | Tile::Key(_) => true,
            Tile::Wall => false,
            Tile::Door(name) => keys.contains(name),
        })
        .unwrap_or(false)
}

fn distance_to_keys(
    world: &World,
    keys: &HashSet<char>,
    start: C,
) -> Vec<(char, usize)> {
    let mut fringe = VecDeque::new();
    let mut done = HashMap::new();
    let mut found_keys = Vec::new();

    fringe.push_back((start, 0));

    while let Some((pos, distance)) = fringe.pop_front() {
        if done.contains_key(&pos) {
            continue;
        }

        fringe.extend(
            adjacent(pos)
                .iter()
                .filter(|p| traversable(&world.tiles, keys, **p))
                .map(|p| (*p, distance + 1)),
        );
        done.insert(pos, distance);
        match world.tiles[&pos] {
            Tile::Key(name) if !keys.contains(&name) => found_keys.push((name, distance)),
            _ => {}
        }
    }

    found_keys
}

fn collect_all_keys(world: &World) -> Option<usize> {
    fn collect_remaining(
        world: &World,
        cache: &mut HashMap<(Vector<C>, HashSet<char>), Option<usize>>,
        input: (Vector<C>, HashSet<char>),
    ) -> Option<usize> {
        if let Some(&result) = cache.get(&input) {
            return result;
        }

        let (positions, keys) = input;

        // check to see if we are done
        if world.keys.len() == keys.len() {
            return Some(0);
        }

        let mut best = None;

        // consider all keys can we reach from here
        for (i, &pos) in positions.iter().enumerate() {
            for (key, distance) in distance_to_keys(world, &keys, pos) {
                let mut new_keys = keys.clone();
                new_keys.insert(key);
                let mut new_positions = positions.clone();
                new_positions[i] = world.keys[&key];
                if let Some(remaining_distance) =
                    collect_remaining(world, cache, (new_positions, new_keys))
                {
                    if best
                        .map(|d| distance + remaining_distance < d)
                        .unwrap_or(true)
                    {
                        best = Some(distance + remaining_distance);
                    }
                }
            }
        }

        cache.insert((positions, keys), best);
        best
    };

    let mut cache = HashMap::new();
    collect_remaining(world, &mut cache, (Vector::from(&world.entrances), HashSet::new()))
}

fn to_quad_world(world: &mut World) {
    // ensure there is only one entrance
    if world.entrances.len() != 1 {
        panic!("There must be one entrance!");
    }

    let pos = world.entrances[0];

    // add the new walls
    world.tiles.insert(pos, Tile::Wall);
    for new_wall in adjacent(pos) {
        world.tiles.insert(new_wall, Tile::Wall);
    }

    // add the new entrances
    let Complex { re: x, im: y } = pos;
    world.entrances = vec![
        Complex::new(x + 1, y + 1),
        Complex::new(x + 1, y - 1),
        Complex::new(x - 1, y - 1),
        Complex::new(x - 1, y + 1),
    ];
}

fn main() {
    let mut world = parse_input();

    println!("{}", collect_all_keys(&world).unwrap());

    to_quad_world(&mut world);
    println!("{}", collect_all_keys(&world).unwrap());
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_shortest_path() {
        let world = parse(String::from(
            "\
            ########################\n\
            #f.D.E.e.C.b.A.@.a.B.c.#\n\
            ######################.#\n\
            #d.....................#\n\
            ########################",
        ));

        let distances = distance_to_keys(
            &world,
            &HashSet::from(vec!['a', 'b', 'c']),
            Complex::new(21, 1),
        );

        assert_eq!(distances, vec![('e', 14), ('d', 24)]);
    }

    #[test]
    fn test_collect_keys() {
        let world = parse(String::from(
            "\
            ########################\n\
            #f.D.E.e.C.b.A.@.a.B.c.#\n\
            ######################.#\n\
            #d.....................#\n\
            ########################",
        ));

        assert_eq!(Some(86), collect_all_keys(&world));
    }
}
