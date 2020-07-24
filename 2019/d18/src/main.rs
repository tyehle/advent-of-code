use im::HashSet;
use num_complex::Complex;
use std::collections::{HashMap, VecDeque};

type C = Complex<i32>;

#[derive(PartialEq, Eq, Hash, Debug)]
enum Tile {
    Open,
    Wall,
    Entrance,
    Door(char),
    Key(char),
}

fn parse_input() -> (HashMap<C, Tile>, HashMap<Tile, C>) {
    parse(std::fs::read_to_string("input.txt").expect("Failed to open input file"))
}

fn parse(input: String) -> (HashMap<C, Tile>, HashMap<Tile, C>) {
    let mut world = HashMap::new();
    let mut objects = HashMap::new();
    let mut x = 0;
    let mut y = 0;

    for c in input.chars() {
        // add to the map
        let pos = Complex::new(x, y);
        match c {
            '.' => {
                world.insert(pos, Tile::Open);
            }

            '#' => {
                world.insert(pos, Tile::Wall);
            }

            '@' => {
                world.insert(pos, Tile::Entrance);
                objects.insert(Tile::Entrance, pos);
            }

            '\n' => {}

            c if c.is_ascii_uppercase() => {
                let name = c.to_lowercase().next().unwrap();
                world.insert(pos, Tile::Door(name));
                objects.insert(Tile::Door(name), pos);
            }

            c if c.is_ascii_lowercase() => {
                world.insert(pos, Tile::Key(c));
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

    (world, objects)
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

fn shortest_path(
    world: &HashMap<C, Tile>,
    keys: &HashSet<char>,
    start: C,
    dest: C,
) -> Option<usize> {
    let mut fringe = VecDeque::new();
    let mut done = HashSet::new();

    fringe.push_back((start, 0));

    while let Some((pos, distance)) = fringe.pop_front() {
        if done.contains(&pos) {
            continue;
        }

        if pos == dest {
            return Some(distance);
        }

        fringe.extend(
            adjacent(pos)
                .iter()
                .filter(|p| traversable(world, keys, **p))
                .map(|p| (*p, distance + 1)),
        );
        done.insert(pos);
    }

    None
}

fn collect_all_keys(world: &HashMap<C, Tile>, objects: &HashMap<Tile, C>) -> Option<usize> {
    let start = objects.get(&Tile::Entrance).unwrap().clone();

    let all_keys = objects
        .iter()
        .filter_map(|(tile, pos)| match tile {
            Tile::Key(name) => Some((*name, *pos)),
            _ => None,
        })
        .collect();

    fn collect_remaining(
        world: &HashMap<C, Tile>,
        all_keys: &HashMap<char, C>,
        cache: &mut HashMap<(C, HashSet<char>), Option<usize>>,
        pos: C,
        keys: HashSet<char>,
    ) -> Option<usize> {
        if let Some(&result) = cache.get(&(pos, keys.clone())) {
            return result;
        }

        // check to see if we are done
        if all_keys.len() == keys.len() {
            return Some(0);
        }

        let mut best = None;
        // consider all keys can we reach from here
        for (&key, &key_loc) in all_keys.iter().filter(|(k, _)| !keys.contains(k)) {
            if let Some(distance) = shortest_path(world, &keys, pos, key_loc) {
                // println!("Found {} path to {} using {:?}", distance, key, keys);
                let mut new_keys = keys.clone();
                new_keys.insert(key);
                if let Some(remaining_distance) =
                    collect_remaining(world, all_keys, cache, key_loc, new_keys)
                {
                    let candidate = distance + remaining_distance;
                    if best.map(|d| candidate < d).unwrap_or(true) {
                        best = Some(candidate);
                    }
                }
            }
        }

        cache.insert((pos, keys), best);
        best
    };

    let mut cache = HashMap::new();
    collect_remaining(world, &all_keys, &mut cache, start, HashSet::new())
}

fn main() {
    let (world, objects) = parse_input();

    // got 186. Too low

    println!("{}", collect_all_keys(&world, &objects).unwrap());
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_shortest_path() {
        let (world, objects) = parse(String::from(
            "\
            ########################\n\
            #f.D.E.e.C.b.A.@.a.B.c.#\n\
            ######################.#\n\
            #d.....................#\n\
            ########################",
        ));

        let dist = shortest_path(
            &world,
            &HashSet::from(vec!['a', 'b', 'c']),
            objects[&Tile::Key('c')],
            objects[&Tile::Key('e')],
        );
        assert_eq!(dist, Some(14));

        let dist = shortest_path(
            &world,
            &HashSet::from(vec!['a', 'b', 'c']),
            objects[&Tile::Key('c')],
            objects[&Tile::Key('f')],
        );
        assert_eq!(dist, None);

        let dist = shortest_path(
            &world,
            &HashSet::from(vec!['a', 'b', 'c']),
            objects[&Tile::Key('c')],
            objects[&Tile::Key('d')],
        );
        assert_eq!(dist, Some(24));
    }

    #[test]
    fn test_collect_keys() {
        let (world, objects) = parse(String::from(
            "\
            ########################\n\
            #f.D.E.e.C.b.A.@.a.B.c.#\n\
            ######################.#\n\
            #d.....................#\n\
            ########################",
        ));

        assert_eq!(Some(86), collect_all_keys(&world, &objects));
    }
}
