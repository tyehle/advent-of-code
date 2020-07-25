use im::HashSet;
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

fn distance_to_keys(
    world: &HashMap<C, Tile>,
    keys: &HashSet<char>,
    start: C,
) -> Vec<(char, usize)> {
    let mut fringe = VecDeque::new();
    let mut done = HashMap::new();

    fringe.push_back((start, 0));

    while let Some((pos, distance)) = fringe.pop_front() {
        if done.contains_key(&pos) {
            continue;
        }

        fringe.extend(
            adjacent(pos)
                .iter()
                .filter(|p| traversable(world, keys, **p))
                .map(|p| (*p, distance + 1)),
        );
        done.insert(pos, distance);
    }

    done.iter()
        .filter_map(|(k, v)| match world[k] {
            Tile::Key(name) if !keys.contains(&name) => Some((name, *v)),
            _ => None,
        })
        .collect()
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
        for (key, distance) in distance_to_keys(world, &keys, pos) {
            let mut new_keys = keys.clone();
            new_keys.insert(key);
            if let Some(remaining_distance) =
                collect_remaining(world, all_keys, cache, all_keys[&key], new_keys)
            {
                if best
                    .map(|d| distance + remaining_distance < d)
                    .unwrap_or(true)
                {
                    best = Some(distance + remaining_distance);
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

        let distances = distance_to_keys(
            &world,
            &HashSet::from(vec!['a', 'b', 'c']),
            objects[&Tile::Key('c')],
        );

        assert_eq!(distances, vec![('e', 14), ('d', 24)]);
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
