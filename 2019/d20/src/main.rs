use std::collections::{HashMap, HashSet, VecDeque};

type Point = (i64, i64);

#[derive(Debug)]
enum Tile {
    Portal(Point, String),
    Start,
    End,
    Open,
    Wall,
}

type World = HashMap<Point, Tile>;

fn parse() -> World {
    parse_input(std::fs::read_to_string("input.txt").expect("Could not open input.txt"))
}

fn parse_input(input: String) -> World {
    let mut world = HashMap::new();
    let mut labels = HashMap::new();

    let mut x = 1;
    let mut y = 1;
    for c in input.chars() {
        match c {
            ' ' | '\n' => {}
            '.' => {
                world.insert((x, y), Tile::Open);
            }
            '#' => {
                world.insert((x, y), Tile::Wall);
            }
            _ => {
                labels.insert((x, y), c);
            }
        }

        if c == '\n' {
            x = 1;
            y += 1;
        } else {
            x += 1;
        }
    }

    // turn the labels into tiles
    let mut portal_ends = HashMap::new();
    while let Some((&pos, &c)) = labels.iter().next() {
        // there will be only one label adjacent to this one
        let &other = adjacent(pos)
            .iter()
            .find(|p| labels.contains_key(p))
            .expect(&format!("Invalid label {} at ({}, {})", c, pos.0, pos.1));
        let name: String = if other > pos {
            [c, labels[&other]].iter().collect()
        } else {
            [labels[&other], c].iter().collect()
        };

        labels.remove(&pos);
        labels.remove(&other);

        // find the adjacent open tile
        let &labeled_loc = adjacent(pos)
            .iter()
            .chain(adjacent(other).iter())
            .find(|p| match world.get(p) {
                Some(Tile::Open) => true,
                _ => false,
            })
            .unwrap();

        if name == "AA" {
            world.insert(labeled_loc, Tile::Start);
        } else if name == "ZZ" {
            world.insert(labeled_loc, Tile::End);
        } else {
            match portal_ends.remove(&name) {
                None => {
                    portal_ends.insert(name, labeled_loc);
                }

                Some(exit) => {
                    world.insert(labeled_loc, Tile::Portal(exit, name.clone()));
                    world.insert(exit, Tile::Portal(labeled_loc, name));
                }
            }
        }
    }

    world
}

fn adjacent(pos: Point) -> Vec<Point> {
    let (x, y) = pos;
    vec![(x + 1, y), (x, y + 1), (x - 1, y), (x, y - 1)]
}

fn step(world: &World, pos: Point) -> Vec<Point> {
    let mut adj: Vec<Point> = adjacent(pos)
        .iter()
        .filter(|p| match world.get(p) {
            None | Some(Tile::Wall) => false,
            _ => true,
        })
        .cloned()
        .collect();

    match world.get(&pos) {
        Some(&Tile::Portal(exit, _)) => { adj.push(exit); },
        _ => {},
    }

    adj
}

fn bfs(world: &World) -> Option<usize> {
    let &start = world.iter().find(|(_, tile)| match tile { Tile::Start => true, _ => false }).unwrap().0;

    let mut done = HashSet::new();
    let mut fringe = VecDeque::new();
    fringe.push_back((start, 0));

    while let Some((pos, distance)) = fringe.pop_front() {
        if done.contains(&pos) {
            continue;
        }

        if let Some(Tile::End) = world.get(&pos) {
            return Some(distance);
        }

        fringe.extend(step(world, pos).iter().map(|&p| (p, distance+1)));
        done.insert(pos);
    }

    None
}

fn main() {
    let input = parse();
    println!("{:?}", bfs(&input));
}
