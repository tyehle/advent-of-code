use nom::bytes::complete::tag;
use nom::character::complete::{char, digit1, line_ending, one_of, space0};
use nom::combinator::{opt, recognize};
use nom::sequence::{delimited, preceded, tuple};
use nom::{map, map_res, named, preceded, separated_list};
use std::fs;
use std::fmt;
use std::ops::AddAssign;
use std::collections::HashSet;

#[derive(Debug, Copy, Clone, Hash, Eq, PartialEq)]
struct R3 {
    x: i32,
    y: i32,
    z: i32,
}

impl R3 {
    fn new(vs: [i32; 3]) -> R3 {
        R3 {
            x: vs[0],
            y: vs[1],
            z: vs[2],
        }
    }

    fn zero() -> R3 {
        R3 {x: 0, y: 0, z: 0}
    }
}

impl AddAssign for R3 {
    fn add_assign(&mut self, other: Self) {
        *self = Self {
            x: self.x + other.x,
            y: self.y + other.y,
            z: self.z + other.z,
        };
    }
}

impl fmt::Display for R3 {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "<x={:4}, y={:4}, z={:4}>", self.x, self.y, self.z)
    }
}

#[derive(Debug, Hash, Clone, PartialEq, Eq)]
struct Moon {
    pos: R3,
    vel: R3,
}

impl fmt::Display for Moon {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "pos: {}, vel: {}", self.pos, self.vel)
    }
}

impl Moon {
    fn new(pos: R3) -> Moon {
        Moon {
            pos,
            vel: R3::zero(),
        }
    }
}

fn read() -> Vec<Moon> {
    parse(&fs::read_to_string("input.txt").unwrap())
}

fn parse(s: &str) -> Vec<Moon> {
    full_input(s).unwrap().1.iter().cloned().map(Moon::new).collect()
}

// parser

named!(
    int<&str, i32>,
    map_res!(recognize(preceded(opt(char('-')), digit1)), str::parse)
);

named!(
    coord<&str, i32>,
    preceded!(one_of("xyz"), preceded(char('='), int))
);

named!(
    row<&str, R3>,
    map!(
        delimited(
            char('<'),
            tuple((
                coord,
                preceded(tag(", "), coord),
                preceded(tag(", "), coord)
            )),
            char('>')
        ),
        |(x,y,z)| R3 {x, y, z}
    )
);

named!(
    full_input<&str, Vec<R3>>,
    separated_list!(preceded(line_ending, space0), row)
);

// logic

fn clip(n: i32) -> i32 {
    if n > 0 { 1 }
    else if n == 0 { 0 }
    else { -1 }
}

fn step(moons: &mut Vec<Moon>) {
    // update velocities
    for i in 0..moons.len() {
        for j in 0..moons.len() {
            if i == j {
                continue;
            }
            // println!("{} -> {}", moons[i], moons[j]);
            moons[i].vel.x += clip(moons[j].pos.x - moons[i].pos.x);
            moons[i].vel.y += clip(moons[j].pos.y - moons[i].pos.y);
            moons[i].vel.z += clip(moons[j].pos.z - moons[i].pos.z);
        }
    }

    // update positions
    for moon in moons.iter_mut() {
        moon.pos += moon.vel;
    }
}

fn energy(moon: &Moon) -> i32 {
    let potential = moon.pos.x.abs() + moon.pos.y.abs() + moon.pos.z.abs();
    let kinetic = moon.vel.x.abs() + moon.vel.y.abs() + moon.vel.z.abs();

    potential * kinetic
}

fn show(moons: &Vec<Moon>) {
    for moon in moons {
        println!("{}", moon);
    }
}

fn lcm(a: usize, b: usize) -> usize {
    use num::Integer;
    a.lcm(&b)
}

fn steps_till_loop(moons: &mut Vec<Moon>) -> usize {
    let mut seen_x = HashSet::<Vec<(i32, i32)>>::new();
    let mut seen_y = HashSet::<Vec<(i32, i32)>>::new();
    let mut seen_z = HashSet::<Vec<(i32, i32)>>::new();

    loop {

        let x = moons.iter().cloned().map(|m| (m.pos.x, m.vel.x)).collect();
        let y = moons.iter().cloned().map(|m| (m.pos.y, m.vel.y)).collect();
        let z = moons.iter().cloned().map(|m| (m.pos.z, m.vel.z)).collect();

        if seen_x.contains(&x) && seen_y.contains(&y) && seen_z.contains(&z) {
            break;
        }

        seen_x.insert(x);
        seen_y.insert(y);
        seen_z.insert(z);

        step(moons);
    }

    lcm(lcm(seen_x.len(), seen_y.len()), seen_z.len())
}

fn main() {
    let input = read();

    let mut p1 = input.clone();
    for _ in 0..1000 {
        step(&mut p1)
    }
    println!("{}", p1.iter().map(energy).sum::<i32>());

    println!("{}", steps_till_loop(&mut input.clone()));
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_parsing() {
        assert_eq!(int("-12"), Ok(("", -12)));
        assert_eq!(int("0"), Ok(("", 0)));

        assert_eq!(coord("y=-4"), Ok(("", -4)));

        assert_eq!(row("<x=1, y=3, z=-11>"), Ok(("", R3::new([1, 3, -11]))));

        assert_eq!(
            full_input("<x=1, y=3, z=-11>\n<x=-1, y=-15, z=2>"),
            Ok(("", vec![R3::new([1, 3, -11]), R3::new([-1, -15, 2])]))
        );
    }

    #[test]
    fn test_step() {
        let mut moons = parse(
            "<x=-1, y=0, z=2>
            <x=2, y=-10, z=-7>
            <x=4, y=-8, z=8>
            <x=3, y=5, z=-1>"
        );

        step(&mut moons);

        assert_eq!(moons[0], Moon{pos: R3::new([2, -1, 1]), vel: R3::new([3, -1, -1])});
        // pos=<x= 2, y=-1, z= 1>, vel=<x= 3, y=-1, z=-1>
        assert_eq!(moons[1], Moon{pos: R3::new([3, -7, -4]), vel: R3::new([1, 3, 3])});
        // pos=<x= 3, y=-7, z=-4>, vel=<x= 1, y= 3, z= 3>
        assert_eq!(moons[2], Moon{pos: R3::new([1, -7, 5]), vel: R3::new([-3, 1, -3])});
        // pos=<x= 1, y=-7, z= 5>, vel=<x=-3, y= 1, z=-3>
        assert_eq!(moons[3], Moon{pos: R3::new([2, 2, 0]), vel: R3::new([-1, -3, 1])});
        // pos=<x= 2, y= 2, z= 0>, vel=<x=-1, y=-3, z= 1>

        for _ in 0..9 {
            step(&mut moons);
        }

        assert_eq!(moons[0], Moon{pos: R3::new([2, 1, -3]), vel: R3::new([-3, -2, 1])});
        // pos=<x= 2, y= 1, z=-3>, vel=<x=-3, y=-2, z= 1>
        assert_eq!(moons[1], Moon{pos: R3::new([1, -8, 0]), vel: R3::new([-1, 1, 3])});
        // pos=<x= 1, y=-8, z= 0>, vel=<x=-1, y= 1, z= 3>
        assert_eq!(moons[2], Moon{pos: R3::new([3, -6, 1]), vel: R3::new([3, 2, -3])});
        // pos=<x= 3, y=-6, z= 1>, vel=<x= 3, y= 2, z=-3>
        assert_eq!(moons[3], Moon{pos: R3::new([2, 0, 4]), vel: R3::new([1, -1, -1])});
        // pos=<x= 2, y= 0, z= 4>, vel=<x= 1, y=-1, z=-1>
    }

    #[test]
    fn test_energy() {
        let mut moons = parse(
            "<x=-1, y=0, z=2>
            <x=2, y=-10, z=-7>
            <x=4, y=-8, z=8>
            <x=3, y=5, z=-1>"
        );

        for _ in 0..10 {
            step(&mut moons);
        }

        assert_eq!(energy(&Moon {pos: R3::new([-1, 0, 2]), vel: R3::new([-2, 1, 1])}), 12);

        assert_eq!(moons.iter().map(energy).sum::<i32>(), 179);
    }

    #[test]
    fn test_loop() {
        use num::Integer;
        assert_eq!(3usize.lcm(&5), 15);

        let mut moons = parse(
            "<x=-1, y=0, z=2>
            <x=2, y=-10, z=-7>
            <x=4, y=-8, z=8>
            <x=3, y=5, z=-1>"
        );

        assert_eq!(steps_till_loop(&mut moons), 2772);
    }
}
