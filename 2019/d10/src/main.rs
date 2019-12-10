use num_complex::Complex;
use std::collections::HashSet;
use std::convert::TryInto;
use std::fs;
use std::f32;

fn read() -> HashSet<Complex<i32>> {
    parse(fs::read_to_string("input.txt").unwrap())
}

fn parse(s: String) -> HashSet<Complex<i32>> {
    let mut out = HashSet::new();

    for (y, line) in s.trim().lines().enumerate() {
        for (x, c) in line.trim().chars().enumerate() {
            if c == '#' {
                out.insert(Complex::new(x.try_into().unwrap(), y.try_into().unwrap()));
            }
        }
    }

    out
}

fn gcd(a: i32, b: i32) -> i32 {
    let mut ma = a;
    let mut mb = b;

    while mb != 0 {
        let t = mb;
        mb = ma % mb;
        ma = t;
    }

    ma
}

fn can_see(blocked: &HashSet<Complex<i32>>, from: &Complex<i32>, what: &Complex<i32>) -> bool {
    if what == from {
        return false;
    }

    let slope = what - from;
    let common = gcd(slope.re.abs(), slope.im.abs());

    let mut pos = from.clone();
    let step = slope / common;

    for _ in 1..common.abs() {
        pos += step;
        if blocked.contains(&pos) {
            return false;
        }
    }

    true
}

fn num_seen(blocked: &HashSet<Complex<i32>>, from: &Complex<i32>) -> usize {
    blocked.iter().filter(|p| can_see(blocked, from, p)).count()
}

fn angle(what: &Complex<i32>) -> f32 {
    let a = f32::atan2(what.re as f32, -what.im as f32);
    if a < 0.0 {
        a + 2.0*f32::consts::PI
    } else {
        a
    }
}

fn ith_seen(blocked: &HashSet<Complex<i32>>, from: &Complex<i32>, i: usize) -> Complex<i32> {
    let mut round: Vec<_> = blocked
        .iter()
        .filter(|p| can_see(&blocked, from, p))
        .collect();

    round.sort_by(|a, b| {
        angle(&(*a - from))
            .partial_cmp(&angle(&(*b - from)))
            .unwrap()
    });

    // for x in round.iter() { println!("{}        {}", x, (*x-from)); }

    round[i - 1].clone()
}

fn main() {
    let input = read();
    let best_point = input.iter().max_by_key(|p| num_seen(&input, p)).unwrap();
    println!("{}", num_seen(&input, best_point));
    println!("{}", ith_seen(&input, best_point, 200));
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_can_see() {
        let input = parse(
            "..#..
             #.#.#
             .#.#.
             ..#..
             .#.#.
             #...#"
                .to_string(),
        );

        assert!(!can_see(&input, &Complex::new(2, 3), &Complex::new(4, 5)));
        assert!(!can_see(&input, &Complex::new(2, 3), &Complex::new(0, 5)));
        assert!(!can_see(&input, &Complex::new(2, 3), &Complex::new(4, 1)));
        assert!(!can_see(&input, &Complex::new(2, 3), &Complex::new(0, 1)));
        assert!(can_see(&input, &Complex::new(2, 3), &Complex::new(1, 2)));
        assert!(can_see(&input, &Complex::new(2, 3), &Complex::new(3, 2)));
        assert!(can_see(&input, &Complex::new(2, 3), &Complex::new(3, 4)));
        assert!(can_see(&input, &Complex::new(2, 3), &Complex::new(1, 4)));
        assert!(can_see(&input, &Complex::new(2, 3), &Complex::new(2, 1)));
        assert!(!can_see(&input, &Complex::new(2, 3), &Complex::new(2, 0)));
    }

    #[test]
    fn test_thing() {
        assert_eq!(
            num_seen(
                &parse(
                    ".#..#
                     .....
                     #####
                     ....#
                     ...##"
                        .to_string()
                ),
                &Complex::new(3, 4)
            ),
            8
        );

        assert_eq!(
            num_seen(
                &parse(
                    "......#.#.
                    #..#.#....
                    ..#######.
                    .#.#.###..
                    .#..#.....
                    ..#....#.#
                    #..#....#.
                    .##.#..###
                    ##...#..#.
                    .#....####"
                        .to_string()
                ),
                &Complex::new(5, 8)
            ),
            33
        );

        let complex = parse(
            ".#..##.###...#######
            ##.############..##.
            .#.######.########.#
            .###.#######.####.#.
            #####.##.#.##.###.##
            ..#####..#.#########
            ####################
            #.####....###.#.#.##
            ##.#################
            #####.##.###..####..
            ..######..##.#######
            ####.##.####...##..#
            .#####..#.######.###
            ##...#.##########...
            #.##########.#######
            .####.#.###.###.#.##
            ....##.##.###..#####
            .#.#.###########.###
            #.#.#.#####.####.###
            ###.##.####.##.#..##"
                .to_string(),
        );

        assert_eq!(num_seen(&complex, &Complex::new(11, 13)), 210);
        assert_eq!(ith_seen(&complex, &Complex::new(11, 13), 200), Complex::new(8, 2));
    }
}
