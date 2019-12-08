use std::fs;

fn read() -> Vec<u32> {
    parse(fs::read_to_string("input.txt").unwrap().trim())
}

fn parse(s: &str) -> Vec<u32> {
    s.chars().map(|c| c.to_digit(10).unwrap()).collect()
}

fn chunk(data: &Vec<u32>, size: usize) -> Vec<Vec<u32>> {
    data.chunks(size).map(Vec::from).collect()
}

fn part_1(layers: &Vec<Vec<u32>>) -> usize {
    let least_zeros = layers.into_iter().min_by_key(|layer| layer.iter().filter(|n| **n == 0).count()).unwrap();

    let ones = least_zeros.iter().filter(|n| **n == 1).count();
    let twos = least_zeros.iter().filter(|n| **n == 2).count();

    ones * twos
}

fn flatten(layers: &Vec<Vec<u32>>) -> Vec<u32> {
    let mut it = layers.iter();
    let mut out = it.next().unwrap().clone();

    for layer in it {
        for i in 0..out.len() {
            out[i] = match out[i] {
                0 => 0,
                1 => 1,
                2 => layer[i],
                bad => panic!("Borked at {}: {}", i, bad),
            }
        }
    }

    out
}

fn main() {
    let width = 25;
    let height = 6;
    let input = read();

    let layers = chunk(&input, width*height);

    println!("{}", part_1(&layers));

    for row in flatten(&layers).chunks(width) {
        for n in row {
            print!("{}", match n {
                0 => " ",
                1 => "#",
                2 => "?",
                _ => panic!("WHAT"),
            });
        }
        println!("");
    }
}
