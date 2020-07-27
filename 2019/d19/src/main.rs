mod intcode;
use intcode::*;


fn probe(input: &[i64], x: i64, y: i64) -> bool {
    let mut controller = State::new(input);

    controller.push(x);
    controller.push(y);

    match controller.pop_lazy() {
        Some(1) => true,
        Some(0) => false,
        bad => panic!("Unexpected output: {:?}", bad),
    }
}

fn count_affected(input: &[i64], size: i64) -> usize {
    let mut result = 0;

    let visualize = true;

    for y in 0..size {
        for x in 0..size {
            if probe(input, x, y) {
                if visualize { print!("█"); }
                result += 1
            } else {
                if visualize { print!("░"); }
            }
        }
        if visualize { println!(); }
    }

    result
}

fn closest_square(input: &[i64], size: i64) -> (i64, i64) {
    let mut x = 0;
    let mut y = 0;

    while !probe(input, x, y+size-1) {
        println!("{} {}", x, y);
        while !probe(input, x, y+size-1) { x += 1; }
        while !probe(input, x+size-1, y) { y+= 1; }
    }

    (x, y)
}

fn main() {
    let input = parse();
    println!("{}", count_affected(&input, 50));
    let (x, y) = closest_square(&input, 100);
    println!("({}, {}) -> {}", x, y, x*10000+y);
}
