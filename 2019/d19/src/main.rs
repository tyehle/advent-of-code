mod intcode;
use intcode::*;

fn count_affected(input: &[i64], size: i64) -> usize {
    let mut result = 0;

    for x in 0..size {
        for y in 0..size {
            let mut controller = State::new(input);
            controller.push(x);
            controller.push(y);

            match controller.pop_lazy() {
                Some(0) => {},
                Some(1) => result += 1,
                bad => panic!("Unexpected output: {:?}", bad)
            }
        }
    }

    result
}

fn main() {
    let input = parse();
    println!("{}", count_affected(&input, 50));
}
