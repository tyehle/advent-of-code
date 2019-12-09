mod intcode;
use intcode::*;

fn main() {
    let input = parse();
    println!("{:?}", execute(input.clone(), vec![1]).output);
    println!("{:?}", execute(input.clone(), vec![2]).output);
}
