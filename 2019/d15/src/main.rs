use std::fs;

mod intcode;
use intcode::*;


fn parse() -> Vec<i64> {
    parse_program(fs::read_to_string("input.txt").unwrap())
}


fn main() {
    let input = parse();
    dbg!(input);
}
