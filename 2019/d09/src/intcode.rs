use std::collections::{HashMap, VecDeque};
use std::convert::{TryFrom, TryInto};
use std::fs;

static DEBUG: bool = false;

#[derive(Debug)]
pub struct State {
    pub pc: usize,
    pub rel_base: i64,
    pub mem: Vec<i64>,
    pub extended_mem: HashMap<usize, i64>,
    pub input: VecDeque<i64>,
    pub output: VecDeque<i64>,
}

pub fn parse_program(s: String) -> Vec<i64> {
    s.trim().split(',').map(|i| i.parse().unwrap()).collect()
}

pub fn parse() -> Vec<i64> {
    parse_program(fs::read_to_string("input.txt").unwrap())
}

fn param_mode(position: usize, opcode: i64) -> i64 {
    opcode / 10i64.pow(u32::try_from(position).unwrap() + 2) % 10
}

fn get_mem(address: usize, s: &State) -> i64 {
    if address < s.mem.len() {
        s.mem[address]
    } else {
        *s.extended_mem.get(&address).unwrap_or(&0)
    }
}

fn get_param(position: usize, opcode: i64, s: &mut State) -> i64 {
    let immediate = s.mem[s.pc + position + 1];
    match param_mode(position, opcode) {
        0 => get_mem(usize::try_from(immediate).unwrap(), s),
        1 => immediate,
        2 => get_mem(usize::try_from(immediate + s.rel_base).unwrap(), s),
        _ => panic!("Unknown parameter mode at {}: {}", s.pc, opcode),
    }
}

fn set_mem(address: usize, value: i64, s: &mut State) {
    if address < s.mem.len() {
        s.mem[address] = value;
    } else {
        s.extended_mem.insert(address, value);
    }
}

fn store_value(position: usize, opcode: i64, s: &mut State, value: i64) {
    match param_mode(position, opcode) {
        0 => {
            let loc = s.mem[s.pc + position + 1];
            set_mem(usize::try_from(loc).unwrap(), value, s);
        }
        2 => {
            let loc = s.mem[s.pc + position + 1];
            set_mem(usize::try_from(loc + s.rel_base).unwrap(), value, s);
        }
        _ => panic!("Bad store mode at {}: {}", s.pc, opcode),
    }
}

// instructions

fn binop(opcode: i64, s: &mut State, op: impl Fn(i64, i64) -> i64) {
    if DEBUG {
        println!("binop {:?}", &s.mem[s.pc..s.pc + 4]);
    }
    let result = op(get_param(0, opcode, s), get_param(1, opcode, s));
    store_value(2, opcode, s, result);
    s.pc += 4;
}

fn read(opcode: i64, s: &mut State) {
    if DEBUG {
        println!("read  {:?}", &s.mem[s.pc..s.pc + 2]);
    }
    let result = s.input.pop_front().unwrap();
    store_value(0, opcode, s, result);
    s.pc += 2;
}

fn write(opcode: i64, s: &mut State) {
    if DEBUG {
        println!("write {:?}", &s.mem[s.pc..s.pc + 2]);
    }
    let value = get_param(0, opcode, s);
    s.output.push_back(value);
    s.pc += 2;
}

fn jump_cond(opcode: i64, s: &mut State, cond: impl Fn(i64) -> bool) {
    if DEBUG {
        println!("c_jmp {:?}", &s.mem[s.pc..s.pc + 3]);
    }
    if cond(get_param(0, opcode, s)) {
        s.pc = get_param(1, opcode, s).try_into().unwrap();
    } else {
        s.pc += 3;
    }
}

fn adj_rel_base(opcode: i64, s: &mut State) {
    if DEBUG {
        println!("rel_base {:?}", &s.mem[s.pc..s.pc + 2])
    }
    s.rel_base += get_param(0, opcode, s);
    s.pc += 2;
}

// interpreter

pub fn step(state: &mut State) -> bool {
    let opcode = state.mem[state.pc];

    if opcode == 99 {
        false
    } else {
        match opcode % 100 {
            1 => binop(opcode, state, |a, b| a + b),
            2 => binop(opcode, state, |a, b| a * b),
            3 => {
                // stop if we need input and don't have it yet
                if state.input.is_empty() {
                    return false;
                }
                read(opcode, state)
            }
            4 => write(opcode, state),
            5 => jump_cond(opcode, state, |i| i != 0),
            6 => jump_cond(opcode, state, |i| i == 0),
            7 => binop(opcode, state, |a, b| if a < b { 1 } else { 0 }),
            8 => binop(opcode, state, |a, b| if a == b { 1 } else { 0 }),
            9 => adj_rel_base(opcode, state),
            _ => panic!("Bad instruction at {}: {}", state.pc, opcode),
        }

        true
    }
}

pub fn execute(mem: Vec<i64>, input: Vec<i64>) -> State {
    let mut state = State {
        pc: 0,
        rel_base: 0,
        mem,
        extended_mem: HashMap::new(),
        input: VecDeque::from(input),
        output: VecDeque::new(),
    };

    while step(&mut state) {}

    state
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_math() {
        assert_eq!(
            execute(vec![1, 0, 0, 0, 99], vec![]).mem,
            vec![2, 0, 0, 0, 99]
        );
        assert_eq!(
            execute(vec![2, 3, 0, 3, 99], vec![]).mem,
            vec![2, 3, 0, 6, 99]
        );
        assert_eq!(
            execute(vec![1, 1, 1, 4, 99, 5, 6, 0, 99], vec![]).mem,
            vec![30, 1, 1, 4, 2, 5, 6, 0, 99],
        );
    }

    #[test]
    fn test_io() {
        assert_eq!(execute(vec![3, 0, 99], vec![7]).mem, vec![7, 0, 99]);
        assert_eq!(execute(vec![4, 3, 99, 9], vec![]).output, vec![9]);
    }

    #[test]
    fn test_mode() {
        assert_eq!(
            execute(vec![1002, 4, 3, 4, 33], vec![]).mem,
            vec![1002, 4, 3, 4, 99]
        );
    }

    #[test]
    fn test_jump() {
        assert_eq!(
            execute(
                vec![3, 12, 6, 12, 15, 1, 13, 14, 13, 4, 13, 99, -1, 0, 1, 9],
                vec![0]
            )
            .output,
            vec![0]
        );
        assert_eq!(
            execute(
                vec![3, 3, 1105, -1, 9, 1101, 0, 0, 12, 4, 12, 99, 1],
                vec![0]
            )
            .output,
            vec![0]
        );
    }

    #[test]
    fn test_compare() {
        assert_eq!(
            execute(vec![3, 9, 8, 9, 10, 9, 4, 9, 99, -1, 8], vec![8]).output,
            vec![1]
        );
        assert_eq!(
            execute(vec![3, 9, 8, 9, 10, 9, 4, 9, 99, -1, 8], vec![7]).output,
            vec![0]
        );
        assert_eq!(
            execute(vec![3, 9, 7, 9, 10, 9, 4, 9, 99, -1, 8], vec![8]).output,
            vec![0]
        );
        assert_eq!(
            execute(vec![3, 9, 7, 9, 10, 9, 4, 9, 99, -1, 8], vec![-1]).output,
            vec![1]
        );
    }

    #[test]
    fn test_rel_base() {
        // set rel_base to 3
        // write output from x
        // end
        assert_eq!(
            execute(vec![109, 3, 204, 2, 99, 42], vec![]).output,
            vec![42]
        );
    }

    #[test]
    fn test_sparse_mem() {
        assert_eq!(
            execute(
                vec![1101, 42, 0, 100_000_000_000_000, 4, 100_000_000_000_000, 99],
                vec![]
            )
            .output,
            vec![42]
        );
        assert_eq!(execute(vec![4, 85939, 99], vec![]).output, vec![0]);
    }

    #[test]
    fn test_large_values() {
        assert_eq!(
            execute(vec![104, 42_000_000_000_000_000, 99], vec![]).output,
            vec![42_000_000_000_000_000]
        );
    }
}
