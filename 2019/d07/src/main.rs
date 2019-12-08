use permutohedron::Heap;
use std::collections::VecDeque;
use std::convert::{TryFrom, TryInto};
use std::fs;
use std::iter;
use std::iter::FromIterator;

#[derive(Debug)]
struct State {
    pc: usize,
    mem: Vec<i32>,
    input: VecDeque<i32>,
    output: VecDeque<i32>,
}

fn parse_program(s: String) -> Vec<i32> {
    s.trim().split(',').map(|i| i.parse().unwrap()).collect()
}

fn parse() -> Vec<i32> {
    parse_program(fs::read_to_string("input.txt").unwrap())
}

fn param_mode(position: usize, opcode: i32) -> i32 {
    opcode / 10i32.pow(u32::try_from(position).unwrap() + 2) % 10
}

fn get_param(position: usize, opcode: i32, s: &mut State) -> i32 {
    let immediate = s.mem[s.pc + position + 1];
    match param_mode(position, opcode) {
        0 => s.mem[usize::try_from(immediate).unwrap()],
        1 => immediate,
        _ => panic!("Unknown parameter mode at {}: {}", s.pc, opcode),
    }
}

fn store_value(position: usize, opcode: i32, s: &mut State, value: i32) {
    match param_mode(position, opcode) {
        0 => {
            let loc = s.mem[s.pc + position + 1];
            s.mem[usize::try_from(loc).unwrap()] = value
        }
        _ => panic!("Bad store mode at {}: {}", s.pc, opcode),
    }
}

// instructions

fn binop(opcode: i32, s: &mut State, op: impl Fn(i32, i32) -> i32) {
    // println!("binop {:?}", &s.mem[s.pc..s.pc + 4]);
    let result = op(get_param(0, opcode, s), get_param(1, opcode, s));
    store_value(2, opcode, s, result);
    s.pc += 4;
}

fn read(opcode: i32, s: &mut State) {
    // println!("read  {:?}", &s.mem[s.pc..s.pc + 2]);
    let result = s.input.pop_front().unwrap();
    store_value(0, opcode, s, result);
    s.pc += 2;
}

fn write(opcode: i32, s: &mut State) {
    // println!("write {:?}", &s.mem[s.pc..s.pc + 2]);
    let value = get_param(0, opcode, s);
    s.output.push_back(value);
    s.pc += 2;
}

fn jump_cond(opcode: i32, s: &mut State, cond: impl Fn(i32) -> bool) {
    // println!("c_jmp {:?}", &s.mem[s.pc..s.pc + 3]);
    if cond(get_param(0, opcode, s)) {
        s.pc = get_param(1, opcode, s).try_into().unwrap();
    } else {
        s.pc += 3;
    }
}

// interpreter

fn step(state: &mut State) -> bool {
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
            _ => panic!("Bad instruction at {}: {}", state.pc, opcode),
        }

        true
    }
}

fn execute(mem: Vec<i32>, input: Vec<i32>) -> State {
    let mut state = State {
        pc: 0,
        mem,
        input: VecDeque::from(input),
        output: VecDeque::new(),
    };

    while step(&mut state) {}

    state
}

fn amp_sequence(prog: &[i32], phases: &[i32]) -> i32 {
    let mut power = 0;
    let mut amps: Vec<State> = phases
        .iter()
        .map(|phase| State {
            pc: 0,
            mem: Vec::from(prog),
            input: VecDeque::from_iter(iter::once(*phase)),
            output: VecDeque::new(),
        })
        .collect();

    while amps[0].mem[amps[0].pc] != 99 {
        for mut amp in &mut amps {
            amp.input.push_back(power);
            while step(&mut amp) {}
            power = amp.output.pop_front().unwrap();
        }
    }

    // for phase in phases {
    //     power = execute(prog.clone(), vec![power, *phase]).output[0];
    // }

    power
}

fn best_sequence(prog: &[i32], phases: &[i32]) -> Vec<i32> {
    Heap::new(&mut Vec::from(phases))
        .max_by_key(|phases| amp_sequence(prog, phases))
        .unwrap()
}

fn main() {
    let input = parse();
    println!(
        "{:?}",
        amp_sequence(&input, &best_sequence(&input, &(0..5).collect::<Vec<_>>()))
    );
    println!(
        "{:?}",
        amp_sequence(&input, &best_sequence(&input, &(5..10).collect::<Vec<_>>()))
    );
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
    fn test_amp() {
        assert_eq!(
            amp_sequence(
                &vec![3, 15, 3, 16, 1002, 16, 10, 16, 1, 16, 15, 15, 4, 15, 99, 0, 0],
                &vec![4, 3, 2, 1, 0]
            ),
            43210
        );
    }
}
