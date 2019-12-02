use std::fs;
use std::collections::HashMap;

fn parse() -> Vec<usize> {
    fs::read_to_string("input.txt")
        .unwrap()
        .trim()
        .split(",")
        .map(|i| i.parse().unwrap())
        .collect()
}

fn build_dispatch_table() -> HashMap<usize, impl Fn(&mut usize, &mut Vec<usize>) -> ()> {
    fn binop(op: fn(usize, usize) -> usize) -> impl Fn(&mut usize, &mut Vec<usize>) -> () {
        move |pc, mem| {
            let a = mem[mem[*pc + 1]];
            let b = mem[mem[*pc + 2]];
            let store = mem[*pc + 3];

            mem[store] = op(a, b);

            *pc += 4;
        }
    }

    let mut instructions = HashMap::<usize, _>::new();
    instructions.insert(1, binop(|a, b| a+b));
    instructions.insert(2, binop(|a, b| a*b));

    instructions
}

fn step(ops: &HashMap<usize, impl Fn(&mut usize, &mut Vec<usize>) -> ()>, pc: &mut usize, mem: &mut Vec<usize>) -> bool {
    let opcode = mem[*pc];

    if opcode == 99 {
        false
    } else {
        match ops.get(&opcode) {
            Some(op) => op(pc, mem),
            None => panic!("Bad instruction at {}: {}", pc, opcode),
        }
        true
    }
}

fn execute(mem: &mut Vec<usize>) {
    let instructions = build_dispatch_table();

    let mut pc = 0;
    while step(&instructions, &mut pc, mem) {}
}

fn execute_params(a: usize, b: usize, prog: &Vec<usize>) -> Vec<usize> {
    let mut mem = prog.clone();
    mem[1] = a;
    mem[2] = b;
    execute(&mut mem);
    mem
}

fn find_inputs(target: usize, prog: &Vec<usize>) -> Option<(usize, usize)> {
    for a in 0..99 {
        for b in 0..99 {
            if execute_params(a, b, prog)[0] == target {
                return Some((a, b));
            }
        }
    }

    None
}

fn main() {
    let input = parse();

    println!("{}", execute_params(12, 2, &input)[0]);

    match find_inputs(19690720, &input) {
        Some((a, b)) => println!("a: {}, b: {}, 100a+b: {}", a, b, 100 * a + b),
        None => println!("No result"),
    }
}

#[cfg(test)]
mod test {
    use super::*;

    fn check(prog: Vec<usize>, result: Vec<usize>) {
        let mut mem = prog.clone();
        execute(&mut mem);
        assert_eq!(mem, result);
    }

    #[test]
    fn test_thing() {
        check(vec![1, 0, 0, 0, 99], vec![2, 0, 0, 0, 99]);
        check(vec![2, 3, 0, 3, 99], vec![2, 3, 0, 6, 99]);
        check(
            vec![1, 1, 1, 4, 99, 5, 6, 0, 99],
            vec![30, 1, 1, 4, 2, 5, 6, 0, 99],
        );
    }
}
