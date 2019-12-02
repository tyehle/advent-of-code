use std::fs;

fn parse() -> Vec<usize> {
    fs::read_to_string("input.txt")
        .unwrap()
        .trim()
        .split(",")
        .map(|i| i.parse().unwrap())
        .collect()
}

fn execute(mem: &mut Vec<usize>) {
    let mut pc = 0;

    loop {
        if mem[pc] == 99 {
            return;
        }

        let a = mem[mem[pc + 1]];
        let b = mem[mem[pc + 2]];
        let store = mem[pc + 3];

        match mem[pc] {
            1 => mem[store] = a + b,
            2 => mem[store] = a * b,
            bad => println!("Bad instruction at {}: {}", pc, bad),
        }

        pc += 4
    }
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
                println!("a: {}, b: {}, 100a+b: {}", a, b, 100 * a + b);
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

    // find_inputs(19690720, &input);
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
