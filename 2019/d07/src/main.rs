use permutohedron::Heap;
use std::collections::VecDeque;
use std::iter;
use std::iter::FromIterator;

mod intcode;
use intcode::*;


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
