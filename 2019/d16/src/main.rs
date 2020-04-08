use std::collections::HashMap;
use std::fs;

fn parse_input() -> Vec<i32> {
    parse(&fs::read_to_string("input.txt").unwrap())
}

fn parse(input: &str) -> Vec<i32> {
    input
        .trim()
        .chars()
        .map(|c| c.to_digit(10).unwrap() as i32)
        .collect()
}

fn truncate(n: i32) -> i32 {
    if n < 0 {
        -n % 10
    } else {
        n % 10
    }
}

fn index(input_digit: usize, output_digit: usize, pattern: &[i32]) -> i32 {
    pattern[((input_digit + 1) / (output_digit + 1)) % pattern.len()]
}

fn apply_pattern_cache(
    input: &[i32],
    pattern: &[i32],
    offset: usize,
    differences: &[HashMap<usize, i32>],
) -> Vec<i32> {
    let mut output = Vec::new();

    for output_digit in 0..input.len() {
        let digit = if output_digit > 0 && output_digit <= differences.len() {
            let map = &differences[output_digit - 1];
            output[output_digit - 1]
                + map
                    .iter()
                    .map(|(&index, &factor)| input[index-offset] * factor)
                    .sum::<i32>()
        } else {
            input
                .iter()
                .enumerate()
                .map(|(input_digit, n)| {
                    index(input_digit + offset, output_digit + offset, pattern) * n
                })
                .sum::<i32>()
        };

        output.push(digit);
    }

    for i in 0..output.len() {
        output[i] = truncate(output[i]);
    }

    output
}

fn reapeat_pattern(n: usize, input: &[i32], pattern: &[i32], offset: usize) -> Vec<i32> {
    let differences = (0..input.len())
        .map(|i| row_difference(i+offset, input.len()+offset, pattern))
        .collect::<Vec<_>>();

    let mut state = Vec::from(input);
    for _ in 0..n {
        state = apply_pattern_cache(&state, pattern, offset, &differences);
    }
    state
}

fn row_difference(from: usize, length: usize, pattern: &[i32]) -> HashMap<usize, i32> {
    let mut result = HashMap::new();

    // Mad hacks
    if from >= length / 2 {
        result.insert(from, -1);
    } else {
        for i in from..length {
            let difference = index(i, from + 1, pattern) - index(i, from, pattern);
            if difference != 0 {
                result.insert(i, difference);
            }
        }
    }

    result
}

fn main() {
    let input = parse_input();
    let pattern = vec![0, 1, 0, -1];

    let offset: usize = input[..7]
        .iter()
        .rev()
        .enumerate()
        .map(|(i, &n)| (n as usize) * 10usize.pow(i as u32))
        .sum();
    println!("offset: {}", offset);
    let biginput = &([&input[..]; 10000].concat())[offset..];
    println!("big input size: {}", biginput.len());

    println!(
        "{:?}",
        reapeat_pattern(100, &input, &pattern, 0)[..8]
            .iter()
            .collect::<Vec<_>>()
    );
    println!(
        "{:?}",
        reapeat_pattern(100, biginput, &pattern, offset)[..8]
            .iter()
            .collect::<Vec<_>>()
    );
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_parse() {
        assert_eq!(parse("1234"), vec![1, 2, 3, 4]);
    }

    #[test]
    fn test_truncate() {
        assert_eq!(truncate(0), 0);
        assert_eq!(truncate(1), 1);
        assert_eq!(truncate(12), 2);
        assert_eq!(truncate(1245), 5);
        assert_eq!(truncate(-13), 3);
    }

    #[test]
    fn test_index() {
        assert_eq!(index(0, 0, &vec![1, 2, 3, 4]), 2);
        assert_eq!(index(5, 0, &vec![1, 2, 3, 4]), 3);
        assert_eq!(index(0, 1, &vec![1, 2, 3, 4]), 1);
        assert_eq!(index(4, 1, &vec![1, 2, 3, 4]), 3);
        assert_eq!(index(5, 1, &vec![1, 2, 3, 4]), 4);
    }

    #[test]
    fn test_apply_pattern() {
        let pattern = vec![0, 1, 0, -1];
        assert_eq!(
            apply_pattern_cache(&parse("12345678"), &pattern, 0, &[]),
            parse("48226158")
        );
        assert_eq!(
            apply_pattern_cache(&parse("2345678"), &pattern, 1, &[]),
            parse("8226158")
        );
    }

    #[test]
    fn test_repeat_pattern() {
        let pattern = vec![0, 1, 0, -1];
        assert_eq!(
            reapeat_pattern(1, &parse("12345678"), &pattern, 0),
            parse("48226158")
        );
        assert_eq!(
            reapeat_pattern(1, &parse("2345678"), &pattern, 1),
            parse("8226158")
        );
        assert_eq!(
            reapeat_pattern(2, &parse("12345678"), &pattern, 0),
            parse("34040438")
        );
        assert_eq!(
            reapeat_pattern(2, &parse("2345678"), &pattern, 1),
            parse("4040438")
        );
        assert_eq!(
            reapeat_pattern(4, &parse("12345678"), &pattern, 0),
            parse("01029498")
        );

        assert_eq!(
            reapeat_pattern(100, &parse("19617804207202209144916044189917"), &pattern, 0)[..8],
            parse("73745418")[..8]
        );
    }

    #[test]
    fn test_concat() {
        let input = parse("123");
        assert_eq!([&input[..]; 3].concat(), vec![1, 2, 3, 1, 2, 3, 1, 2, 3]);
    }

    #[test]
    fn test_row_difference() {
        use std::iter::FromIterator;

        let pattern = vec![0, 1, 0, -1];

        assert_eq!(
            row_difference(0, 4, &pattern),
            HashMap::from_iter([(0, -1), (1, 1), (2, 2)].iter().cloned())
        );
        assert_eq!(
            row_difference(4, 8, &pattern),
            HashMap::from_iter([(4, -1)].iter().cloned())
        );
    }
}
