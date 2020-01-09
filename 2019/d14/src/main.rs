use std::collections::HashMap;
use std::fs;


fn parse_input() -> HashMap<String, (i32, Vec<(i32, String)>)> {
    parse(&fs::read_to_string("input.txt").unwrap())
}

fn parse(input: &str) -> HashMap<String, (i32, Vec<(i32, String)>)> {
    input.trim().lines().map(parse_reaction).collect()
}

fn parse_reaction(line: &str) -> (String, (i32, Vec<(i32, String)>)) {
    match line.split(" => ").collect::<Vec<_>>()[..] {
        [ins, output] => {
            let (amount, name) = parse_chemical(output);
            (
                name,
                (amount, ins.split(", ").map(parse_chemical).collect())
            )
        },
        _ => panic!(""),
    }
}

fn parse_chemical(raw: &str) -> (i32, String) {
    match raw.trim().split(' ').collect::<Vec<_>>()[..] {
        [num, name] => (num.parse().unwrap(), name.to_string()),
        _ => panic!(""),
    }
}

fn p1(rules: &HashMap<String, (i32, Vec<(i32, String)>)>) {
    let mut required: HashMap<String, i32> = HashMap::new();
    let mut extra: HashMap<String, i32> = HashMap::new();
    required.insert("FUEL".to_string(), 1);

    fn not_done(p: (&String, &i32)) -> bool {
        p.1 > &0 && p.0 != "ORE"
    }

    while let Some((name, required_amount)) = required.iter().find(|p| not_done(*p)) {
        let (produced_amount, inputs) = &rules[name];
        let times = (required_amount - 1) / produced_amount + 1;
        let extra_amount = produced_amount * times - required_amount;
    }

    unimplemented!();
}

fn main() {
    let input = parse_input();
    p1(&input)
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_parse_chemical() {
        assert_eq!(parse_chemical("12 AB"), (12, "AB".to_string()));
        assert_eq!(parse_chemical(" 1 ASDF "), (1, "ASDF".to_string()));
    }

    #[test]
    fn test_parse_reaction() {
        assert_eq!(
            parse_reaction("8 A, 9 B => 3 C"),
            (
                (3, "C".to_string()),
                vec![(8, "A".to_string()), (9, "B".to_string())],
            )
        );
    }
}
