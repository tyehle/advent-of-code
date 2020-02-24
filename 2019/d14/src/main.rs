use std::collections::HashMap;
use std::fs;


fn parse_input() -> HashMap<String, (i64, Vec<(i64, String)>)> {
    parse(&fs::read_to_string("input.txt").unwrap())
}

fn parse(input: &str) -> HashMap<String, (i64, Vec<(i64, String)>)> {
    input.trim().lines().map(parse_reaction).collect()
}

fn parse_reaction(line: &str) -> (String, (i64, Vec<(i64, String)>)) {
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

fn parse_chemical(raw: &str) -> (i64, String) {
    match raw.trim().split(' ').collect::<Vec<_>>()[..] {
        [num, name] => (num.parse().unwrap(), name.to_string()),
        _ => panic!(""),
    }
}

fn p1(rules: &HashMap<String, (i64, Vec<(i64, String)>)>, fuel: i64) -> i64 {
    let mut required: HashMap<&str, i64> = HashMap::new();
    let mut extra: HashMap<&str, i64> = HashMap::new();
    required.insert("FUEL", fuel);

    fn not_done(p: (&&str, &i64)) -> bool {
        p.1 > &0 && *p.0 != "ORE"
    }

    loop {
        let name = match required.iter().find(|p| not_done(*p)) {
            None => break,
            Some((name, _)) => *name,
        };

        // Check if we have what we need in `extra`
        let amount_in_extra = extra.get(name).unwrap_or(&0);
        let reduced_amount = amount_in_extra - required[name];
        if reduced_amount >= 0 {
            // Don't run rule, take from extra
            extra.insert(name, reduced_amount);
        } else {
            // Use all from extra, and run modified rule
            let amount_needed = -reduced_amount;
            let (produced_amount, inputs) = &rules[name];
            let times = amount_needed / produced_amount + if amount_needed % produced_amount == 0 {0} else {1};
            // put inputs into required
            extra.insert(name, produced_amount * times - amount_needed);
            // put any extra into extra
            for (amount, name) in inputs {
                let value = required.entry(name).or_insert(0);
                *value += amount * times;
            }
        }
        required.remove(name);
    }

    required["ORE"]
}

fn hackx(ore: i64, rules: &HashMap<String, (i64, Vec<(i64, String)>)>) -> i64 {
    let mut lower_bound = 1;
    let mut upper_bound = 0;
    let mut guess = 2;

    while upper_bound == 0 {
        if p1(rules, guess) > ore {
            upper_bound = guess;
            break;
        } else {
            lower_bound = guess;
            guess *= 2;
        }
    }

    while upper_bound - lower_bound > 1 {
        guess = (lower_bound + upper_bound) / 2;
        if p1(rules, guess) > ore {
            upper_bound = guess;
        } else {
            lower_bound = guess;
        }
    }

    lower_bound
}

fn main() {
    let input = parse_input();
    println!("{}", p1(&input, 1));
    println!("{}", hackx(1_000_000_000_000, &input));
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
