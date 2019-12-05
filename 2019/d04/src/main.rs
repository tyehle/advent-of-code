

fn is_valid(pw: Vec<i32>) -> bool {
    let mut double = false;
    let mut prev = pw[0];

    for i in 1..pw.len() {
        if pw[i] < prev {
            return false;
        }

        if pw[i] == prev {
            double = true;
        }

        prev = pw[i];
    }

    double
}

fn is_valid_b(pw: Vec<i32>) -> bool {
    let mut run = 0;
    let mut double = false;
    let mut prev = pw[0];

    for i in 1..pw.len() {
        if pw[i] < prev {
            return false;
        }

        if pw[i] == prev {
            run += 1;
        } else {
            if run == 1 {
                double = true;
            }
            run = 0;
        }

        prev = pw[i];
    }

    double || run == 1
}

fn digits(n: i32) -> Vec<i32> {
    n.to_string().chars().map(|c| c.to_string().parse().unwrap()).collect()
}

fn brute_count_a(low: i32, high: i32) -> i32 {
    let mut count = 0;

    for i in low..high {
        if is_valid(digits(i)) {
            count += 1;
        }
    }

    count
}

fn brute_count_b(low: i32, high: i32) -> i32 {
    let mut count = 0;

    for i in low..high {
        if is_valid_b(digits(i)) {
            count += 1;
        }
    }

    count
}

fn main() {
    println!("{}", brute_count_a(153517, 630395));
    println!("{}", brute_count_b(153517, 630395));
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_b_valid() {
        assert!(is_valid_b(digits(111122)));
        assert!(!is_valid_b(digits(123444)));
    }
}
