fn is_valid(pw: &[u8]) -> bool {
    let mut double = false;
    let mut prev = pw[0];

    for d in pw.iter().skip(1) {
        if *d < prev {
            return false;
        }

        if *d == prev {
            double = true;
        }

        prev = *d;
    }

    double
}

fn is_valid_b(pw: &[u8]) -> bool {
    let mut run = 0;
    let mut double = false;
    let mut prev = pw[0];

    for d in pw.iter().skip(1) {
        if *d < prev {
            return false;
        }

        if *d == prev {
            run += 1;
        } else {
            if run == 1 {
                double = true;
            }
            run = 0;
        }

        prev = *d;
    }

    double || run == 1
}

fn digits(n: i32) -> Vec<u8> {
    n.to_string()
        .chars()
        .map(|c| c.to_string().parse().unwrap())
        .collect()
}

fn inc(ds: &mut Vec<u8>) {
    let mut pos = ds.len() - 1;

    // look for the first digit that's not a 9
    while ds[pos] >= 9 {
        pos -= 1;
    }

    // set digit
    ds[pos] += 1;

    // set digits below
    for i in pos + 1..ds.len() {
        ds[i] = ds[i - 1];
    }
}

fn counter(low: i32, high: i32, predicate: impl Fn(&[u8]) -> bool) -> u32 {
    let mut ds = digits(low);
    let end = digits(high);
    let mut count = 0;

    loop {
        if predicate(&ds) {
            count += 1;
        }

        inc(&mut ds);

        // check end condition
        for i in 0..ds.len() {
            if end[i] > ds[i] {
                break;
            }

            if end[i] < ds[i] {
                return count;
            }
        }
    }
}

fn main() {
    println!("{}", counter(153_517, 630_395, is_valid));
    println!("{}", counter(153_517, 630_395, is_valid_b));
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_b_valid() {
        assert!(is_valid_b(&digits(111_122)));
        assert!(!is_valid_b(&digits(123_444)));
    }

    #[test]
    fn test_inc() {
        let mut ds = vec![3, 5, 5, 9, 9];
        inc(&mut ds);
        assert_eq!(ds, vec![3, 5, 6, 6, 6]);
    }
}
