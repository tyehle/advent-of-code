use std::fs;


/// Fuel for a given mass
fn fuel_for(mass: i32) -> i32 {
    mass / 3 - 2
}


/// Total fuel for a module
fn module_fuel(mass: i32) -> i32 {
    let mut total = 0;
    let mut current = fuel_for(mass);
    while current > 0 {
        total += current;
        current = fuel_for(current);
    }
    total
}


/// Read the puzzle input
fn parse_input(path: &str) -> Vec<i32> {
    fs::read_to_string(path)
        .unwrap()
        .lines()
        .map(|l| l.parse().unwrap())
        .collect()
}


fn main() {
    let input = parse_input("input.txt");
    println!("{}", input.iter().map(|m| fuel_for(*m)).sum::<i32>());
    println!("{}", input.iter().map(|m| module_fuel(*m)).sum::<i32>());
}


#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_module_fuel() {
        assert_eq!(2, module_fuel(14));
        assert_eq!(966, module_fuel(1969));
        assert_eq!(50346, module_fuel(100756));
    }
}
