use num_complex::Complex;
use std::collections::{HashMap, HashSet};
use std::io::{Read, Write};
use std::iter::FromIterator;

mod intcode;
use intcode::*;

fn read_tile(prog: &mut State) -> Option<(Complex<i64>, i64)> {
    let x = prog.pop_lazy()?;
    let y = prog.pop_lazy()?;
    let value = prog.pop_lazy()?;

    Some((Complex::new(x, y), value))
}

fn init_game(prog: &mut State, tiles: &mut HashMap<Complex<i64>, i64>) {
    while let Some((k, v)) = read_tile(prog) {
        tiles.insert(k, v);
    }
}

fn p1(input: Vec<i64>) -> usize {
    let mut prog = State::new(input);
    let mut tiles = HashMap::new();

    init_game(&mut prog, &mut tiles);

    println!(
        "min: {:?}, max: {:?}",
        tiles.keys().min_by_key(|c| c.l1_norm()),
        tiles.keys().max_by_key(|c| c.l1_norm())
    );

    tiles.values().filter(|v| **v == 2).count()
}

fn get_control() -> i64 {
    let mut chars = std::io::stdin().bytes();
    let mut out = 0;

    loop {
        match chars.next() {
            Some(Ok(97)) => out = -1,
            Some(Ok(100)) => out = 1,
            Some(Ok(10)) => break,
            Some(Ok(_)) => (),
            bad => panic!("{:?}", bad),
        }
    }

    print!("{}", ansi_escapes::CursorUp(1));
    std::io::stdout().flush().expect("Could not flush stdout");

    out
}

fn p2(input: Vec<i64>) {
    let mut prog = State::new(input);
    let mut tiles = HashMap::new();
    let score_slot = Complex::new(-1, 0);

    prog.set(0, 2);

    init_game(&mut prog, &mut tiles);
    draw_all(&tiles);

    let (paddle_x, ball_x) = find_mem_locs(&mut prog);

    let mut prev_ball_x = prog.get(ball_x);

    prog.execute();
    while !prog.is_done() {
        // get_control();

        prog.push(-prog.get(ball_x) + prev_ball_x);
        prog.set(paddle_x, 2 * prog.get(ball_x) - prev_ball_x);
        // prog.push(prog.get(ball_x) - prev_ball_x);
        // prog.set(paddle_x, prog.get(ball_x));

        prev_ball_x = prog.get(ball_x);

        // draw all updates
        let mut update = read_tile(&mut prog);
        while update.is_some() {
            let (k, v) = update.unwrap();
            tiles.insert(k, v);
            if k != score_slot {
                draw_at(tile_glyph(v), 1, &k);
            }
            update = read_tile(&mut prog);
        }

        prog.execute();
    }

    println!("score: {:?}", tiles.get(&score_slot));
}

fn find_mem_locs(prog: &mut State) -> (usize, usize) {
    fn mem_diff(control: i64, prog: &mut State) -> HashMap<usize, (i64, i64)> {
        prog.execute();
        let mem1 = prog.mem.clone();

        prog.push(control);

        prog.execute();
        let mem2 = prog.mem.clone();

        let mut changes = HashMap::new();
        for (k, v) in mem1.iter() {
            if mem2.get(k) != Some(v) {
                changes.insert(*k, (*mem1.get(k).unwrap_or(&0), *mem2.get(k).unwrap_or(&0)));
            }
        }

        changes
    }

    // fn print_diff(diff: &HashMap<usize, (i64, i64)>) {
    //     for (k, (v0, v1)) in diff.iter() {
    //         if (v0 - v1).abs() == 1 {
    //             println!("{}:  {}  ->  {}", k, v0, v1);
    //         }
    //     }
    // }

    fn change(diff: Option<&(i64, i64)>) -> i64 {
        diff.map(|(a, b)| b - a).unwrap_or(0)
    }

    // println!("\nLeft:");
    let left = mem_diff(-1, prog);
    // print_diff(&left);

    // println!("\nRight:");
    let right = mem_diff(1, prog);
    // print_diff(&right);

    // println!("\nStill:");
    let still = mem_diff(0, prog);
    // print_diff(&still);

    let all_keys: HashSet<usize> =
        HashSet::from_iter(left.keys().chain(right.keys()).chain(still.keys()).copied());

    let paddle_locs: Vec<usize> = all_keys
        .iter()
        .filter(|p| {
            change(left.get(p)) == -1 && change(right.get(p)) == 1 && !still.contains_key(p)
        })
        .copied()
        .collect();

    let ball_locs: Vec<usize> = all_keys
        .iter()
        .filter(|p| {
            change(left.get(p)) == 1 && change(right.get(p)) == 1 && change(still.get(p)) == 1
        })
        .copied()
        .collect();

    (
        match paddle_locs[..] {
            [v] => v,
            _ => panic!("Bad paddle loc: {:?}", paddle_locs),
        },
        match ball_locs[..] {
            [v] => v,
            _ => panic!("Bad ball loc: {:?}", ball_locs),
        },
    )
}

fn tile_glyph(value: i64) -> &'static str {
    match value {
        0 => "\x1B[90;40m \x1B[0m",
        1 => "\x1B[90;47m \x1B[0m",
        2 => "\x1B[95;40mx\x1B[0m",
        3 => "\x1B[93;40m-\x1B[0m",
        4 => "\x1B[96;40mo\x1B[0m",
        _ => "\x1B[91;40m!\x1B[0m",
    }
}

fn draw_at(what: &str, width: i16, pos: &Complex<i64>) {
    let to_pos = pos - Complex::new(0, 26);
    print!(
        "{}{}{}",
        // move the cursor there
        ansi_escapes::CursorMove::XY(to_pos.re as i16, to_pos.im as i16),
        // draw the thing
        what,
        // replace the cursor
        ansi_escapes::CursorMove::XY(-to_pos.re as i16 - width, -to_pos.im as i16)
    );

    std::io::stdout().flush().expect("Could not flush stdout");
}

fn draw_all(tiles: &HashMap<Complex<i64>, i64>) {
    for y in 0..=25 {
        for x in 0..=39 {
            print!(
                "{}",
                tile_glyph(*tiles.get(&Complex::new(x, y)).unwrap_or(&0))
            );
        }
        println!("{}", ansi_escapes::CursorBackward(40));
    }

    print!("\n{}", ansi_escapes::CursorUp(1));

    std::io::stdout().flush().expect("Could not flush stdout");
}

fn main() {
    let input = parse();
    println!("{}", p1(input.clone()));
    p2(input.clone());
}
