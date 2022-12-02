use fnv::FnvHashSet;
use std::fs::File;
use std::io::{prelude::*, BufReader};
use std::time::Instant;

const INPUT_FILE: &str = "input.txt";

fn part_one() -> i32 {
    let f = File::open(INPUT_FILE).unwrap();
    let reader = BufReader::new(f);

    reader
        .lines()
        .map(|line| -> i32 { line.unwrap().parse().unwrap() })
        .sum()
}

fn part_two() -> Option<i32> {
    let f = File::open(INPUT_FILE).unwrap();
    let reader = BufReader::new(f);
    let nums: Vec<i32> = reader
        .lines()
        .map(|line| -> i32 { line.unwrap().parse().unwrap() })
        .collect();

    let mut prev_sum = 0;
    let mut prev_sums = FnvHashSet::default();

    for n in nums.iter().cycle() {
        let sum = prev_sum + n;

        if !prev_sums.insert(sum) {
            return Some(sum);
        } else {
            prev_sum = sum;
        }
    }

    None
}

fn time<F>(fun: F)
where
    F: FnOnce() -> (),
{
    let now = Instant::now();
    fun();
    let duration = now.elapsed();
    println!(
        "Execution Time: {}.{:06}µs",
        duration.as_secs(),
        duration.subsec_micros()
    );
}

fn main() {
    time(|| println!("Part One: {}", part_one()));
    time(|| println!("Part Two: {}", part_two().unwrap()));
}
