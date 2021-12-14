/** Functional approach to AoC2021 D1 in Rust (by a Haskell developer).
 *
 * Haskell solutions for task 1 and 2 respectively:
 *
 *     count . map (uncurry (<)) . pairs . map sum . windows 3 . parse @Int
 *     count . map (uncurry (<)) . pairs .                       parse @Int
 *
 * where
 *
 *   * count is a simple fold to count the number of Trues in a list
 *   * parse is `map tread . Text.lines`
 *   * tread is `read` using Text instead of String (input uses Text)
 */

//const EXAMPLE_INPUT: &[u32] = &[199, 200, 208, 210, 200, 207, 240, 269, 260, 263];

use std::io::Result;

fn main() -> Result<()> {
    let args: Vec<String> = std::env::args().collect();
    let file = &args[1];
    let data = parse(file)?;
    let result = solve(3, data.as_slice());
    println!("{}", result);
    Ok(())
}

// basically an explicit error-handled `map (tread @u32) . Text.lines`
fn parse(file: &str) -> Result<Vec<u32>> {
    let data = std::fs::read_to_string(file)?;
    let mut parsed: Vec<u32> = Vec::new();
    for line in data.lines() {
        parsed.push(line.parse::<u32>().unwrap());
    }
    Ok(parsed)
}

// Solves for both tasks: window size 1 for task 1 (does nothing), 3 for task 2.
fn solve(size: usize, xs: &[u32]) -> u32 {
    xs
        .windows(size)              // windows x
        .map(|x| x.iter().sum())    // map sum
        .collect::<Vec<u32>>()      // explicitly run map (type annotation required)
        .as_slice()                 // explicitly extract type to work on
        .windows(2)                 // windows 2
        .map(|x| x[0] < x[1])       // map (uncurry (<))
        .collect::<Vec<bool>>()     // explicitly run map
        .iter()                     // explicitly extract type to work on
        .fold(0, |acc, x| if *x { acc + 1 } else { acc }) // fold-based count
}

/** Post thoughts.
 *
 * Minor, but I wasn't able to stay as polymorphic with Rust as I was in
 * Haskell, I kept getting errors. It's probably possible, I'm just not a Rust
 * developer. So I concretize early to `u32`.
 *
 * rustc complained to me that `collect()` requires an explicit type annotation.
 * Also, map operations require an explicit "collect" step, which I assumed is
 * like executing the map.
 */
