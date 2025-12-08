mod run;

use std::{path::{Path, PathBuf}, process::ExitCode};

fn main() -> Result<(), ExitCode> {
    let mut dir = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    dir.pop();
    dir.push("ticc-tests");
    dir.push("programs");
    dir.push("run-heavy");
    dir.push(test_name()?);
    println!("test dir: {}", dir.display());
    if !dir.exists() {
        eprintln!("test dir does not exist, creating");
        create_scaffold(&dir).map_err(|e| {
            eprintln!("failed to create template: {e}");
            ExitCode::FAILURE
        })?;
        return Ok(());
    }

    for (i, input) in ["input/test", "input/1"].iter().enumerate() {
        eprintln!("running on {input}");
        run::run(run::Opt {
            input: dir.join("main.tic"),
            output: None,
            check: false,
            emit_ir: false,
            eval: false,
            run: Some(dir.join(input)),
            runner: Some(ticc::Runner::Bytecode),
            optimize: true,
            no_verify: false,
            stack: None,
            time: true,
            warn: i == 0,
        });
    }

    Ok(())
}

fn create_scaffold(dir: &Path) -> Result<(), std::io::Error> {
    std::fs::create_dir(dir)?;
    std::fs::create_dir(dir.join("input"))?;
    std::fs::write(dir.join("main.tic"), TEMPLATE)?;
    std::fs::write(dir.join("input/1"), "todo: paste input")?;
    std::fs::write(dir.join("input/test"), "todo: paste test input")?;
    Ok(())
}

fn test_name() -> Result<String, ExitCode> {
    let args = std::env::args().collect::<Vec<_>>();
    match args.as_slice() {
        [_, year, day] => {
            if year.len() != 4 || !year.starts_with("20") {
                eprintln!("bad year: {year:?}");
                Err(ExitCode::FAILURE)
            } else if day.len() != 2 {
                eprintln!("bad day: {day:?}");
                Err(ExitCode::FAILURE)
            } else {
                Ok(format!("aoc{year}-day{day}"))
            }
        }
        _ => {
            eprintln!("usage: cargo run -p aoc -- year day");
            Err(ExitCode::FAILURE)
        }
    }
}

const TEMPLATE: &[u8] =
br#"import ints from "std/int";
import option(Option, Some, None) from "std/option";
import strings from "std/string";
import list(List, Nil, Cons) from "std/list";
import result(Result, Ok, Err) from "std/result";
import array(Array) from "std/array";
import utils from "../shared/aoc-utils.tic";
import parse from "../shared/parse.tic";

let parseInput : string -> Result inputType string =
    \input ->
        ?todoParseInput;

let part1 : inputType -> string =
    \x ->
        "todo";

let part2 : inputType -> string =
    \x ->
        "todo";

export let main : string -> string = utils.makeSolution parseInput part1 part2;
"#;
