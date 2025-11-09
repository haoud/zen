use ariadne::Source;
use chumsky::prelude::*;

pub mod config;

fn main() {
    let matches = clap::command!("zen")
        .about("Zen programming language compiler")
        .subcommand_required(false)
        .subcommand(
            clap::Command::new("translate")
                .about("Translate a Zen project to an unity C file")
                .arg(clap::arg!([file] "The file to compile"))
                .arg(clap::arg!(-o --output <output> "The output file"))
                .arg(
                    clap::arg!(--"dump-tokens" "Dump the tokens to the console")
                        .action(clap::ArgAction::SetTrue),
                )
                .arg(
                    clap::arg!(--"dump-ast" "Dump the AST to the console")
                        .action(clap::ArgAction::SetTrue),
                )
                .arg(
                    clap::arg!(--"dump-ir" "Dump the intermediate representation to the console")
                        .action(clap::ArgAction::SetTrue),
                )
                .arg(
                    clap::arg!(--"trace-time" "Trace the time taken by each compilation phase")
                        .action(clap::ArgAction::SetTrue),
                ),
        )
        .arg(
            clap::arg!(--"dump-tokens" "Dump the tokens to the console")
                .action(clap::ArgAction::SetTrue),
        )
        .arg(
            clap::arg!(--"dump-ast" "Dump the AST to the console").action(clap::ArgAction::SetTrue),
        )
        .arg(
            clap::arg!(--"dump-ir" "Dump the intermediate representation to the console")
                .action(clap::ArgAction::SetTrue),
        )
        .arg(
            clap::arg!(--"trace-time" "Trace the time taken by each compilation phase")
                .action(clap::ArgAction::SetTrue),
        )
        .get_matches();

    // Dispatch the appropriate action based on the subcommand. If no subcommand
    // is provided, we default to the "translate" action.
    if let Some(args) = matches.subcommand_matches("translate") {
        translate(args);
    } else if matches.subcommand_name().is_none() {
        translate(&matches);
    }
}

fn translate(args: &clap::ArgMatches) {
    let default_output_name = String::from(config::DEFAULT_OUTPUT_C_NAME);
    let default_input_name = String::from(config::DEFAULT_INPUT_NAME);
    let output = args
        .try_get_one::<String>("output")
        .unwrap_or(Some(&default_output_name))
        .unwrap_or(&default_output_name);
    let path = args
        .try_get_one::<String>("file")
        .unwrap_or(Some(&default_input_name))
        .unwrap_or(&default_input_name);

    let program = match std::fs::read_to_string(path) {
        Ok(content) => content,
        Err(error) => {
            eprintln!("Error reading file {}: {}", path, error);
            return;
        }
    };

    let source = Source::from(program.as_str());
    let (lexing_time, (tokens, errors)) =
        trace_time(|| lexer::lexer().parse(&program).into_output_errors());

    // Print the tokenized input if the flag is set. Even if errors are encountered,
    // we still want to print the tokens that were successfully parsed for debugging
    // purposes.
    if args.get_flag("dump-tokens") {
        println!("Tokens:");
        for token in tokens.as_ref().unwrap_or(&vec![]) {
            println!(" - {token:?}");
        }
    }

    // If there were lexer errors, print them and exit early.
    if !errors.is_empty() {
        for e in errors {
            eprintln!("{e}");
        }
        return;
    }

    // Parse the tokens into an AST.
    let tokens = tokens.unwrap();
    let (parsing_time, (ast, errors)) = trace_time(|| {
        parser::file_parser()
            .parse(
                tokens
                    .as_slice()
                    .map((program.len()..program.len()).into(), |spanned| {
                        spanned.as_tuple()
                    }),
            )
            .into_output_errors()
    });

    // Dump the AST if the flag is set. Even if errors are encountered, we still want to
    // print the AST that was successfully parsed for debugging purposes.
    if args.get_flag("dump-ast") {
        println!("AST:");
        if let Some(node) = &ast {
            println!("{node:#?}");
        }
    }

    if !errors.is_empty() {
        for e in errors {
            eprintln!("{e}");
        }
        return;
    }

    let mut program = ast.unwrap();
    let mut semantic_analyzer = semantic::SemanticAnalysis::new(path);
    let semantic_time = trace_time(|| semantic_analyzer.check_program(&mut program)).0;

    // If there were semantic analysis errors, print them and exit early.
    let types = match semantic_analyzer.finalize() {
        Ok(types) => types,
        Err(errors) => {
            for e in errors {
                e.eprint((path.as_str(), &source)).unwrap();
            }
            return;
        }
    };

    // Generate C code from the AST and print it if the flag 'dump-ir' flag is set.
    let (codegen_time, code) = trace_time(|| codegen::c::generate(&program, types));
    std::fs::write(output, &code).expect("Failed to write to output file");
    if args.get_flag("dump-ir") {
        println!("Generated C code:");
        println!("{}", &code);
    }

    // If the 'trace-time' flag is set, print the time taken by each compilation phase
    if args.get_flag("trace-time") {
        println!("Compilation time breakdown:");
        println!(" - Lexing time:      {}", readable_duration(lexing_time));
        println!(" - Parsing time:     {}", readable_duration(parsing_time));
        println!(" - Semantic time:    {}", readable_duration(semantic_time));
        println!(" - Codegen time:     {}", readable_duration(codegen_time));
        // TODO: Add the total time measurement including file I/O since the start of the
        // compilation to improve accuracy.
    }
}

/// Utility function to trace the execution time of a given closure, returning the
/// duration along with the result of the closure.
fn trace_time<T, F: FnOnce() -> T>(f: F) -> (std::time::Duration, T) {
    let start = std::time::Instant::now();
    let result = f();
    let duration = start.elapsed();
    (duration, result)
}

/// Format a `Duration` into a human-readable string, with appropriate units and precision. This
/// function converts the duration into seconds, milliseconds, microseconds, or nanoseconds
/// based on its magnitude.
fn readable_duration(duration: std::time::Duration) -> String {
    let nanos = duration.as_nanos();
    if nanos >= 1_000_000_000 {
        format!("{:.3} s", nanos as f64 / 1_000_000_000.0)
    } else if nanos >= 1_000_000 {
        format!("{:.3} ms", nanos as f64 / 1_000_000.0)
    } else if nanos >= 1_000 {
        format!("{:.3} µs", nanos as f64 / 1_000.0)
    } else {
        format!("{} ns", nanos)
    }
}
