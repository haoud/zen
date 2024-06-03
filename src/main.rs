//! TODO:
//!  - Improve the code, reduce the number of unwraps
//!  - Comment the code
//!  - Unit testings
//!  - Release the first git version ^_^

use chumsky::prelude::*;
use clap::{arg, command};
use codegen::c::compiler::Compiler;

pub mod ast;
pub mod codegen;
pub mod config;
pub mod lang;
pub mod lexer;
pub mod parser;
pub mod semantic;

/// The main entry point of the compiler. It parses the command line
/// arguments and dispatches the appropriate action. The compiler use
/// the `clap` crate to parse the command line arguments, and I can
/// say that it's a very good crate to use for this purpose, and is
/// WAY painless than using `getopts` in C/C++.
fn main() {
    let matches = command!("zen")
        .about("Zen programming language compiler")
        .subcommand_required(true)
        .subcommand(
            clap::Command::new("build")
                .about("Build a Zen source file")
                .arg(arg!([file] "The file to compile"))
                .arg(arg!(-o --output <output> "The output file"))
                .arg(
                    arg!(--"dump-tokens" "Dump the tokens to the console")
                        .action(clap::ArgAction::SetTrue),
                )
                .arg(
                    arg!(--"dump-ast" "Dump the AST to the console")
                        .action(clap::ArgAction::SetTrue),
                )
                .arg(
                    arg!(--"dump-ir" "Dump the intermediate representation to the console")
                    .action(clap::ArgAction::SetTrue),
                )
                .arg(
                    arg!(--"llvm-backend"                    
                        "Use the LLVM backend to generate code (experimental)")
                    .action(clap::ArgAction::SetTrue),
                ),
        )
        .subcommand(
            clap::Command::new("translate")
                .about("Translate a Zen source file to C")
                .arg(arg!([file] "The file to translate"))
                .arg(arg!(-o --output <output> "The output file")),
        )
        .subcommand(
            clap::Command::new("create")
                .about("Create a new Zen project")
                .arg(arg!([name] "The name of the project")),
        )
        .get_matches();

    // Dispatch the appropriate action based on the subcommand. If no
    // subcommand is provided, we default to the `build` subcommand
    // since it's the most common action.
    if let Some(args) = matches.subcommand_matches("build") {
        build(args);
    } else if let Some(_args) = matches.subcommand_matches("translate") {
        unimplemented!();
    } else if let Some(_args) = matches.subcommand_matches("create") {
        unimplemented!();
    } else if matches.subcommand_name().is_none() {
        build(&matches);
    }
}

/// Build a Zen source file.
fn build(build: &clap::ArgMatches) {
    let default_output_name = String::from(config::DEFAULT_OUTPUT_NAME);
    let default_input_name = String::from(config::DEFAULT_INPUT_NAME);

    // Parse the output file if provided, otherwise use the default
    // output name defined in the configuration file.
    let output = build
        .try_get_one::<String>("output")
        .unwrap_or(Some(&default_output_name))
        .unwrap_or(&default_output_name);

    // Parse the input file if provided, otherwise use the default
    // input name defined in the configuration file.
    let path = build
        .try_get_one::<String>("file")
        .unwrap_or(Some(&default_input_name))
        .unwrap_or(&default_input_name);

    // Read the input file.
    let input = match std::fs::read_to_string(path) {
        Ok(input) => input,
        Err(err) => {
            panic!("Error reading {}: {}", path, err);
        }
    };

    // Tokenize the input using the lexer.
    let (tokens, lexer_errors) =
        lexer::lexer().parse(&input).into_output_errors();

    // Print the tokenized input if the flag is set.
    // For debugging purposes only !
    if build.get_flag("dump-tokens") {
        println!("Tokens:");
        for token in tokens.as_ref().unwrap() {
            println!(" - {:?}", token);
        }
    }

    let (ast, parse_errors) = if let Some(tokens) = &tokens {
        let (ast, parser_errors) = parser::expr_parser()
            .map_with(|ast, errors| (ast, errors.span()))
            .parse(tokens.as_slice().spanned((input.len()..input.len()).into()))
            .into_output_errors();

        // Print the AST if the flag is set.
        // For debugging purposes only !
        if build.get_flag("dump-ast") {
            println!("AST:");
            if let Some(expr) = &ast {
                println!("{:#?}", expr);
            }
        }

        (ast, parser_errors)
    } else {
        (None, vec![])
    };

    // Merge lexer and parser errors into a single iterator
    // and print them using ariadne to have a nice error and
    // a good user experience.
    lexer_errors
        .into_iter()
        .map(|e| e.map_token(|c| c.to_string()))
        .chain(
            parse_errors
                .into_iter()
                .map(|e| e.map_token(|c| c.to_string())),
        )
        .for_each(|error| {
            let start = error.span().start;
            let end = error.span().end;
            let range = start..end;
            ariadne::Report::build(ariadne::ReportKind::Error, path, start)
                .with_label(
                    ariadne::Label::new((path, range))
                        .with_color(ariadne::Color::Red),
                )
                .with_message(format!("{}", error))
                .with_code(0)
                .finish()
                .eprint((path, ariadne::Source::from(input.clone())))
                .unwrap();
        });

    // If there are any syntax errors or parsing errors, we stop here
    // and don't continue to the semantic analysis and code generation
    // phases because it doesn't make sense to do so: the AST generated
    // is not correct and we can't generate code from it.
    if let Some((expr, _)) = ast {
        if build.get_flag("llvm-backend") {
            let code = codegen::llvm::build(&expr, output);
            if build.get_flag("dump-ir") {
                println!("Intermediate LLVM Representation:");
                println!("{}", code);
            }
        } else {
            let code = codegen::c::generate(&expr);
            if build.get_flag("dump-ir") {
                println!("Intermediate C Representation:");
                println!("{}", code);
            }

            let clang = codegen::c::compiler::Clang::new();
            match clang.exists() {
                true => clang.build(&code, output),
                false => {
                    panic!("Error: clang is not installed on the system")
                }
            }
        }
    }
}
