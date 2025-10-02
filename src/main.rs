use crate::semantic::SemanticAnalysis;
use ariadne::Source;
use chumsky::prelude::*;

pub mod ast;
pub mod codegen;
pub mod config;
pub mod lang;
pub mod lexer;
pub mod parser;
pub mod semantic;

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

    let program = std::fs::read_to_string(path).expect("Failed to read file");
    let source = Source::from(program.as_str());
    let (tokens, errors) = lexer::lexer().parse(&program).into_output_errors();

    // Print the tokenized input if the flag is set. Even if errors are encountered,
    // we still want to print the tokens that were successfully parsed for debugging
    // purposes.
    if args.get_flag("dump-tokens") {
        println!("Tokens:");
        for token in tokens.as_ref().unwrap_or(&vec![]) {
            println!(" - {:?}", token);
        }
    }

    // If there were lexer errors, print them and exit early.
    if !errors.is_empty() {
        for e in errors {
            eprintln!("{}", e);
        }
        return;
    }

    // Parse the tokens into an AST.
    let tokens = tokens.unwrap();
    let (ast, errors) = parser::file_parser()
        .parse(
            tokens
                .as_slice()
                .map((program.len()..program.len()).into(), |spanned| {
                    (&spanned.0, &spanned.1)
                }),
        )
        .into_output_errors();

    // Dump the AST if the flag is set. Even if errors are encountered, we still want to
    // print the AST that was successfully parsed for debugging purposes.
    if args.get_flag("dump-ast") {
        println!("AST:");
        if let Some(node) = &ast {
            println!("{:#?}", node);
        }
    }

    if !errors.is_empty() {
        for e in errors {
            eprintln!("{}", e);
        }
        return;
    }

    let mut functions = ast.unwrap();
    let mut semantic_analyzer = SemanticAnalysis::new(path);
    let errors = semantic_analyzer.check_functions(&mut functions);

    // If there were semantic analysis errors, print them and exit early.
    if let Err(errors) = errors {
        for e in errors {
            e.eprint((path.as_str(), &source)).unwrap();
        }
        return;
    }

    // Generate C code from the AST and print it if the flag 'dump-ir' flag is set.
    let code = codegen::c::generate(&functions);
    std::fs::write(output, &code).expect("Failed to write to output file");
    if args.get_flag("dump-ir") {
        println!("Generated C code:");
        println!("{}", &code);
    }
}
