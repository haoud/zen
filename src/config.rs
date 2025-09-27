/// The default name of the input file if none is provided.
#[cfg(not(debug_assertions))]
pub static DEFAULT_INPUT_NAME: &str = "main.zen";

/// The default name of the input file if none is provided. For debug builds,
/// we use a different default input file that is located in the `tests`
/// directory. This is useful for testing and development, as we can simply
/// type `cargo run` and it will compile the `tests/main.zen` file without
/// needing to provide any additional arguments.
#[cfg(debug_assertions)]
pub static DEFAULT_INPUT_NAME: &str = "tests/main.zen";

/// The default name of the output file. It use the traditional `a.out`
/// name, that comes from times when the most common binary format used
/// by Unix-like systems was the a.out format.
pub static DEFAULT_OUTPUT_BUILD_NAME: &str = "a.out";

/// The default name of the output file when translating to C code.
pub static DEFAULT_OUTPUT_C_NAME: &str = "zen.c";

/// The default name of the backend to use if none is provided. The
/// default backend is the C backend because it is the most mature
/// backend and the one that is most likely to work (because it is
/// a simple backend that generates C code and then uses the system's
/// C compiler to generate the final binary).
pub static DEFAULT_BACKEND: &str = "c";
