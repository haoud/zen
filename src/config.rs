/// The default name of the input file if none is provided.
pub static DEFAULT_INPUT_NAME: &str = "main.zen";

/// The default name of the output file. It use the traditional `a.out`
/// name, that comes from times when the most common binary format used
/// by Unix-like systems was the a.out format.
pub static DEFAULT_OUTPUT_NAME: &str = "a.out";

/// The default name of the backend to use if none is provided. The
/// default backend is the C backend because it is the most mature
/// backend and the one that is most likely to work (bacause it is
/// a simple backend that generates C code and then uses the system's
/// C compiler to generate the final binary).
pub static DEFAULT_BACKEND: &str = "c";
