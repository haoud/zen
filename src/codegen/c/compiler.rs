use std::io::Write;

/// The different kinds of C compilers that we support.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum CompilerKind {
    Clang,
}

/// A trait that defines the behavior of a compiler. It allows us to
/// abstract the compiler implementation and make it easier to switch
/// between different compilers during the code generation phase.
pub trait Compiler {
    /// Compile the source code and write the output to the specified
    /// file. If the compilation fails, the function will panic.
    fn build(&self, src: &str, output: &str);

    /// Check if the compiler exists in the system
    fn exists(&self) -> bool;
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Clang;

impl Clang {
    /// Create a new instance of the Clang compiler
    #[must_use]
    pub fn new() -> Self {
        Self
    }
}

impl Compiler for Clang {
    /// Check if clang is installed on the system
    #[must_use]
    fn exists(&self) -> bool {
        let clang = std::process::Command::new("clang")
            .arg("--version")
            .output();
        clang.is_ok()
    }

    /// Compile the source code using clang and write the output to the
    /// specified file. If the compilation fails, the function will panic.
    fn build(&self, src: &str, output: &str) {
        let mut clang = std::process::Command::new("clang")
            .arg("-x")
            .arg("c")
            .arg("-o")
            .arg(output)
            .arg("-")
            .stdin(std::process::Stdio::piped())
            .spawn()
            .expect("Failed to compile the source file");

        // Write the source code to the stdin of the clang process
        // so that it can compile it.
        clang
            .stdin
            .as_mut()
            .expect("Failed to open stdin")
            .write_all(src.as_bytes())
            .expect("Failed to write to stdin");

        // Wait for the clang process to finish and check if it was
        // successful. If it wasn't, we panic (we can't do much else).
        let status = clang.wait().expect("Failed to wait for clang");
        if !status.success() {
            panic!("Failed to compile the source file with clang");
        }
    }
}
