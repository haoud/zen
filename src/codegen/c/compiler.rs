use std::io::Write;

/// The different kinds of C compilers that we support. This may be useful
/// in the future if we want to support more compilers or if we want to
/// add specific behavior for each compiler.
///
/// However, this prevents us from using some compiler-specific features that
/// may be useful. For example, the __attribute__ keyword from GCC is supported
/// by Clang, but not by other compilers like MSVC or TCC.
///
/// In the future, we probably only support one compiler, so this enum may
/// be removed.
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

    /// Get the major version of the compiler. This is useful to check
    /// if the compiler supports certain features or if it has bugs that
    /// we need to work around.
    fn major(&self) -> u32;

    /// Check if the compiler exists in the system
    fn exists(&self) -> bool;
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Clang;

impl Clang {
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

    // Get the major version of the clang compiler
    #[must_use]
    fn major(&self) -> u32 {
        let clang = std::process::Command::new("clang")
            .arg("--version")
            .output()
            .expect("Failed to get clang version");

        let output = String::from_utf8(clang.stdout)
            .expect("Failed to parse clang output");

        let version = output
            .lines()
            .next()
            .expect("Failed to get clang version line");

        let version = version
            .split_whitespace()
            .nth(2)
            .expect("Failed to get clang version number");

        let version = version
            .split('.')
            .next()
            .expect("Failed to get clang major version");

        version
            .parse()
            .expect("Failed to parse clang major version")
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
