//! Code generation module. It contains the code generation logic for the
//! compiler. The code generation is clearly separated from the rest of the
//! compiler logic, so that it can be easily replaced with different backends
//! if needed.
//!
//! # C backend
//! This is the main backend for the compiler. It generates C code that can be
//! compiled with any compliant C compiler (TODO: which C standard?). The main
//! goal of this backend is to provide a way to generate code that can be:
//!  - Humanly readable for debugging purposes (and curiosity !)
//!  - Easily optimized by the C compiler, removing the need of a complex
//!    optimization logic in the compiler itself.
//!  - Easily portable to any platform with a C compiler.
//!
//! # LLVM backend (not implemented yet)
//! This is a more advanced backend that generates LLVM IR code. The main goal
//! of this backend is to provide a way to generate optimized code without the
//! need of a C compiler. The LLVM IR code can be compiled to native code with
//! the LLVM compiler, which is known for its advanced optimization
//! capabilities. This backend is not planned for the first versions of the
//! compiler, but it is a good candidate for future versions when the language
//! grammar and features will be more stable and the need for advanced
//! optimization will be more evident.
//!
//! # Custom backend (not implemented yet)
//! Not planned yet and probably for a long time, but it worth mentioning it
//! here as the main goal will be to provide a way to generate unoptimized
//! code really fast, without the need of a complex backend like LLVM or the
//! need of a C compiler (and relexing and reparsing the generated code) and
//! to reduce the compiler dependencies to the minimum, allowing a future
//! rewrite in the language itself to be self-hosted. For simplicity, this
//! will likely be a simple assembly backend, only supporting a few platforms.
pub mod c;
