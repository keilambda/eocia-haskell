# Essentials of Compilation: An Incremental Approach in Haskell

Minimal compiler targeting both Linux and macOS/Darwin platforms. Implements compilation using an incremental approach[^1] [^2] from LVar language to x86_64 assembly (AT&T syntax).

## Languages

- [**LVar**](./src/Stage/LVar.hs): Expression-based language with integer literals, arithmetic and let-bindings
- [**LVarMon**](./src/Stage/LVarMon.hs): LVar but in Monadic Normal Form[^3] [^4]
- [**CVar**](./src/Stage/CVar.hs): Statement-based three-address code intermediate language
- [**X86Var**](./src/Stage/X86Var.hs): X86 intermediate language with variables
- [**X86Int**](./src/Stage/X86Int.hs): Target assembly language

## Features

- Expression simplification via partial evaluation
- Stack-based variable allocation
- Instruction selection with peephole optimization
- Proper stack frame management
- Call ABI compliance (Linux, Darwin)

### Current status

- Implementing Register allocation

## License

BSD-3-Clause

[^1]: https://github.com/IUCompilerCourse/Essentials-of-Compilation
[^2]: http://scheme2006.cs.uchicago.edu/11-ghuloum.pdf
[^3]: https://www.cs.cmu.edu/~crary/819-f09/Moggi91.pdf
[^4]: https://link.springer.com/chapter/10.1007/3-540-36579-6_6
