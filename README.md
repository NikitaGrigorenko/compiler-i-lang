# Imperative language compiler

The lexical, syntax, semantic analyser and code generation tool for programs written in Imperative language.

Developed by using FLEX, Bison and LLVM.

Target platform of the code generation is llvm-bitcode.

Below we provide the detailed build instructions, as well as describe a typical use case of this tool.

## Build instructions

Minimal prerequisites:
- CMake
- make
- flex 2.6.4
- bison (GNU Bison) 3.8.2
- LLVM 17.0.5
  
   #### Flex installation process
   Ubuntu:
   ```Bash
   sudo apt-get install flex
   ```
   MacOS:
   ```Bash
   brew install flex
   ```

   #### Bison installation process
   Ubuntu:
   ```Bash
   sudo apt-get install bison
   ```
   MacOS:
   ```Bash
   brew install bison
   ```
   > Note: do not forget export path: ```export PATH="$(brew --prefix bison)/bin:$PATH"```

   #### LLVM installation process
   Ubuntu:
   ```Bash
   sudo apt-get install llvm
   ```
   MacOS:
   ```Bash
   brew install llvm
   ```
   > Note: do not forget export paths
   
> Note: all implementation process was tested only on macOS Sonoma

Environment Setup:
```bash
# Navigate to the cloned Git repository folder with the source code of the tool
cd i-lang-new

# Create "build" directory
mkdir build

# Navigate to the "build" directory
cd build

# Run CMake to configure the project
cmake  ../

# Run Make to build the project
make
```


## Sample Usage

The syntax for a sample invocation of this analyser tool is as follows:

```bash
./cxx_compiler ../source_programs/test2.i
```

Above, `./cxx_compiler` invokes an automatic lexical, syntax and semantic analysis, as well as code generation and compilation of IR llvm  for the given program `test2.i`.

Source_programs folder contains some test examples of programs on Imperative language.
After running the command above you will get an output in format:

```bash
-------------- Lexer Result --------------

routine identifier(identifier : identifier):identifier is
    var identifier :identifier is identifier;
    if identifier > 1 then
       identifier := identifier(identifier - 1) + identifier(identifier - 2);
    end
    return identifier;
end

routine identifier():identifier is
    identifier(identifier(3));
    return 0;
end

-------------- Parser Result --------------
Here you will see possible syntax errors 

------------- Semantic Result -------------
Here you will see possible semantic errors 

--------- Code Generation Result ----------
; ModuleID = 'compilation_result'
source_filename = "compilation_result"

@"$_print_int_str" = private unnamed_addr constant [4 x i8] c"%d\0A\00", align 1

declare i32 @printf(ptr, ...)

define void @printInt(i32 %_int_to_print) {
entrypoint:
  %printf.ret = call i32 (ptr, ...) @printf(ptr @"$_print_int_str", i32 %_int_to_print)
  ret void
}

define i32 @fib(i32 %0) {
entrypoint:
  %1 = icmp sgt i32 %0, 1
  br i1 %1, label %"func$fib_t_0", label %"func$fib_1"

"func$fib_t_0":                                   ; preds = %entrypoint
  %2 = sub i32 %0, 1
  %3 = call i32 @fib(i32 %2)
  %4 = sub i32 %0, 2
  %5 = call i32 @fib(i32 %4)
  %6 = add i32 %3, %5
  br label %"func$fib_1"

  ...

```

After compilation done, you can execute the compiled program by using:
`./compiledCode`
