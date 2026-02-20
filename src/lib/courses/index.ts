import type { Course } from "./types";
import { chapters as goChapters, lessons as goLessons } from "@/lib/lessons";
import { zigChapters, zigLessons } from "@/lib/lessons/zig";
import { sqlChapters, sqlLessons } from "@/lib/lessons/sql";
import { arm64Chapters, arm64Lessons } from "@/lib/lessons/arm64";
import { cChapters, cLessons } from "@/lib/lessons/c";
import { gleamChapters, gleamLessons } from "@/lib/lessons/gleam";
import { rChapters, rLessons } from "@/lib/lessons/r";
import { holycChapters, holycLessons } from "@/lib/lessons/holyc";
import { linuxChapters, linuxLessons } from "@/lib/lessons/linux";
import { coreutilsChapters, coreutilsLessons } from "@/lib/lessons/coreutils";
import { kernelChapters, kernelLessons } from "@/lib/lessons/kernel";
import { jsChapters, jsLessons } from "@/lib/lessons/javascript";
import { tsChapters, tsLessons } from "@/lib/lessons/typescript";
import { algorithmsChapters, algorithmsLessons } from "@/lib/lessons/algorithms";
import { distributedSystemsChapters, distributedSystemsLessons } from "@/lib/lessons/distributed-systems";
import { rubyChapters, rubyLessons } from "@/lib/lessons/ruby";
import { treesChapters, treesLessons } from "@/lib/lessons/trees";

const goIntroductionContent = `
## Why Go?

Go is a statically typed, compiled language designed for simplicity and efficiency. It compiles fast, runs fast, and is easy to read. Here is what makes it stand out:

- **Fast compilation** -- Go compiles entire projects in seconds, even large codebases.
- **Simple syntax** -- The language has only 25 keywords. If you know any C-style language, you can read Go code immediately.
- **Built-in concurrency** -- Goroutines and channels make concurrent programming straightforward.
- **Rich standard library** -- HTTP servers, JSON encoding, cryptography, testing -- all built in, no third-party dependencies required.
- **Single binary deployment** -- Go compiles to a single static binary. No runtime, no VM, no dependencies to install on the target machine.

## The Story

Go was created at Google in 2007 by Robert Griesemer, Rob Pike, and Ken Thompson. The three designers were frustrated with the complexity of existing systems languages, particularly C++: slow builds, complicated dependency management, and a language specification that had grown unwieldy over decades.

Their goal was a language that combined the performance and safety of a compiled language with the ease of use of a dynamic one. Go was open-sourced in November 2009 and reached its first stable release, Go 1.0, in March 2012 with a strong backward-compatibility promise.

Rob Pike had co-created the Plan 9 operating system and the UTF-8 encoding. Ken Thompson had co-created Unix and the C programming language. Robert Griesemer had worked on the V8 JavaScript engine and the Java HotSpot VM.

## Who Uses Go

Go has become the backbone of modern cloud infrastructure. Some of the most influential open-source projects are written in Go:

- **Docker** -- the containerization platform that changed how software is deployed.
- **Kubernetes** -- the container orchestration system that powers cloud-native applications.
- **Terraform** -- the infrastructure-as-code tool used to manage cloud resources.

Major companies use Go extensively: Google, Uber, Dropbox, Twitch, Cloudflare, and many others. It is especially popular for building microservices, CLI tools, DevOps tooling, and network services.

## What You Will Learn

This course contains **17 lessons** organized into **8 chapters**:

1. **Foundations** -- How Go programs are structured: packages, imports, and the \`main\` function. Variables, types, and constants.
2. **Control Flow** -- Conditionals with \`if\`/\`else\` and \`switch\`. Loops with \`for\`.
3. **Functions** -- Declaring functions, multiple return values, named returns, and deferred execution.
4. **Data Structures** -- Slices (dynamic arrays), maps (hash tables), and string operations.
5. **Custom Types** -- Pointers, structs, and interfaces.
6. **Error Handling** -- Go's explicit approach to errors using values instead of exceptions.
7. **Generics** -- Type parameters for functions and data structures.
8. **Concurrency** -- Goroutines, channels, and the select statement.

Each lesson explains a concept, demonstrates it with code examples, and gives you an exercise to practice.

Let's get started.
`;

const goWhatsNextContent = `
## Congratulations

You have completed all the lessons. You now have a solid foundation in Go's core language features: packages, variables, control flow, functions, slices, maps, structs, interfaces, error handling, generics, and concurrency.

That is a real accomplishment. You know enough to read and write Go code, understand Go codebases, and start building your own projects.

## The Standard Library

Go ships with a comprehensive standard library. Key packages to explore:

- \`net/http\` -- build web servers and HTTP clients
- \`encoding/json\` -- encode and decode JSON
- \`os\` and \`io\` -- file system and I/O operations
- \`testing\` -- write and run tests
- \`context\` -- manage cancellation and timeouts
- \`sync\` -- mutexes, wait groups, and other synchronization primitives
- \`database/sql\` -- database access interface

## Build Something

The best way to learn is to build. Some project ideas:

- **A command-line tool** -- a file organizer, task tracker, or URL shortener
- **A REST API** with \`net/http\` -- user authentication, CRUD operations, JSON responses
- **A concurrent web scraper** -- fetch multiple pages in parallel using goroutines
- **A chat server** using WebSockets -- real-time communication between clients

## References

Here are the best resources for continuing your Go journey:

- [The Go Programming Language Specification](https://go.dev/ref/spec) -- the official language spec, precise and surprisingly readable.
- [Effective Go](https://go.dev/doc/effective_go) -- idiomatic Go patterns and conventions, written by the Go team.
- [Go by Example](https://gobyexample.com) -- annotated code examples covering a wide range of Go features.
- [The Go Blog](https://go.dev/blog) -- official blog with in-depth articles on language features and design decisions.
- [The Go Playground](https://go.dev/play) -- run and share Go code in the browser.
- *The Go Programming Language* by Alan Donovan and Brian Kernighan (Addison-Wesley, 2015) -- the most comprehensive Go book.
- [Go Standard Library Documentation](https://pkg.go.dev/std) -- detailed documentation for every package in the standard library.
`;

const zigIntroductionContent = `
## Why Zig?

Zig is a systems programming language designed for robustness, performance, and maintainability. It aims to be a better C -- with modern ergonomics but without hidden control flow, hidden allocations, or undefined behavior.

- **No hidden control flow** -- No operator overloading, no hidden function calls. What you see is what the machine executes.
- **Compile-time execution** -- Zig's \`comptime\` lets you run arbitrary code at compile time, replacing the need for macros and code generation.
- **Manual memory management done right** -- Allocators are explicit and passed as parameters. No global allocator, no garbage collector.
- **C interop** -- Zig can import C headers and call C functions directly, with no bindings or FFI boilerplate.
- **Safety without runtime cost** -- Bounds checking, null checking, and undefined behavior detection in debug builds, stripped in release builds.

## The Story

Zig was created by Andrew Kelley, who started working on it in 2015 after becoming frustrated with C and C++ while working on performance-critical systems. The first public release came in 2017.

Andrew's goal was a language that could be used everywhere C is used -- operating systems, embedded systems, high-performance computing -- but with a focus on making the programmer's intent clear and catching bugs at compile time rather than at runtime.

The Zig Software Foundation was established to support the development of the language and its ecosystem.

## Who Uses Zig

Zig is gaining traction in systems programming and infrastructure:

- **Bun** -- the fast JavaScript runtime and toolkit is written in Zig.
- **TigerBeetle** -- a high-performance financial transactions database.
- **Mach Engine** -- a game engine built on Zig.
- **Uber** -- uses Zig for network proxy infrastructure.

Zig is also used as a drop-in C/C++ cross-compiler, since its toolchain includes a complete C compiler with cross-compilation support out of the box.

## What You Will Learn

This course contains **12 lessons** organized into **6 chapters**:

1. **Foundations** -- How Zig programs are structured, variables, types, and printing output.
2. **Control Flow** -- Conditionals with \`if\`/\`else\` and \`switch\`. Loops with \`while\` and \`for\`.
3. **Functions** -- Function declarations, error unions, and error handling.
4. **Data Structures** -- Arrays, slices, and structs with methods.
5. **Memory** -- Pointers, allocators, and manual memory management.
6. **Advanced** -- Compile-time execution, optionals, and tagged unions.

Each lesson explains a concept, demonstrates it with code examples, and gives you an exercise to practice.

Let's get started.
`;

const zigWhatsNextContent = `
## Congratulations

You have completed all twelve lessons. You now have a solid foundation in Zig's core features: variables, control flow, functions, error handling, arrays, structs, pointers, allocators, comptime, and optionals.

That is a real accomplishment. You understand the key ideas that make Zig unique among systems languages.

## What to Explore Next

Here are topics to dive deeper into:

- **Async/Await** -- Zig has built-in support for async functions and event loops.
- **SIMD and Vectors** -- Zig exposes SIMD operations through its vector types.
- **C Interop** -- Import and use C libraries directly in Zig code with \`@cImport\`.
- **Build System** -- Zig's \`build.zig\` replaces Makefiles and CMake with Zig code.
- **Testing** -- Zig has built-in test blocks that run with \`zig test\`.

## Build Something

The best way to learn is to build. Some project ideas:

- **A command-line tool** -- a file processor, argument parser, or system utility
- **A simple HTTP server** -- using Zig's standard library networking
- **A data structure library** -- implement a hash map, linked list, or B-tree
- **A game** -- using a Zig game framework or raw SDL bindings

## References

- [Zig Language Reference](https://ziglang.org/documentation/master/) -- the official language documentation.
- [Zig Standard Library Documentation](https://ziglang.org/documentation/master/std/) -- API docs for the standard library.
- [Ziglings](https://codeberg.org/ziglings/exercises/) -- small exercises to learn Zig.
- [Zig News](https://zig.news/) -- community articles and tutorials.
- [Zig GitHub](https://github.com/ziglang/zig) -- the source code and issue tracker.
`;

const sqlIntroductionContent = `
## Why PostgreSQL?

PostgreSQL is the world's most advanced open-source relational database. It is reliable, feature-rich, and standards-compliant. Here is what makes it stand out:

- **Battle-tested** -- Used by Apple, Instagram, Spotify, and thousands of other companies in production.
- **Feature-rich** -- JSON, full-text search, arrays, window functions, CTEs, and more -- all built in.
- **Standards-compliant** -- Follows the SQL standard closely, so what you learn here applies broadly.
- **Free and open source** -- No licensing fees, backed by a strong community since 1996.
- **Extensible** -- Custom types, operators, index methods, and procedural languages.

## The Story

PostgreSQL traces its origins to the POSTGRES project at UC Berkeley, started by Professor Michael Stonebraker in 1986. The project was a research successor to the Ingres database. In 1996, the project was renamed PostgreSQL to reflect its SQL support, and the open-source community took over development.

Today, PostgreSQL is developed by a global community of contributors. Major releases come out annually, each adding significant features while maintaining backward compatibility.

## Who Uses PostgreSQL

PostgreSQL powers some of the world's largest applications:

- **Instagram** -- stores hundreds of millions of user records.
- **Spotify** -- manages music catalog and user data.
- **Apple** -- uses PostgreSQL for various internal services.
- **Reddit, Twitch, The Guardian** -- all rely on PostgreSQL in production.

It is the default choice for startups and enterprises alike, and the most popular database on platforms like Supabase, Neon, and Railway.

## What You Will Learn

This course contains **22 lessons** organized into **8 chapters**:

1. **SQL Basics** -- SELECT queries, filtering with WHERE, sorting, and pagination.
2. **Tables** -- Data types, CREATE TABLE, constraints, keys, and ALTER TABLE.
3. **CRUD Operations** -- INSERT, UPDATE, DELETE, upsert, and bulk operations.
4. **Joins** -- INNER JOIN, LEFT/RIGHT/FULL OUTER JOIN, self joins, and cross joins.
5. **Aggregations** -- COUNT, SUM, AVG, GROUP BY, HAVING, ROLLUP, and CUBE.
6. **Subqueries & CTEs** -- Nested queries, correlated subqueries, and Common Table Expressions.
7. **Advanced SQL** -- Window functions, JSON operations, arrays, and full-text search.
8. **Performance** -- Indexes, EXPLAIN ANALYZE, query optimization, and database design.

Each lesson explains a concept, demonstrates it with SQL examples, and gives you an exercise to practice against a live PostgreSQL database running in your browser.

Let's get started.
`;

const sqlWhatsNextContent = `
## Congratulations

You have completed all 22 lessons. You now have a solid foundation in SQL and PostgreSQL: querying, filtering, joining, aggregating, subqueries, CTEs, window functions, JSON, indexes, and database design.

That is a real accomplishment. You can read and write SQL confidently, design database schemas, and optimize queries.

## What to Explore Next

Here are topics to dive deeper into:

- **Transactions** -- ACID properties, BEGIN/COMMIT/ROLLBACK, isolation levels, and deadlocks.
- **Stored Procedures and Functions** -- PL/pgSQL for server-side logic.
- **Triggers** -- Automatically execute functions when data changes.
- **Views and Materialized Views** -- Save complex queries as virtual or cached tables.
- **Partitioning** -- Split large tables for better performance.
- **Replication** -- Set up read replicas and high availability.

## Build Something

The best way to learn is to build. Some project ideas:

- **A REST API with a database backend** -- CRUD endpoints backed by PostgreSQL.
- **A data dashboard** -- aggregate and visualize data from multiple tables.
- **A multi-tenant application** -- row-level security and schema isolation.
- **A search engine** -- full-text search with ranking and highlighting.

## References

- [PostgreSQL Official Documentation](https://www.postgresql.org/docs/current/) -- comprehensive and well-written.
- [PostgreSQL Tutorial](https://www.postgresqltutorial.com/) -- practical examples for common tasks.
- [Use The Index, Luke](https://use-the-index-luke.com/) -- deep dive into SQL indexing and performance.
- [pgexercises.com](https://pgexercises.com/) -- interactive SQL exercises.
- *Designing Data-Intensive Applications* by Martin Kleppmann (O'Reilly, 2017) -- the best book on database internals and distributed systems.
`;

const arm64IntroductionContent = `
## Why ARM64 Assembly?

ARM64 (AArch64) is the dominant processor architecture of the modern era. It powers every iPhone, every Android phone, Apple's M-series Macs, AWS Graviton servers, and billions of embedded devices. Understanding ARM64 assembly gives you direct insight into how these processors execute code.

- **The architecture of the future** -- ARM's power efficiency is moving it from mobile into servers, desktops, and supercomputers.
- **Clean, modern design** -- ARM64 is a modern RISC architecture with a regular, orthogonal instruction set that is easier to learn than x86.
- **Fixed-width instructions** -- Every instruction is exactly 32 bits. No variable-length decoding complexity.
- **31 general-purpose registers** -- Generous register file means less memory traffic.
- **Real-world relevance** -- Knowing ARM64 assembly helps you understand compiler output, debug low-level issues, and write performance-critical code.

## The Story

ARM started in 1983 at Acorn Computers in Cambridge, England. Sophie Wilson and Steve Furber designed the original ARM1 processor. The name originally stood for "Acorn RISC Machine," later "Advanced RISC Machine."

ARM's big insight was that a simpler processor could be faster and more power-efficient. This bet paid off spectacularly: ARM processors now ship in over 250 billion chips, more than any other processor family in history.

ARM64 (AArch64) was introduced in 2011 with the ARMv8-A architecture, bringing 64-bit computing to the ARM world. It was a clean break from the 32-bit ARM instruction set, designed from scratch with modern computing needs in mind.

## What You Will Learn

This course contains **16 lessons** organized into **6 chapters**:

1. **Foundations** -- Registers, MOV, arithmetic, and your first program using Linux syscalls.
2. **Memory** -- Load/store architecture, addressing modes, and the stack.
3. **Control Flow** -- Condition flags, CMP, conditional branches, and loops.
4. **Functions** -- BL/RET, the calling convention, and recursive functions.
5. **Bitwise Operations** -- AND, OR, XOR, shifts, and bit manipulation patterns.
6. **Putting It Together** -- String operations and sorting algorithms.

Each lesson explains a concept, demonstrates it with code, and gives you an exercise. Your code runs directly in the browser using a custom ARM64 interpreter -- no native toolchain needed.

Let's get started.
`;

const arm64WhatsNextContent = `
## Congratulations

You have completed all 16 lessons. You now understand ARM64's register model, memory operations, control flow, functions, bitwise operations, and have implemented real algorithms in assembly.

That is a real accomplishment. ARM64 assembly is not easy, and you have built a solid foundation.

## What to Explore Next

- **System calls** -- Linux has hundreds of syscalls beyond write and exit: open, read, mmap, socket, and more.
- **SIMD/NEON** -- ARM64's vector instructions for parallel data processing.
- **Floating point** -- ARM64 has 32 dedicated 128-bit vector/FP registers (V0-V31).
- **Atomic operations** -- LDXR/STXR for lock-free concurrent programming.
- **Exception handling** -- How ARM64 handles interrupts, page faults, and system calls at the hardware level.
- **Performance optimization** -- Instruction scheduling, cache effects, and branch prediction.

## Build Something

- **A simple shell** -- Read commands, fork processes, execute programs.
- **A bootloader** -- Write bare-metal ARM64 code that runs without an OS.
- **An emulator** -- Build an emulator for a simpler architecture (like CHIP-8) in ARM64.
- **Optimize a hot loop** -- Take a C function, look at the compiler output, and hand-optimize it.

## References

- [ARM Architecture Reference Manual](https://developer.arm.com/documentation/ddi0487/latest/) -- the definitive reference for ARM64.
- [ARM64 Instruction Set Quick Reference](https://developer.arm.com/documentation/100076/0100/) -- concise instruction reference.
- [Azeria Labs ARM Assembly Basics](https://azeria-labs.com/writing-arm-assembly-language/) -- excellent tutorials on ARM assembly.
- [ARM Assembly Internals and Reverse Engineering](https://www.wiley.com/en-us/ARM+Assembly+Internals+and+Reverse+Engineering-p-9781119745303) by Maria Markstedter -- comprehensive book.
- [Computer Organization and Design: ARM Edition](https://www.elsevier.com/books/computer-organization-and-design-arm-edition/patterson/978-0-12-801733-3) by Patterson and Hennessy.
`;

const cIntroductionContent = `
## Why C?

C is one of the most important programming languages in history. Created by Dennis Ritchie at Bell Labs in 1972, it is the language behind operating systems, embedded systems, databases, compilers, and countless other foundational software.

- **Close to the metal** -- C gives you direct access to memory and hardware. No garbage collector, no runtime overhead.
- **Portable** -- C runs on virtually every platform, from microcontrollers to supercomputers.
- **The lingua franca** -- Most other languages (Python, Ruby, Node.js) have runtimes written in C. Understanding C means understanding how software really works.
- **Simple but powerful** -- C has a small core: variables, functions, pointers, structs, and arrays. These simple building blocks compose into complex systems.

## The Story

Dennis Ritchie developed C at Bell Labs between 1969 and 1973, alongside the Unix operating system. C evolved from an earlier language called B (created by Ken Thompson), which itself descended from BCPL. The first edition of *The C Programming Language* by Brian Kernighan and Dennis Ritchie (known as K&R) was published in 1978 and became one of the most influential programming books ever written.

C was standardized by ANSI in 1989 (C89/C90) and has been updated several times since: C99, C11, C17, and C23.

## The ARM64 Assembly View

This course has a unique feature: when you run your C code, you can see the **ARM64 assembly** it compiles to. Click the **Assembly** tab after running to see the actual machine instructions generated by the TCC compiler.

This gives you a window into how your high-level C code translates to low-level processor instructions -- how variables become registers, how function calls become branch instructions, and how loops become conditional jumps.

## What You Will Learn

This course contains **18 lessons** organized into **7 chapters**:

1. **C Basics** -- Your first C program, variables, types, arithmetic, and type casting.
2. **Control Flow** -- Conditionals with \`if\`/\`else\`, loops with \`for\`/\`while\`, and \`switch\` statements.
3. **Enums and Bitwise** -- Enum declarations, named constants, and bitwise operators for flags and bit manipulation.
4. **Functions** -- Declaring functions, parameters, return values, and recursion.
5. **Arrays and Strings** -- Arrays, C-style strings, and multi-dimensional arrays.
6. **Pointers** -- Address-of, dereference, pointer arithmetic, and arrays as pointers.
7. **Structs** -- Defining structs, member access, and struct pointers with the arrow operator.

Each lesson explains a concept, demonstrates it with code examples, and gives you an exercise to practice. Your code is compiled to ARM64 by TCC running in your browser, then executed by our ARM64 interpreter.

Let's get started.
`;

const cWhatsNextContent = `
## Congratulations

You have completed all 18 lessons. You now have a solid foundation in C: variables, types, control flow, switch statements, enums, bitwise operators, functions, arrays, strings, pointers, and structs.

That is a real accomplishment. C is not easy, and understanding it gives you a deep appreciation for how software works at the system level.

## What to Explore Next

Here are topics to dive deeper into:

- **Dynamic memory allocation** -- \`malloc\`, \`free\`, \`calloc\`, \`realloc\` for heap-allocated memory.
- **File I/O** -- \`fopen\`, \`fread\`, \`fwrite\`, \`fprintf\` for reading and writing files.
- **Preprocessor** -- \`#define\`, \`#ifdef\`, \`#include\` for compile-time configuration.
- **Linked lists** -- Build dynamic data structures with structs and pointers.
- **Header files** -- Organize larger programs across multiple files.
- **Makefiles** -- Build systems for C projects.

## Build Something

The best way to learn is to build. Some project ideas:

- **A command-line calculator** -- parse and evaluate arithmetic expressions
- **A linked list library** -- insert, delete, search, and sort operations
- **A simple shell** -- read commands, fork processes, and execute programs
- **A memory allocator** -- implement your own \`malloc\` and \`free\`

## References

- *The C Programming Language* by Brian Kernighan and Dennis Ritchie (Prentice Hall, 1988) -- the definitive C book, still relevant today.
- [C Reference](https://en.cppreference.com/w/c) -- comprehensive online reference for the C standard library.
- [Beej's Guide to C Programming](https://beej.us/guide/bgc/) -- free online guide, well-written and practical.
- [Modern C](https://gustedt.gitlabpages.inria.fr/modern-c/) by Jens Gustedt -- a modern take on C programming.
- [CS:APP](https://csapp.cs.cmu.edu/) -- *Computer Systems: A Programmer's Perspective* covers how C maps to machine code.
`;

const gleamIntroductionContent = `
## Why Gleam?

Gleam is a friendly functional programming language with a powerful type system that catches mistakes before your code runs. It compiles to both Erlang and JavaScript, letting you build reliable software for any platform.

- **Type safe** -- Gleam's type system catches errors at compile time. No null pointer exceptions, no undefined is not a function. If it compiles, it works.
- **Functional** -- Immutable data, pattern matching, and the pipe operator make code easy to read and reason about.
- **Erlang VM** -- Gleam runs on the BEAM, the battle-tested virtual machine behind WhatsApp, Discord, and other systems serving millions of users.
- **JavaScript target** -- Gleam also compiles to JavaScript, so you can share code between server and client.
- **Friendly** -- Clear error messages, simple syntax, and a welcoming community. Gleam is designed to be a joy to use.

## The Story

Gleam was created by Louis Pilfold, who started working on it in 2018. Louis wanted a language that combined the reliability of the Erlang ecosystem with the developer experience of modern typed languages.

The Erlang VM is legendary for building fault-tolerant, concurrent systems -- it powers phone networks, messaging apps, and databases that need to stay up 24/7. But Erlang's dynamic typing and unusual syntax can be barriers for newcomers. Gleam bridges this gap with a familiar syntax and a strong type system, while giving you full access to the BEAM ecosystem.

Gleam reached version 1.0 in March 2024, marking its stability commitment.

## Who Uses Gleam

Gleam is a young but rapidly growing language:

- **Gleam itself** -- the Gleam compiler is written in Rust, and its standard library and package manager are written in Gleam.
- **Web applications** -- frameworks like Lustre (frontend) and Wisp (backend) let you build full-stack Gleam apps.
- **The BEAM ecosystem** -- Gleam interoperates seamlessly with Erlang and Elixir libraries.

## What You Will Learn

This course contains **16 lessons** organized into **7 chapters**:

1. **Foundations** -- How Gleam programs are structured: imports, functions, and printing output.
2. **Data Types** -- Strings, numbers, and the operations you can perform on them.
3. **Control Flow** -- Case expressions and pattern matching.
4. **Collections** -- Lists and tuples for grouping data.
5. **Custom Types** -- Sum types, records, and generics.
6. **Error Handling** -- The Result type and the use expression.
7. **Functional Patterns** -- The pipe operator and higher-order functions.

Each lesson explains a concept, demonstrates it with code examples, and gives you an exercise to practice.

Let's get started.
`;

const gleamWhatsNextContent = `
## Congratulations

You have completed all 16 lessons. You now have a solid foundation in Gleam: functions, types, pattern matching, custom types, error handling, pipes, and higher-order functions.

That is a real accomplishment. You understand the key ideas that make Gleam a powerful and enjoyable language.

## What to Explore Next

Here are topics to dive deeper into:

- **The BEAM** -- Learn about processes, message passing, and OTP for building fault-tolerant concurrent systems.
- **Lustre** -- A frontend framework for building interactive web apps in Gleam.
- **Wisp** -- A web framework for building backend services in Gleam.
- **Gleam OTP** -- Typed wrappers around Erlang's OTP behaviors.
- **JavaScript interop** -- Call JavaScript libraries from Gleam code.

## Build Something

The best way to learn is to build. Some project ideas:

- **A command-line tool** -- parse arguments, read files, and produce output
- **A web API** with Wisp -- routes, JSON responses, and database queries
- **A frontend app** with Lustre -- components, state management, and effects
- **A Discord bot** -- using Gleam on the BEAM for real-time message handling

## References

- [Gleam Language Tour](https://tour.gleam.run/) -- interactive guide to every Gleam feature.
- [Gleam Documentation](https://gleam.run/documentation/) -- official language documentation.
- [Gleam Standard Library](https://hexdocs.pm/gleam_stdlib/) -- API docs for the standard library.
- [Gleam Packages](https://packages.gleam.run/) -- the Gleam package index.
- [Awesome Gleam](https://github.com/gleam-lang/awesome-gleam) -- community-curated list of Gleam resources.
- [Gleam Discord](https://discord.gg/Fm8Pwmy) -- the official community chat.
`;

const rIntroductionContent = `
## Why R?

R is the language of statistics and data science. It was designed from the ground up for data analysis, visualization, and statistical computing. Here is what makes it stand out:

- **Built for data** -- Vectors, data frames, and matrices are first-class citizens. Data manipulation is natural and expressive.
- **Unmatched statistics** -- From t-tests to Bayesian inference, R has every statistical method built in or available through packages.
- **Visualization** -- R's plotting capabilities, especially ggplot2, produce publication-quality graphics with minimal code.
- **CRAN** -- The Comprehensive R Archive Network hosts over 20,000 packages covering every domain of data analysis.
- **Interactive analysis** -- R excels at exploratory data analysis with its REPL and notebook-style workflows.

## The Story

R was created by Ross Ihaka and Robert Gentleman at the University of Auckland, New Zealand, in 1993. The name "R" is a play on the creators' first names and a nod to S, the language that inspired it.

S was developed at Bell Labs by John Chambers in the 1970s as a language for statistical computing. R reimplemented S as free, open-source software and quickly surpassed it. The first stable release, R 1.0.0, came in February 2000.

Today, R is maintained by the R Core Team and has one of the most active open-source communities in data science.

## Who Uses R

R is the standard tool in many fields:

- **Academia** -- the dominant language for statistical research and publications.
- **Pharmaceutical industry** -- used for clinical trial analysis and FDA submissions.
- **Finance** -- risk modeling, time series analysis, and quantitative trading.
- **Tech companies** -- Google, Facebook, Microsoft, and Twitter use R for data analysis.

The tidyverse ecosystem, created by Hadley Wickham, has made R accessible to a much broader audience with packages like dplyr, ggplot2, and tidyr.

## What You Will Learn

This course contains **16 lessons** organized into **7 chapters**:

1. **Foundations** -- Printing output, variables, types, and arithmetic.
2. **Vectors** -- Creating vectors, vectorized operations, indexing, and filtering.
3. **Control Flow** -- Conditionals and loops.
4. **Functions** -- Defining functions, default arguments, closures, and higher-order functions.
5. **Data Structures** -- Lists, matrices, and data frames.
6. **Data Manipulation** -- Apply functions and data frame operations.
7. **Strings** -- String manipulation with paste, gsub, and sprintf.

Each lesson explains a concept, demonstrates it with code examples, and gives you an exercise to practice.

Let's get started.
`;

const rWhatsNextContent = `
## Congratulations

You have completed all 16 lessons. You now have a solid foundation in R: variables, vectors, control flow, functions, lists, matrices, data frames, apply functions, and string operations.

That is a real accomplishment. You understand the core building blocks that make R powerful for data analysis.

## What to Explore Next

Here are topics to dive deeper into:

- **ggplot2** -- The most popular R visualization package. Learn the grammar of graphics.
- **dplyr** -- Data manipulation with filter, select, mutate, summarize, and group_by.
- **tidyr** -- Reshape data with pivot_longer and pivot_wider.
- **R Markdown** -- Create reproducible reports combining code, output, and narrative.
- **Shiny** -- Build interactive web applications entirely in R.
- **Statistical modeling** -- Linear models, GLMs, and machine learning with caret or tidymodels.

## Build Something

The best way to learn is to build. Some project ideas:

- **An exploratory data analysis** -- download a dataset from Kaggle and analyze it with dplyr and ggplot2.
- **A statistical report** -- run hypothesis tests and build regression models in R Markdown.
- **A Shiny dashboard** -- create an interactive visualization app.
- **A data pipeline** -- clean, transform, and analyze a messy real-world dataset.

## References

- [R for Data Science](https://r4ds.hadley.nz/) by Hadley Wickham -- the best introduction to modern R.
- [Advanced R](https://adv-r.hadley.nz/) by Hadley Wickham -- deep dive into R's internals and programming patterns.
- [The R Manuals](https://cran.r-project.org/manuals.html) -- official documentation from the R Core Team.
- [CRAN Task Views](https://cran.r-project.org/web/views/) -- curated lists of packages by topic.
- [RStudio Cheatsheets](https://posit.co/resources/cheatsheets/) -- quick reference cards for popular packages.
- [R-bloggers](https://www.r-bloggers.com/) -- community blog aggregator for R content.
`;

const holycIntroductionContent = `
## Why HolyC?

HolyC is the programming language Terry A. Davis created for TempleOS — a complete operating system he built alone over more than a decade. It is a C dialect with unique features designed for simplicity, directness, and a close relationship between programmer and machine.

- **No preprocessor** -- HolyC has no \`#define\`. Metaprogramming is done with \`#exe\` blocks that run real code at compile time.
- **No \`continue\`** -- Intentionally omitted to encourage explicit control flow with \`goto\`.
- **Capital built-ins** -- \`Print\`, \`TRUE\`, \`FALSE\` and other built-ins are capitalized to distinguish them from user code.
- **Default arguments** -- Functions can have default parameter values at any position, unlike C.
- **Range switch cases** -- \`case 1...5:\` matches a range of values in a single case.
- **JIT model** -- HolyC compiles and runs immediately. There is no separate compile step and no \`main\` function required.
- **Classes** -- \`class\` replaces C's \`typedef struct\`, with cleaner syntax.

## The Story

Terry Davis began working on TempleOS around 2003 after a series of hospitalizations. He believed God had given him a specification for a "Third Temple" — a personal, direct interface between humans and the divine — implemented as an operating system.

TempleOS is 100,000 lines of HolyC, written entirely by one person. It includes its own compiler, filesystem (\`RedSea\`), 2D/3D graphics, music, and a Bible-verse-generating oracle. The entire system runs in ring 0 — there is no user/kernel separation, no memory protection, no networking. Every part of the system is equally accessible to every program.

Terry Davis died in 2018. TempleOS remains a singular achievement: a complete, working operating system, programming language, and development environment created by one human being.

## The Aiwnios Runtime

This course runs HolyC in your browser using **Aiwnios**, a reimplementation of the TempleOS HolyC compiler that runs on Linux (and, via WebAssembly, in browsers). Aiwnios supports HolyC's core syntax and built-ins, making it possible to learn the language without running TempleOS itself.

## What You Will Learn

This course contains **16 lessons** organized into **6 chapters**:

1. **The Temple** -- Your first HolyC program, printing output, and comments.
2. **Types & Variables** -- Integer types, floating point, booleans, and \`auto\`.
3. **Control Flow** -- \`if\`/\`else\`, switch with ranges, and loops without \`continue\`.
4. **Functions** -- Declaring functions, default arguments, and output parameters.
5. **Classes & Structures** -- Defining classes, member access, and inheritance patterns.
6. **Advanced HolyC** -- Arrays, pointers, and compile-time expressions with \`#exe\`.

Each lesson explains a concept, demonstrates it with code, and gives you an exercise to practice.

Let's get started.
`;

const holycWhatsNextContent = `
## Congratulations

You have completed all 16 lessons. You now understand HolyC's types, control flow, functions, classes, pointers, and compile-time metaprogramming.

That is a real accomplishment. HolyC is an unusual language with a singular history, and you have learned its core ideas.

## What to Explore Next

- **TempleOS itself** -- Download and run TempleOS in a virtual machine. Experience HolyC in its native environment: no mouse, no networking, 640×480 16-color display.
- **Aiwnios** -- Explore the Aiwnios source code on GitHub. It is a complete HolyC compiler and runtime written in C.
- **The TempleOS source** -- Read Terry's original HolyC code. It is available in the TempleOS distribution and on GitHub mirrors.
- **\`#exe\` metaprogramming** -- Explore how \`#exe\` blocks can generate data structures, precompute tables, and embed constants at compile time.
- **TempleOS graphics** -- The original TempleOS has a sprite editor, 3D flight simulator, and other programs written in HolyC.

## Build Something

- **A calculator** -- parse and evaluate arithmetic expressions in HolyC
- **A sorting visualizer** -- implement bubble sort or quicksort and print each step
- **A number theory program** -- primes, factorization, or the Collatz sequence
- **A text adventure** -- simple game logic using HolyC's \`switch\` and functions

## References

- [TempleOS on GitHub](https://github.com/cia-foundation/TempleOS) -- a mirror of the TempleOS source code.
- [Aiwnios on GitHub](https://github.com/Aiwnios/Aiwnios) -- the HolyC compiler used in this course.
- [TempleOS Wikipedia](https://en.wikipedia.org/wiki/TempleOS) -- background on the OS and its creator.
- [The TempleOS Online Documentation](https://templeos.org) -- Terry's original documentation, preserved online.
- [HolyC Overview](https://en.wikipedia.org/wiki/HolyC) -- a concise summary of the language's features.
`;

const coreutilsIntroductionContent = `
## Build the Tools You Use Every Day

Every command you ran in the Linux course — \`cat\`, \`grep\`, \`wc\`, \`head\`, \`tail\`, \`tr\`, \`uniq\` — is a C program. These programs are part of the **GNU coreutils** package, and they have been powering Unix systems for decades.

This course asks one question: **can you build them yourself?**

Each lesson gives you a coreutils command and asks you to implement its core logic as a C function. Not the full command with every flag and edge case — just the essential algorithm.

- \`cat\` → print a string character by character
- \`wc -l\` → count newlines
- \`grep\` → find lines containing a substring
- \`tail\` → print the last N lines
- \`tac\` → print lines in reverse order

Simple descriptions. Interesting implementations.

## Why This Matters

Writing these tools from scratch forces you to think about problems that are usually hidden:

- **How does \`wc -w\` know where one word ends and another begins?** A state machine.
- **How does \`tail\` know where the last N lines start without reading the file twice?** Scanning backwards.
- **How does \`grep\` check every position in a line?** A nested loop over all starting positions.

These are real algorithms running on millions of machines every day. You will understand them after implementing them.

## The Connection

This course is designed to follow the **Linux** and **C** courses on this platform:

- The Linux course taught you to *use* these commands from the shell.
- The C course taught you the language they are written in.
- This course connects both: rewrite the commands in C.

## What You Will Learn

This course contains **13 lessons** organized into **4 chapters**:

1. **Output** -- \`echo\`, \`cat\`, \`rev\`. Print strings, character by character, and reversed.
2. **Counting** -- \`wc -c\`, \`wc -l\`, \`wc -w\`. Count characters, lines, and words using pointer arithmetic and state machines.
3. **Filtering** -- \`head\`, \`tail\`, \`grep\`. Select lines from the start, end, or by pattern.
4. **Transformation** -- \`toupper\`, \`tr\`, \`uniq\`, \`tac\`. Modify and reorder text.

Each lesson explains the command, walks through the algorithm, and gives you a C function to implement and test.

Let's build coreutils.
`;

const coreutilsWhatsNextContent = `
## Congratulations

You have reimplemented 13 coreutils in C: \`echo\`, \`cat\`, \`rev\`, \`wc -c/l/w\`, \`head\`, \`tail\`, \`grep\`, \`toupper\`, \`tr\`, \`uniq\`, and \`tac\`.

That is real systems programming. These are the same algorithms that run on every Linux server in the world.

## What to Explore Next

You have built the core. Here are ways to go deeper:

- **Add the missing flags** -- \`head -n N\` with \`atoi\` to parse the argument; \`grep -i\` for case-insensitive matching; \`uniq -c\` to count occurrences.
- **Read from stdin** -- Real coreutils read from \`stdin\` (or files given as arguments). Add \`fgets\` or \`getchar\` loops to handle real input.
- **Handle multiple files** -- \`cat file1 file2\` opens and concatenates multiple files using \`fopen\`, \`fread\`, \`fclose\`.
- **Add error handling** -- What happens when a file does not exist? \`fprintf(stderr, ...)\` and non-zero exit codes.
- **Implement sort** -- Sorting lines requires either dynamic memory (\`malloc\`) or a fixed buffer. A classic exercise.

## Build Something Real

The ultimate exercise: **write a complete coreutil that works on your system**.

\`\`\`c
// mycat.c
#include <stdio.h>

int main(int argc, char *argv[]) {
    if (argc < 2) {
        // read from stdin
        int c;
        while ((c = getchar()) != EOF) putchar(c);
        return 0;
    }
    for (int i = 1; i < argc; i++) {
        FILE *f = fopen(argv[i], "r");
        if (!f) { fprintf(stderr, "mycat: %s: No such file\\n", argv[i]); return 1; }
        int c;
        while ((c = fgetc(f)) != EOF) putchar(c);
        fclose(f);
    }
    return 0;
}
\`\`\`

Compile with \`gcc mycat.c -o mycat\` and test it on real files. This is software you actually built, running on real hardware.

## References

- [GNU coreutils source code](https://github.com/coreutils/coreutils) -- Read the real implementations. \`src/cat.c\` is ~700 lines. \`src/wc.c\` handles UTF-8. See how far your implementations got.
- [The C Programming Language](https://www.cs.princeton.edu/~bwk/cbook.html) by Kernighan & Ritchie -- The original book. Chapter 7 on I/O is directly relevant.
- [Beej's Guide to C](https://beej.us/guide/bgc/) -- Free, comprehensive, practical.
- \`man 1 coreutils\` -- On any Linux system, \`man cat\`, \`man grep\`, etc. document every flag.
`;

const kernelIntroductionContent = `
## How Linux Works Under the Hood

You have used Linux commands. You have written C programs. Now go one level deeper: **how does the Linux kernel itself work?**

This course teaches Linux internals through C simulations. Every lesson implements a real kernel data structure or algorithm — not toy code, but the actual patterns from \`kernel/sched.c\`, \`mm/slab.c\`, \`fs/inode.c\`, and \`kernel/signal.c\`.

Since we run in the browser, we simulate rather than invoke real syscalls. But the data structures, algorithms, and logic are faithful to the real kernel.

## What the Kernel Does

The Linux kernel is the software layer between your programs and the hardware. Its four main jobs:

- **Process management** — create, schedule, and terminate processes
- **Memory management** — map virtual addresses to physical RAM, handle page faults
- **File system** — abstract disks into files and directories
- **IPC and signals** — let processes communicate and coordinate

Each chapter of this course covers one of these subsystems.

## What You Will Learn

This course contains **15 lessons** organized into **5 chapters**:

1. **Processes** — The Process Control Block (\`task_struct\`), state transitions, \`fork()\`, and round-robin scheduling.
2. **Memory** — Virtual address layout, page table walks, and the slab allocator.
3. **File System** — Inodes, file descriptor tables, and the File Allocation Table (FAT).
4. **IPC** — Ring buffers (the kernel pipe implementation), semaphores, and signal dispatch.
5. **System Calls** — The syscall dispatch table and \`sys_write\`.

Each lesson explains the concept, shows where it lives in the real kernel source, and gives you a C function to implement and test.

Let's read the kernel.
\`;

const kernelWhatsNextContent = \`
## Congratulations

You have implemented the core data structures of a real operating system:

- **Processes**: PCB, state machine, fork, round-robin scheduler
- **Memory**: virtual address parsing, page tables, slab allocator
- **File System**: inodes, file descriptor tables, FAT chains
- **IPC**: ring buffers, semaphores, signal dispatch tables
- **Syscalls**: syscall table, sys_write

These are not abstractions — they are the actual patterns used in \`linux/sched.h\`, \`mm/slab.c\`, \`fs/inode.h\`, and \`kernel/signal.c\`.

## Go Deeper

- **Read the kernel source** — [elixir.bootlin.com](https://elixir.bootlin.com/linux/latest/source) lets you browse the Linux source online with cross-references. Start with \`include/linux/sched.h\` (task_struct) and \`mm/slab.c\`.
- **Write a kernel module** — A kernel module is a C file you compile and load into a running kernel. The classic first module: a character device that returns "hello" when read.
- **Explore xv6** — MIT's teaching operating system, written in C for RISC-V. It implements everything you learned here in ~8,000 lines: [github.com/mit-pdos/xv6-riscv](https://github.com/mit-pdos/xv6-riscv).
- **Read Operating Systems: Three Easy Pieces** — Free online OS textbook covering virtualization, concurrency, and persistence in depth: [ostep.org](https://ostep.org).
- **Try the Linux kernel labs** — [linux-kernel-labs.github.io](https://linux-kernel-labs.github.io) has hands-on exercises for kernel development.

## References

- *Linux Kernel Development* by Robert Love — The best single-volume guide to the kernel internals.
- *Understanding the Linux Kernel* by Bovet & Cesati — Deep dive into kernel mechanisms.
- [The Linux Kernel documentation](https://www.kernel.org/doc/html/latest/) — Official docs, including subsystem-specific guides.
`;

const linuxIntroductionContent = `
## Why Linux?

Linux is the operating system that runs the world. It powers web servers, cloud infrastructure, Android phones, supercomputers, and embedded systems. If you write software professionally, you will work with Linux every day.

- **Free and open source** -- The kernel source code is public, auditable, and modifiable. You own your system.
- **Dominant in servers** -- Over 96% of the world's web servers run Linux. AWS, Google Cloud, and Azure are built on Linux.
- **The developer's OS** -- Package managers, shell scripting, SSH, containers, and deployment pipelines all assume Linux fluency.
- **Stable and secure** -- Linux powers critical infrastructure because it is reliable, configurable, and has a strong security model.

## The Story

Linus Torvalds was a 21-year-old Finnish student in 1991 when he announced on a Usenet newsgroup: "I'm doing a (free) operating system (just a hobby, won't be big and professional like GNU)." That project became the Linux kernel — today one of the largest collaborative software projects in history, with contributions from thousands of developers and companies including Google, Intel, Red Hat, and Microsoft.

The GNU Project, started by Richard Stallman in 1983, had already created most of the user-space tools (shell, compiler, coreutils). Combined with Torvalds' kernel, the result was a complete free operating system: GNU/Linux.

## The Shell

At the heart of Linux is the **shell** — a text interface where you type commands. The default shell on most systems is **Bash** (Bourne Again SHell). The shell is not just a way to run programs; it is a full scripting language for automating tasks.

This course runs your shell commands in an in-browser Linux simulator. You get a real shell experience — a virtual filesystem, common commands, pipes, variables, and scripting — without needing to install anything.

## What You Will Learn

This course contains **17 lessons** organized into **4 chapters**:

1. **The Shell** -- \`echo\`, \`pwd\`, \`ls\`, \`cd\`, and \`cat\`. The fundamental commands for navigating and reading files.
2. **Working with Files** -- \`mkdir\`, \`touch\`, \`cp\`, \`mv\`, and \`rm\`. Creating, copying, moving, and deleting files and directories.
3. **Text Processing** -- \`head\`, \`grep\`, pipes, and \`wc\`. Reading parts of files, searching, and combining commands with the pipe operator.
4. **Shell Scripting** -- Variables, \`for\` loops, and \`if\`/\`else\` conditionals. Writing scripts that automate work.

Each lesson explains a concept, demonstrates it with examples, and gives you an exercise to practice in the browser.

Let's get started.
`;

const linuxWhatsNextContent = `
## Congratulations

You have completed all 17 lessons. You now know how to navigate the filesystem, manage files, process text, and write shell scripts with variables, loops, and conditionals.

That is a real foundation. Linux fluency opens up a world of capabilities: automating repetitive tasks, managing servers, building deployment pipelines, and understanding how software systems work.

## What to Explore Next

Here are topics to dive deeper into:

- **Redirections** -- \`>\` and \`>>\` write stdout to files; \`<\` reads stdin from files; \`2>\` redirects stderr.
- **File permissions** -- \`chmod\`, \`chown\`, and understanding \`rwxr-xr-x\` in detail.
- **Processes** -- \`ps\`, \`top\`, \`kill\`, background jobs with \`&\`, and \`fg\`/\`bg\`.
- **Regular expressions** -- \`grep -E\`, \`sed\`, and \`awk\` for powerful text transformation.
- **SSH** -- Connecting to remote machines securely.
- **cron** -- Scheduling scripts to run at specific times.
- **Package management** -- \`apt\`, \`yum\`, or \`pacman\` depending on your distribution.
- **Environment variables** -- \`export\`, \`.bashrc\`, and \`.profile\`.

## Build Something

The best way to solidify Linux skills is to use them:

- **A backup script** -- automatically copy important files to a backup directory with timestamps
- **A log analyzer** -- parse a log file with \`grep\`, \`awk\`, and \`sort\` to find the most common errors
- **A project scaffolder** -- a script that creates a standard directory structure for new projects
- **A system monitor** -- display disk usage, memory, and CPU load in a readable format

## References

- [The Linux Command Line](https://linuxcommand.org/tlcl.php) by William Shotts -- free online book, comprehensive and well-written.
- [Bash Reference Manual](https://www.gnu.org/software/bash/manual/bash.html) -- the official Bash documentation.
- [TLDR Pages](https://tldr.sh/) -- community-maintained simplified man pages with practical examples.
- [Explain Shell](https://explainshell.com/) -- paste any shell command and get a visual breakdown of what each part does.
- [The Art of Command Line](https://github.com/jlevy/the-art-of-command-line) -- a curated guide of command-line tips and tricks.
- [Linux Journey](https://linuxjourney.com/) -- interactive Linux learning with exercises.
`;

const jsIntroductionContent = `
## Why JavaScript?

JavaScript is the language of the web. It runs in every browser, powers millions of websites and apps, and is consistently one of the most popular programming languages in the world.

- **Everywhere** -- Every browser runs JavaScript natively. No install, no compile step, no runtime to set up. Open the console and start coding.
- **Full-stack** -- With Node.js, JavaScript runs on servers too. One language for frontend, backend, scripting, and tooling.
- **Huge ecosystem** -- npm hosts over 2 million packages. Whatever you need to build, there is probably a library for it.
- **Fast enough** -- Modern JavaScript engines (V8, SpiderMonkey) use JIT compilation. JavaScript is fast enough for games, real-time apps, and data processing.
- **Expressive** -- First-class functions, closures, destructuring, and the rest of the modern ES6+ syntax make JavaScript surprisingly elegant.

## The Story

JavaScript was created by Brendan Eich in just 10 days in May 1995 while working at Netscape. Originally called Mocha, then LiveScript, it was renamed JavaScript as a marketing move to capitalize on Java's popularity — despite having almost nothing to do with Java.

The language was standardized in 1997 as ECMAScript (ES1). For years, browser incompatibilities made JavaScript painful to write. The release of ES6 (ES2015) transformed the language: \`let\`, \`const\`, arrow functions, classes, modules, Promises, and much more arrived all at once.

Today, JavaScript evolves yearly, with new features added through TC39, the standards committee. Engines like Google's V8 (used in Chrome and Node.js) make it one of the fastest dynamic languages in existence.

## Who Uses JavaScript

Practically everyone in web development:

- **React, Vue, Angular** -- the dominant frontend frameworks are all JavaScript.
- **Node.js** -- JavaScript on the server, powering Netflix, LinkedIn, and Uber.
- **Electron** -- VS Code, Slack, and Figma are built with JavaScript.
- **React Native** -- mobile apps from a single JavaScript codebase.

## What You Will Learn

This course contains **15 lessons** organized into **5 chapters**:

1. **Basics** -- \`console.log\`, variables with \`let\` and \`const\`, and string operations.
2. **Control Flow** -- Conditionals with \`if\`/\`else\`, \`for\` loops, and \`while\` loops.
3. **Functions** -- Declaring functions, arrow functions, and closures.
4. **Arrays** -- Creating and modifying arrays, then \`map\`, \`filter\`, and \`reduce\`.
5. **Objects** -- Key-value pairs, property access, and destructuring.

Each lesson explains a concept, shows examples, and gives you an exercise to write and run in your browser.

Let's get started.
`;

const jsWhatsNextContent = `
## Congratulations

You have completed all 15 lessons. You now know JavaScript's core language: variables, types, control flow, functions, closures, arrays, higher-order functions, objects, and destructuring.

That is a real foundation. You can read and write JavaScript, understand most code you encounter, and start building your own projects.

## What to Explore Next

Here are the natural next steps:

- **The DOM** -- \`document.querySelector\`, event listeners, and updating the page dynamically. This is how JavaScript makes web pages interactive.
- **Fetch API** -- Make HTTP requests with \`fetch()\` to load data from APIs. Return JSON and display it on a page.
- **Promises and async/await** -- Handle asynchronous operations cleanly. Essential for working with APIs and databases.
- **ES6+ Features** -- Spread (\`...\`), rest parameters, optional chaining (\`?.\`), nullish coalescing (\`??\`), and template literals.
- **Modules** -- \`import\` and \`export\` to organize code across files.
- **TypeScript** -- Add static types to JavaScript for better tooling and fewer bugs at scale.

## Build Something

The best way to learn is to build. Some project ideas:

- **A todo app** -- add, complete, and delete tasks. Store them in localStorage.
- **A weather app** -- fetch data from a public weather API and display it.
- **A quiz game** -- questions and answers, score tracking, and a timer.
- **A markdown previewer** -- type markdown on the left, see HTML on the right.

## References

- [MDN Web Docs](https://developer.mozilla.org/en-US/docs/Web/JavaScript) -- the definitive JavaScript reference. Every built-in function, every API, with examples.
- [javascript.info](https://javascript.info/) -- the best free JavaScript tutorial. Comprehensive, modern, well-explained.
- [Eloquent JavaScript](https://eloquentjavascript.net/) by Marijn Haverbeke -- a free book that goes deep into the language.
- [You Don't Know JS](https://github.com/getify/You-Dont-Know-JS) by Kyle Simpson -- six books explaining JavaScript's quirks and internals in depth.
- [The Modern JavaScript Tutorial](https://javascript.info/) -- practical, modern, and thorough.
`;

const tsIntroductionContent = `
## Why TypeScript?

TypeScript is JavaScript with static types. It is a superset of JavaScript — any valid JavaScript is valid TypeScript. The type system catches errors at compile time, before your code ever runs.

- **Catch bugs early** -- Type errors are caught when you write code, not when users hit them. A mistyped property name, a wrong argument type, or a missing return value are all caught before deployment.
- **Better tooling** -- TypeScript enables powerful autocomplete, inline documentation, and safe refactoring in any editor. The type system knows what properties an object has.
- **Scales with your codebase** -- As projects grow, types make code easier to understand and modify safely. Large teams rely on TypeScript to work on shared codebases without stepping on each other.
- **Gradual adoption** -- You can add TypeScript to an existing JavaScript project one file at a time. The \`strict: false\` option lets you start loose and tighten up incrementally.
- **The industry standard** -- React, Angular, Vue, Node.js, and virtually every major JavaScript framework are written in or have first-class TypeScript support.

## The Story

TypeScript was created at Microsoft by Anders Hejlsberg — the original designer of C# and Delphi. It was announced in October 2012 after being developed internally for two years.

Microsoft's motivation was practical: they were building large applications in JavaScript and finding it increasingly hard to maintain at scale. TypeScript was their answer — a typed layer that compiles away entirely, leaving standard JavaScript.

The project went open source from day one on GitHub. It gained widespread adoption in the JavaScript ecosystem, especially after Angular 2 (2016) made TypeScript its primary language. Today, TypeScript is consistently one of the most loved and most used programming languages.

## Who Uses TypeScript

The JavaScript ecosystem has largely adopted TypeScript:

- **Microsoft** -- VS Code, Teams, and Office Online are built with TypeScript.
- **Google** -- Angular is TypeScript-first.
- **Airbnb** -- migrated their entire frontend to TypeScript.
- **Slack, Asana, Figma, Stripe** -- all use TypeScript in production.

The DefinitelyTyped repository hosts type definitions for over 8,000 JavaScript libraries, making TypeScript work with the entire npm ecosystem.

## What You Will Learn

This course contains **15 lessons** organized into **6 chapters**:

1. **Basics** -- Type annotations for variables, arrays, and function parameters and return types.
2. **Interfaces** -- Defining object shapes with interfaces, type aliases, and optional properties.
3. **Type System** -- Union types, type narrowing with \`typeof\`, and literal types.
4. **Generics** -- Generic functions and generic constraints with \`extends\` and \`keyof\`.
5. **Classes** -- TypeScript classes with access modifiers, and enums.
6. **Advanced** -- Readonly properties and type guard functions.

Each lesson explains a concept, demonstrates it with code examples, and gives you an exercise to practice.

Let's get started.
`;

const tsWhatsNextContent = `
## Congratulations

You have completed all 15 lessons. You now have a solid foundation in TypeScript: type annotations, interfaces, union types, type narrowing, generics, classes, enums, readonly, and type guards.

That is a real accomplishment. You understand the core type system features that make TypeScript valuable at scale.

## What to Explore Next

Here are topics to dive deeper into:

- **Utility Types** -- \`Partial<T>\`, \`Required<T>\`, \`Pick<T, K>\`, \`Omit<T, K>\`, \`Record<K, V>\`, \`ReturnType<F>\` — TypeScript's built-in type transformations.
- **Mapped Types** -- Transform every property of a type: \`{ [K in keyof T]: ... }\`.
- **Conditional Types** -- \`T extends U ? X : Y\` for type-level branching.
- **Template Literal Types** -- String manipulation at the type level: \`\\\`get\${Capitalize<string>}\\\`\`.
- **Declaration Files** -- Write \`.d.ts\` files to add types to JavaScript libraries.
- **Strict Mode** -- Enable \`strict: true\` for stricter null checking and better safety.
- **Decorators** -- Metadata annotations used heavily in Angular and NestJS.

## Build Something

The best way to learn is to build. Some project ideas:

- **A typed REST API client** -- define types for requests and responses, with generics for different endpoints
- **A state machine** -- use discriminated unions to model application states safely
- **A type-safe event emitter** -- generics and mapped types to enforce correct event handler signatures
- **Migrate a JavaScript project** -- take an existing JS project and add TypeScript types incrementally

## References

- [TypeScript Handbook](https://www.typescriptlang.org/docs/handbook/intro.html) -- the official guide, comprehensive and well-written.
- [TypeScript Playground](https://www.typescriptlang.org/play) -- write and run TypeScript in the browser, see the compiled JavaScript.
- [Type Challenges](https://github.com/type-challenges/type-challenges) -- a collection of type system puzzles to sharpen your skills.
- [Total TypeScript](https://www.totaltypescript.com/) by Matt Pocock -- advanced TypeScript patterns explained clearly.
- [Effective TypeScript](https://effectivetypescript.com/) by Dan Vanderkam -- 62 specific ways to improve your TypeScript.
- *Programming TypeScript* by Boris Cherny (O'Reilly, 2019) -- a comprehensive book on TypeScript for JavaScript developers.
`;

const algorithmsIntroductionContent = `
## Why Study Algorithms?

Algorithms are the foundation of all software. Every program you write — whether it sorts a list, searches a database, or finds a route on a map — relies on well-understood algorithms. Learning them makes you a better programmer at every level.

- **Speed matters** -- A naive O(n²) sort on 1 million elements takes seconds. An O(n log n) sort takes milliseconds. The algorithm is the difference.
- **Interview ready** -- Algorithms are the core of technical interviews at every top tech company.
- **Problem solving** -- Algorithm design teaches systematic thinking: how to decompose problems, identify patterns, and reason about correctness.
- **Universal knowledge** -- These algorithms run on every language, every platform, every architecture.

## The Canon

The algorithms in this course are not academic curiosities. They are the ones that power real software:

- **Merge sort** is used in Python's \`sorted()\` and Java's \`Arrays.sort()\`.
- **Quick sort** underlies C's \`qsort\` and C++'s \`std::sort\`.
- **BFS** finds shortest paths in Google Maps and social network friend suggestions.
- **Dijkstra's algorithm** powers GPS navigation and network routing protocols.
- **Dynamic programming** is used in sequence alignment (bioinformatics), spell correction, and compiler optimization.

## What You Will Learn

This course contains **15 lessons** organized into **5 chapters**:

1. **Sorting** -- Bubble sort, selection sort, insertion sort, merge sort, and quick sort. Understand the trade-offs between O(n²) and O(n log n) algorithms.
2. **Searching** -- Linear search for unsorted data, binary search for sorted data. The power of O(log n).
3. **Data Structures** -- Stack (LIFO), Queue (FIFO), and Linked List. The building blocks for more complex algorithms.
4. **Graphs** -- Breadth-first search (BFS), depth-first search (DFS), and Dijkstra's shortest path algorithm.
5. **Dynamic Programming** -- Memoization, the Fibonacci sequence, and the Longest Common Subsequence problem.

Each lesson explains the algorithm, shows the code, and gives you an exercise to implement it.

Let's get started.
`;

const algorithmsWhatsNextContent = `
## Congratulations

You have completed all 15 lessons. You now understand sorting algorithms, binary search, stacks, queues, linked lists, graph traversal, and dynamic programming.

That is a real accomplishment. These algorithms appear in virtually every technical interview and underlie most software systems.

## What to Explore Next

You have covered the fundamentals. Here is where to go deeper:

- **Heaps and Priority Queues** -- Efficient O(log n) min/max operations. The foundation for Dijkstra with a priority queue and heap sort.
- **Hash Tables** -- O(1) average lookup, insertion, and deletion. The most used data structure in practice.
- **Binary Search Trees** -- O(log n) search, insert, delete for dynamic sorted data.
- **Balanced BSTs (AVL, Red-Black)** -- Guaranteed O(log n) even with adversarial inputs.
- **Tries** -- Efficient prefix searches for autocomplete and spell checking.
- **Union-Find** -- Efficient connected components and Kruskal's minimum spanning tree.
- **Topological Sort** -- Ordering dependencies in build systems and package managers.
- **Bellman-Ford** -- Shortest paths with negative weights.

## Practice

Theory is not enough — you need to practice on real problems:

- [LeetCode](https://leetcode.com/) -- the most popular competitive programming platform. Start with "Easy" problems.
- [NeetCode](https://neetcode.io/) -- curated problem lists with video explanations, organized by pattern.
- [AlgoExpert](https://www.algoexpert.io/) -- 160 handpicked questions with video solutions.
- [Project Euler](https://projecteuler.net/) -- mathematical and computational problems that reward algorithmic thinking.

## References

- *Introduction to Algorithms* (CLRS) by Cormen, Leiserson, Rivest, and Stein -- the definitive textbook. Dense but comprehensive.
- *The Algorithm Design Manual* by Steven Skiena -- practical and readable. Excellent war stories from real-world algorithm use.
- *Grokking Algorithms* by Aditya Bhargava -- illustrated introduction, great for visual learners.
- [Visualgo](https://visualgo.net/) -- visual animations of sorting, graph, and DP algorithms.
- [Big-O Cheat Sheet](https://www.bigocheatsheet.com/) -- quick reference for time and space complexity.
`;

const distributedSystemsIntroductionContent = `
## Why Distributed Systems?

When a single machine is not enough — too slow, too small, or too unreliable — you distribute the workload across multiple machines. This is how every major internet service operates: Google, Amazon, Netflix, and WhatsApp all run on distributed systems.

- **Scale** -- distribute load across hundreds or thousands of machines. No single machine can serve a billion users.
- **Fault tolerance** -- when one machine fails (and it will), the system keeps running. Replication and consensus protocols keep data safe.
- **Latency** -- serve users from servers close to them. Data replicated globally means fast reads everywhere.

But distribution introduces hard problems: machines fail, networks partition, clocks drift, and messages are lost or delayed. Every distributed system must make trade-offs between consistency, availability, and partition tolerance — the famous **CAP theorem**.

## What You Will Build

In this course, you will implement the core algorithms and data structures that power real distributed systems — in JavaScript:

- **Lamport clocks and vector clocks** — how to order events when there is no global clock.
- **Consistent hashing** — how DynamoDB and Cassandra distribute data across nodes without reshuffling everything when a node is added or removed.
- **Leader election** — how Kafka and Kubernetes elect a coordinator.
- **Quorum-based replication** — how to guarantee strong consistency with N replicas.
- **CRDTs** — conflict-free data structures that merge automatically in eventually consistent systems.
- **Bloom filters** — probabilistic membership tests used in Cassandra, Chrome, and Bitcoin.
- **Rate limiting** — the token bucket algorithm used by every production API.
- **Circuit breakers** — the pattern that prevents cascading failures in microservices.
- **LRU caches** — the eviction policy behind CPU caches, Redis, and database buffer pools.
- **Gossip protocols** — how Cassandra and DynamoDB propagate cluster membership.
- **Two-phase commit** — the atomic protocol for distributed transactions.

## What You Will Learn

This course contains **12 lessons** organized into **6 chapters**:

1. **Clocks & Ordering** -- Lamport clocks and vector clocks for logical time.
2. **Data Distribution** -- Consistent hashing for distributing data across nodes.
3. **Consensus** -- Leader election and two-phase commit for coordination.
4. **Replication** -- Quorum-based operations, G-Counter CRDTs, and gossip protocols.
5. **Probabilistic Structures** -- Bloom filters for space-efficient membership testing.
6. **Fault Tolerance** -- Rate limiting, circuit breakers, and LRU caching.

Each lesson explains the concept, shows the algorithm, and gives you an exercise to implement it in JavaScript.

Let's get started.
`;

const distributedSystemsWhatsNextContent = `
## Congratulations

You have completed all 12 lessons. You now understand Lamport clocks, vector clocks, consistent hashing, leader election, two-phase commit, quorums, CRDTs, gossip protocols, Bloom filters, rate limiting, circuit breakers, and LRU caches.

These are the algorithms and patterns that power Amazon, Google, Netflix, and every other major distributed system.

## What to Explore Next

You have covered the foundational algorithms. Here is where to go deeper:

- **Raft consensus** -- The readable alternative to Paxos. A leader is elected per term; all writes go through the leader. Implemented by etcd (Kubernetes), CockroachDB, and TiKV.
- **Paxos** -- The original consensus protocol by Lamport. Notoriously difficult to understand but foundational to Google Chubby and Spanner.
- **MVCC (Multi-Version Concurrency Control)** -- How databases like PostgreSQL and CockroachDB implement snapshot isolation without locking.
- **Saga pattern** -- Managing long-running distributed transactions without 2PC.
- **Consistent Hashing with Virtual Nodes** -- How Cassandra and DynamoDB use virtual nodes for better load balancing.
- **Replication logs** -- WAL shipping, statement-based replication, and row-based replication.

## Build Something

- **A key-value store** -- implement a simple distributed key-value store with consistent hashing and replication.
- **A rate-limited API server** -- build a Node.js server with per-client rate limiting backed by Redis.
- **A Raft implementation** -- implement the Raft leader election protocol across multiple processes.

## References

- *Designing Data-Intensive Applications* by Martin Kleppmann (O'Reilly, 2017) -- the best book on distributed systems for practitioners. Covers replication, partitioning, transactions, consistency, and consensus with clarity and depth.
- [The Raft Paper](https://raft.github.io/raft.pdf) -- "In Search of an Understandable Consensus Algorithm" by Ongaro and Ousterhout. Readable and complete.
- [Amazon DynamoDB Paper](https://www.allthingsdistributed.com/files/amazon-dynamo-sosp2007.pdf) -- the original Dynamo paper. Consistent hashing, quorums, and vector clocks in production.
- [Google Bigtable Paper](https://research.google/pubs/bigtable-a-distributed-storage-system-for-structured-data/) -- how Google stores petabytes of structured data.
- [Jepsen](https://jepsen.io/) -- Kyle Kingsbury's analyses of consistency and failure modes in distributed databases.
- [Martin Kleppmann's Blog](https://martin.kleppmann.com/) -- deep dives into distributed systems research.
`;

const rubyIntroductionContent = `
## Why Ruby?

Ruby is a language designed for programmer happiness. Matz (Yukihiro Matsumoto) created it in 1995 with a single guiding principle: make programming enjoyable. The result is a language that reads almost like English, rewards elegance, and gets out of your way.

- **Expressive** -- Ruby code is concise and readable. A task that takes 10 lines in Java often takes 2 in Ruby.
- **Everything is an object** -- In Ruby, even integers and booleans are objects with methods. \`5.times\`, \`"hello".upcase\`, \`[1,2,3].map\`.
- **Flexible** -- Ruby embraces the idea that there is more than one way to do things. Blocks, procs, lambdas, open classes — the language bends to you.
- **Rails** -- Ruby on Rails, the web framework created by DHH, revolutionized web development. GitHub, Shopify, Basecamp, and Airbnb were all built on Rails.
- **Great for beginners** -- Ruby's syntax is forgiving and the error messages are helpful. It is one of the best first languages.

## The Story

Yukihiro "Matz" Matsumoto began designing Ruby in 1993 and released the first version in 1995. He was inspired by Perl, Smalltalk, Eiffel, Ada, and Lisp — taking what he loved from each.

Ruby remained relatively obscure outside Japan until 2004, when David Heinemeier Hansson (DHH) released Ruby on Rails. Rails showed what Ruby was capable of: a web application framework that made building CRUD apps dramatically faster. The Rails community exploded, and Ruby exploded with it.

Today, Ruby is mature, stable, and widely used in web development, scripting, and DevOps tooling. The MRI (Matz's Ruby Interpreter) is the reference implementation, but JRuby and TruffleRuby bring Ruby to the JVM and GraalVM.

## Who Uses Ruby

Ruby powers some of the world's most recognizable products:

- **GitHub** -- the largest code hosting platform in the world, built on Rails.
- **Shopify** -- e-commerce platform serving over a million merchants, running on a massive Rails monolith.
- **Basecamp** -- the original Rails app, still running on Ruby.
- **Airbnb** -- started on Rails and still uses Ruby extensively.
- **Stripe** -- uses Ruby for parts of its payment infrastructure.

## What You Will Learn

This course contains **15 lessons** organized into **5 chapters**:

1. **Basics** -- \`puts\`, variables, strings with interpolation, and string methods.
2. **Numbers** -- Integer and Float arithmetic, comparison operators, and boolean logic.
3. **Collections** -- Arrays, Hashes, and Ranges — Ruby's three core collection types.
4. **Control Flow** -- Conditionals with \`if\`/\`unless\`, loops with \`while\`/\`until\`, and iterators: \`each\`, \`map\`, \`select\`, \`reduce\`.
5. **Methods & OOP** -- Defining methods with default parameters, blocks and \`yield\`, classes with instance variables, and modules as mixins.

Each lesson explains a concept, shows examples, and gives you an exercise to write and run in your browser.

Let's get started.
`;

const rubyWhatsNextContent = `
## Congratulations

You have completed all 15 lessons. You now know Ruby's core: variables, strings, numbers, arrays, hashes, ranges, control flow, iterators, methods, blocks, classes, and modules.

That is a real foundation. You can read Ruby code, write Ruby programs, and start exploring the vast Ruby ecosystem.

## What to Explore Next

Here are the natural next steps:

- **Ruby on Rails** -- The most popular Ruby framework. Build web applications with models, views, controllers, and a database in hours. Start with the [Rails Guides](https://guides.rubyonrails.org/).
- **RubyGems** -- Ruby's package manager. Over 100,000 gems available. \`gem install\` and \`require\` to add any library.
- **Symbols and frozen strings** -- Ruby's performance optimization for immutable string-like values.
- **Procs and Lambdas** -- First-class functions in Ruby. Understand the difference between \`Proc.new\`, \`proc {}\`, and \`lambda {}\`.
- **Comparable and Enumerable** -- Two of Ruby's most useful modules. Include them in your classes to get dozens of free methods.
- **File I/O** -- \`File.read\`, \`File.write\`, and processing files line by line.
- **Regular expressions** -- Ruby has native regex support: \`"hello" =~ /e(l+)o/\`.
- **Testing** -- RSpec and Minitest, the two dominant testing frameworks. TDD is a core Ruby community value.

## Build Something

The best way to learn is to build:

- **A command-line tool** -- process CSV files, generate reports, or automate a repetitive task.
- **A web scraper** -- use the Nokogiri gem to parse HTML and extract data from websites.
- **A REST API** -- use Sinatra (a lightweight framework) to build a simple HTTP API.
- **A Rails app** -- follow the official Rails tutorial and build a blog or to-do app with a real database.

## References

- [The Ruby Programming Language](https://www.oreilly.com/library/view/the-ruby-programming/9780596516178/) by Matz and David Flanagan (O'Reilly) -- the definitive Ruby book.
- [Programming Ruby (the Pickaxe)](https://pragprog.com/titles/ruby5/programming-ruby-3-3-5th-edition/) by Dave Thomas -- the classic Ruby reference.
- [Ruby Documentation](https://ruby-doc.org/) -- official API docs for the standard library.
- [RubyGems](https://rubygems.org/) -- the Ruby package repository.
- [Ruby on Rails Guides](https://guides.rubyonrails.org/) -- comprehensive guides for the most popular Ruby framework.
- [The Odin Project](https://www.theodinproject.com/paths/full-stack-ruby-on-rails) -- free, comprehensive full-stack curriculum using Ruby and Rails.
`;

const treesIntroductionContent = `
## Why Trees?

Trees are the most important data structure beyond arrays and linked lists. They appear everywhere in software:

- **File systems** — directories are trees. Every \`ls\` you run traverses one.
- **The DOM** — every webpage is a tree of HTML elements.
- **Compilers** — source code is parsed into an Abstract Syntax Tree (AST) before execution.
- **Databases** — B-trees power the indexes that make SQL queries fast.
- **Git** — the commit graph is a tree (actually a DAG). Every \`git log\` traverses it.
- **DNS** — the domain name system is a tree. \`com\` → \`example\` → \`www\`.

Understanding trees is the gateway to understanding how real systems work at depth.

## Why C?

Implementing trees in C forces you to understand them at the lowest level:

- There is no garbage collector. You call \`malloc\` to create a node, and you own that memory.
- There is no tree library. You write the struct, the insert, the traversal — everything.
- The pointer manipulation is explicit. \`root->left = new_node(val)\` is not hidden behind an object system.

When you implement a BST in C, you understand what Python's \`dict\` and Java's \`TreeMap\` are doing under the hood.

## What You Will Learn

This course contains **12 lessons** organized into **4 chapters**:

1. **Binary Trees** — Define the Node struct, create nodes with malloc, and implement all three classic traversals: inorder, preorder, and postorder.
2. **Tree Properties** — Recursive algorithms to count nodes, measure height, and count leaves.
3. **Binary Search Tree** — Insert values, search for values, and find min/max using the BST property.
4. **BST Operations** — Validate that a tree is a correct BST using the min-max technique.

Each lesson explains the concept, walks through the algorithm, and gives you a function to implement and test.

Let's build trees.
`;

const treesWhatsNextContent = `
## Congratulations

You have implemented 12 tree algorithms in C: node creation, all three traversals, node count, height, leaf count, sum, BST insert, BST search, min/max, and BST validation.

That is real data structures knowledge — the kind that comes up in every technical interview and underpins every database and compiler ever written.

## What to Explore Next

You have covered the fundamentals. Here is where to go deeper:

- **BST Delete** — The hardest BST operation. When deleting a node with two children, replace it with its inorder successor (the smallest node in its right subtree).
- **Balanced BSTs (AVL Trees)** — A BST stays O(log n) only if it's balanced. AVL trees self-balance on insert and delete using rotations.
- **Red-Black Trees** — The self-balancing BST used by Linux's process scheduler, Java's \`TreeMap\`, and C++'s \`std::map\`.
- **B-Trees** — N-ary trees optimized for disk access. The structure behind every database index.
- **Tries** — Trees for string prefixes. Used in autocomplete, spell checkers, and IP routing tables.
- **Heaps** — A tree with a different invariant (parent ≤ children). The structure behind priority queues.

## Build Something

- **A sorted set** — use a BST to build a dynamic sorted container with insert, search, delete, and iteration.
- **An expression evaluator** — parse arithmetic expressions into a tree and evaluate them recursively.
- **A file system simulator** — implement a tree of directories and files with path traversal.
- **A spell checker** — build a trie from a dictionary and check words character by character.

## References

- *Introduction to Algorithms* (CLRS) by Cormen, Leiserson, Rivest, and Stein — Chapter 12 covers BSTs; Chapters 13-14 cover Red-Black Trees and augmented data structures.
- *The Algorithm Design Manual* by Steven Skiena — practical discussion of tree variants and when to use each.
- [Visualgo - BST](https://visualgo.net/en/bst) — animated visualization of BST insert, delete, and search.
- [CS50x Data Structures](https://cs50.harvard.edu/x/) — Harvard's free course covers trees with excellent C examples.
`;

export const courses: Course[] = [
  {
    id: "go",
    title: "Go",
    description: "Learn the Go programming language from scratch. Build fast, reliable software with Go's simple syntax, built-in concurrency, and powerful standard library.",
    language: "go",
    chapters: goChapters,
    lessons: goLessons,
    pdfPath: "/go-book.pdf",
    runtimeLabel: "Go runtime",
    introductionContent: goIntroductionContent,
    whatsNextContent: goWhatsNextContent,
  },
  {
    id: "zig",
    title: "Zig",
    description: "Learn the Zig programming language from scratch. Master manual memory management, comptime metaprogramming, and zero-cost abstractions.",
    language: "zig",
    chapters: zigChapters,
    lessons: zigLessons,
    runtimeLabel: "Zig runtime",
    introductionContent: zigIntroductionContent,
    whatsNextContent: zigWhatsNextContent,
  },
  {
    id: "postgresql",
    title: "PostgreSQL",
    description: "Learn SQL and PostgreSQL from scratch. Master queries, joins, aggregations, window functions, and database design with a live database in your browser.",
    language: "sql",
    chapters: sqlChapters,
    lessons: sqlLessons,
    runtimeLabel: "SQL runtime",
    introductionContent: sqlIntroductionContent,
    whatsNextContent: sqlWhatsNextContent,
  },
  {
    id: "arm64",
    title: "ARM64 Assembly",
    description: "Learn ARM64 assembly language from scratch. Understand how processors execute code with registers, memory, branches, and syscalls.",
    language: "arm64",
    chapters: arm64Chapters,
    lessons: arm64Lessons,
    runtimeLabel: "ARM64 runtime",
    introductionContent: arm64IntroductionContent,
    whatsNextContent: arm64WhatsNextContent,
  },
  {
    id: "c",
    title: "C",
    description: "Learn the C programming language from scratch. See the ARM64 assembly your code compiles to, compiled and executed entirely in your browser.",
    language: "c",
    chapters: cChapters,
    lessons: cLessons,
    runtimeLabel: "TCC compiler",
    introductionContent: cIntroductionContent,
    whatsNextContent: cWhatsNextContent,
  },
  {
    id: "gleam",
    title: "Gleam",
    description: "Learn the Gleam programming language from scratch. Build type-safe, functional programs with pattern matching, the pipe operator, and the Result type.",
    language: "gleam",
    chapters: gleamChapters,
    lessons: gleamLessons,
    runtimeLabel: "Gleam runtime",
    introductionContent: gleamIntroductionContent,
    whatsNextContent: gleamWhatsNextContent,
  },
  {
    id: "r",
    title: "R",
    description: "Learn the R programming language from scratch. Master vectors, data frames, functions, and data manipulation with R's powerful built-in tools.",
    language: "r",
    chapters: rChapters,
    lessons: rLessons,
    runtimeLabel: "R runtime",
    introductionContent: rIntroductionContent,
    whatsNextContent: rWhatsNextContent,
  },
  {
    id: "holyc",
    title: "HolyC",
    description: "Learn HolyC, the programming language Terry Davis created for TempleOS. Explore range switches, default arguments, compile-time #exe blocks, and a direct relationship with the machine.",
    language: "holyc",
    chapters: holycChapters,
    lessons: holycLessons,
    runtimeLabel: "Aiwnios HolyC",
    introductionContent: holycIntroductionContent,
    whatsNextContent: holycWhatsNextContent,
  },
  {
    id: "coreutils",
    title: "Coreutils in C",
    description: "Reimplement the classic Unix command-line tools in C. Build echo, cat, grep, wc, head, tail, tr, uniq, and more from scratch — and understand how they work.",
    language: "c",
    chapters: coreutilsChapters,
    lessons: coreutilsLessons,
    runtimeLabel: "TCC compiler",
    introductionContent: coreutilsIntroductionContent,
    whatsNextContent: coreutilsWhatsNextContent,
  },
  {
    id: "javascript",
    title: "JavaScript",
    description: "Learn JavaScript from scratch. Master variables, functions, closures, arrays, and objects — the foundation of the web's most popular language.",
    language: "javascript",
    chapters: jsChapters,
    lessons: jsLessons,
    runtimeLabel: "JavaScript",
    introductionContent: jsIntroductionContent,
    whatsNextContent: jsWhatsNextContent,
  },
  {
    id: "typescript",
    title: "TypeScript",
    description: "Learn TypeScript from scratch. Master type annotations, interfaces, generics, and the type system that makes large-scale JavaScript development reliable.",
    language: "typescript",
    chapters: tsChapters,
    lessons: tsLessons,
    runtimeLabel: "TypeScript",
    introductionContent: tsIntroductionContent,
    whatsNextContent: tsWhatsNextContent,
  },
  {
    id: "linux",
    title: "Linux",
    description: "Learn Linux and the Bash shell from scratch. Master the command line: navigate the filesystem, manage files, process text with pipes, and write shell scripts.",
    language: "bash",
    chapters: linuxChapters,
    lessons: linuxLessons,
    runtimeLabel: "Linux shell",
    introductionContent: linuxIntroductionContent,
    whatsNextContent: linuxWhatsNextContent,
  },
  {
    id: "algorithms",
    title: "Algorithms",
    description: "Learn essential algorithms from scratch. Master sorting, searching, graph traversal, and dynamic programming — the foundations of every technical interview and production system.",
    language: "javascript",
    chapters: algorithmsChapters,
    lessons: algorithmsLessons,
    runtimeLabel: "JavaScript",
    introductionContent: algorithmsIntroductionContent,
    whatsNextContent: algorithmsWhatsNextContent,
  },
  {
    id: "distributed-systems",
    title: "Distributed Systems",
    description: "Learn distributed systems by implementing them. Build Lamport clocks, consistent hashing, quorum replication, CRDTs, Bloom filters, circuit breakers, and more — in JavaScript.",
    language: "javascript",
    chapters: distributedSystemsChapters,
    lessons: distributedSystemsLessons,
    runtimeLabel: "JavaScript",
    introductionContent: distributedSystemsIntroductionContent,
    whatsNextContent: distributedSystemsWhatsNextContent,
  },
  {
    id: "ruby",
    title: "Ruby",
    description: "Learn Ruby from scratch. Master variables, strings, arrays, hashes, iterators, classes, and modules — the expressive language behind Rails, GitHub, and Shopify.",
    language: "ruby",
    chapters: rubyChapters,
    lessons: rubyLessons,
    runtimeLabel: "ruby.wasm",
    introductionContent: rubyIntroductionContent,
    whatsNextContent: rubyWhatsNextContent,
  },
  {
    id: "trees",
    title: "Trees in C",
    description: "Learn tree data structures by implementing them in C. Build binary trees, master inorder/preorder/postorder traversals, and implement a Binary Search Tree from scratch.",
    language: "c",
    chapters: treesChapters,
    lessons: treesLessons,
    runtimeLabel: "TCC compiler",
    introductionContent: treesIntroductionContent,
    whatsNextContent: treesWhatsNextContent,
  },
  {
    id: "kernel",
    title: "Linux Internals",
    description: "Understand how the Linux kernel works by implementing its core data structures in C. Build a process scheduler, page table, slab allocator, ring buffer, semaphore, signal table, and syscall dispatcher.",
    language: "c",
    chapters: kernelChapters,
    lessons: kernelLessons,
    runtimeLabel: "TCC compiler",
    introductionContent: kernelIntroductionContent,
    whatsNextContent: kernelWhatsNextContent,
  },
];

export function getCourse(id: string): Course | undefined {
  return courses.find((c) => c.id === id);
}

export function getCourseOrThrow(id: string): Course {
  const course = getCourse(id);
  if (!course) throw new Error(`Course not found: ${id}`);
  return course;
}
