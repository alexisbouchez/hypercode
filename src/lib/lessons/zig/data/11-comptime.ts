import type { Lesson } from "../../types";

export const comptime: Lesson = {
  id: "comptime",
  title: "Comptime",
  chapterId: "advanced",
  content: `## Compile-Time Evaluation

Zig's \`comptime\` feature is one of the language's most powerful tools. It allows you to run arbitrary Zig code at compile time, eliminating the need for macros, code generation, or template metaprogramming. The same language you use at runtime works at compile time.

### The comptime Keyword

When you mark a variable or expression with \`comptime\`, Zig evaluates it during compilation rather than at runtime:

\`\`\`zig
const x = comptime blk: {
    var result: i32 = 0;
    for (0..10) |i| {
        result += @as(i32, @intCast(i));
    }
    break :blk result;
};
// x is 45, computed at compile time
\`\`\`

### comptime Parameters

Functions can accept \`comptime\` parameters. These must be known at compile time, and the compiler generates specialized code for each unique value:

\`\`\`zig
fn repeat(comptime n: usize, value: i32) [n]i32 {
    return [_]i32{value} ** n;
}

const arr = repeat(5, 42); // [42, 42, 42, 42, 42]
\`\`\`

Notice that the return type uses \`n\` --- a comptime parameter can appear in types. This is how Zig achieves generics without a separate generics syntax.

### Type as a First-Class Value

In Zig, types are values of type \`type\`, and they can be manipulated at compile time:

\`\`\`zig
fn maxValue(comptime T: type) T {
    return switch (@typeInfo(T)) {
        .int => |info| if (info.signedness == .signed)
            @as(T, (1 << (info.bits - 1)) - 1)
        else
            @as(T, @bitCast(@as(std.meta.Int(.unsigned, info.bits), std.math.maxInt(T)))),
        else => @compileError("unsupported type"),
    };
}
\`\`\`

More practically, you can write generic data structures:

\`\`\`zig
fn Pair(comptime T: type) type {
    return struct {
        first: T,
        second: T,
    };
}

const IntPair = Pair(i32);
const p = IntPair{ .first = 10, .second = 20 };
\`\`\`

### Compile-Time Conditionals

Since \`if\` and \`switch\` can operate on comptime values, you get conditional compilation without preprocessor directives:

\`\`\`zig
fn toBytes(comptime T: type, value: T) [@sizeOf(T)]u8 {
    return @bitCast(value);
}
\`\`\`

### comptime vs const

It is important to understand the distinction:
- \`const\` means the variable cannot be reassigned, but its value might be computed at runtime
- \`comptime\` means the value must be known at compile time

\`\`\`zig
const runtime_val: i32 = someRuntimeFunction(); // OK, value computed at runtime
comptime var ct_val: i32 = 42;                   // Must be comptime-known
\`\`\`

### Practical Uses

Comptime is used everywhere in Zig:
- **Format strings** in \`std.debug.print\` are comptime-checked
- **Generic types** like \`ArrayList(T)\` use comptime type parameters
- **Compile-time validation** catches errors before your program ever runs
- **Lookup tables** can be precomputed at compile time

### Your Task

Write a function \`comptime fn fibonacci(comptime n: u32) u32\` that computes the nth Fibonacci number at compile time (F(0) = 0, F(1) = 1, F(n) = F(n-1) + F(n-2)).

Also write a function \`fn comptimePow(comptime base: i64, comptime exp: u32) i64\` that computes \`base\` raised to the power \`exp\` at compile time.`,

  starterCode: `const std = @import("std");

fn fibonacci(comptime n: u32) comptime_int {
\t// Your code here: return the nth Fibonacci number
\t// F(0) = 0, F(1) = 1, F(n) = F(n-1) + F(n-2)
\t_ = n;
\treturn 0;
}

fn comptimePow(comptime base: i64, comptime exp: u32) i64 {
\t// Your code here: return base^exp
\t_ = base;
\t_ = exp;
\treturn 0;
}

pub fn main() !void {
\tconst fib10 = comptime fibonacci(10);
\tstd.debug.print("{}\\n", .{fib10});

\tconst result = comptime comptimePow(2, 10);
\tstd.debug.print("{}\\n", .{result});
}
`,

  solution: `const std = @import("std");

fn fibonacci(comptime n: u32) comptime_int {
\tif (n == 0) return 0;
\tif (n == 1) return 1;
\treturn fibonacci(n - 1) + fibonacci(n - 2);
}

fn comptimePow(comptime base: i64, comptime exp: u32) i64 {
\tif (exp == 0) return 1;
\treturn base * comptimePow(base, exp - 1);
}

pub fn main() !void {
\tconst fib10 = comptime fibonacci(10);
\tstd.debug.print("{}\\n", .{fib10});

\tconst result = comptime comptimePow(2, 10);
\tstd.debug.print("{}\\n", .{result});
}
`,

  tests: [
    {
      name: "fibonacci(0) is 0",
      code: `const std = @import("std");

{{FUNC}}

pub fn main() !void {
\tconst result = comptime fibonacci(0);
\tstd.debug.print("{}\\n", .{result});
}
`,
      expected: "0\n",
    },
    {
      name: "fibonacci(10) is 55",
      code: `const std = @import("std");

{{FUNC}}

pub fn main() !void {
\tconst result = comptime fibonacci(10);
\tstd.debug.print("{}\\n", .{result});
}
`,
      expected: "55\n",
    },
    {
      name: "comptimePow(2, 10) is 1024",
      code: `const std = @import("std");

{{FUNC}}

pub fn main() !void {
\tconst result = comptime comptimePow(2, 10);
\tstd.debug.print("{}\\n", .{result});
}
`,
      expected: "1024\n",
    },
    {
      name: "comptimePow(3, 0) is 1",
      code: `const std = @import("std");

{{FUNC}}

pub fn main() !void {
\tconst result = comptime comptimePow(3, 0);
\tstd.debug.print("{}\\n", .{result});
}
`,
      expected: "1\n",
    },
  ],
};
