import type { Lesson } from "../../types";

export const functions: Lesson = {
  id: "functions",
  title: "Functions",
  chapterId: "functions-chapter",
  content: `## Functions in Zig

Functions are declared with \`fn\`, followed by the name, parameters, and return type:

\`\`\`zig
fn add(a: i32, b: i32) i32 {
    return a + b;
}
\`\`\`

Parameter types come after the parameter name, separated by a colon. Every parameter must have an explicit type. The return type follows the closing parenthesis.

### Public Functions

By default, functions are private to their file. The \`pub\` keyword makes them accessible from other modules:

\`\`\`zig
pub fn greet(name: []const u8) void {
    std.debug.print("Hello, {s}!\\n", .{name});
}
\`\`\`

### Void Return Type

Functions that do not return a value use \`void\`:

\`\`\`zig
fn logMessage(msg: []const u8) void {
    std.debug.print("{s}\\n", .{msg});
}
\`\`\`

### Implicit Return

The last expression in a function can be returned without the \`return\` keyword if the function body is a block expression. However, in practice, using explicit \`return\` statements is more common and idiomatic in Zig.

### Comptime Parameters

Zig has a powerful feature called \`comptime\`. You can mark parameters as compile-time known:

\`\`\`zig
fn multiply(comptime T: type, a: T, b: T) T {
    return a * b;
}

const result = multiply(i32, 5, 3); // 15
\`\`\`

This is Zig's approach to generics. Instead of a separate generics syntax, you pass types as compile-time parameters. The compiler generates specialized code for each type.

> Like Scotty's engineering protocols on the Enterprise, every function should have a clear mission, well-defined inputs, and a predictable output. "She cannae take any more, Captain!" --- unless the function signature says she can.

### Function Pointers

Functions are first-class values. You can store them in variables and pass them as arguments:

\`\`\`zig
fn double(x: i32) i32 {
    return x * 2;
}

fn apply(f: *const fn (i32) i32, value: i32) i32 {
    return f(value);
}

const result = apply(double, 5); // 10
\`\`\`

### Multiple Return Values via Structs

Zig does not have built-in multiple return values like Go. Instead, you return a struct:

\`\`\`zig
const MinMax = struct { min: i32, max: i32 };

fn minMax(values: []const i32) MinMax {
    var min = values[0];
    var max = values[0];
    for (values[1..]) |v| {
        if (v < min) min = v;
        if (v > max) max = v;
    }
    return .{ .min = min, .max = max };
}
\`\`\`

### Your Task

Write two functions:

1. \`max\` - takes two \`i32\` parameters and returns the larger one as \`i32\`
2. \`clamp\` - takes three \`i32\` parameters (\`value\`, \`lower\`, \`upper\`) and returns the value clamped to the range \`[lower, upper]\`. If \`value < lower\`, return \`lower\`. If \`value > upper\`, return \`upper\`. Otherwise, return \`value\`.

Hint: you can call \`max\` from within \`clamp\`, or write \`clamp\` independently.`,

  starterCode: `const std = @import("std");

pub fn max(a: i32, b: i32) i32 {
\t// Your code here
\treturn 0;
}

pub fn clamp(value: i32, lower: i32, upper: i32) i32 {
\t// Your code here
\treturn 0;
}

pub fn main() void {
\tstd.debug.print("max(3, 7) = {d}\\n", .{max(3, 7)});
\tstd.debug.print("clamp(15, 0, 10) = {d}\\n", .{clamp(15, 0, 10)});
}
`,

  solution: `const std = @import("std");

pub fn max(a: i32, b: i32) i32 {
\tif (a > b) {
\t\treturn a;
\t}
\treturn b;
}

pub fn clamp(value: i32, lower: i32, upper: i32) i32 {
\tif (value < lower) {
\t\treturn lower;
\t}
\tif (value > upper) {
\t\treturn upper;
\t}
\treturn value;
}

pub fn main() void {
\tstd.debug.print("max(3, 7) = {d}\\n", .{max(3, 7)});
\tstd.debug.print("clamp(15, 0, 10) = {d}\\n", .{clamp(15, 0, 10)});
}
`,

  tests: [
    {
      name: "max(3, 7) returns 7",
      code: `const std = @import("std");

{{FUNC}}

pub fn main() void {
\tstd.debug.print("{d}\\n", .{max(3, 7)});
}`,
      expected: "7\n",
    },
    {
      name: "max(-2, -5) returns -2",
      code: `const std = @import("std");

{{FUNC}}

pub fn main() void {
\tstd.debug.print("{d}\\n", .{max(-2, -5)});
}`,
      expected: "-2\n",
    },
    {
      name: "clamp(15, 0, 10) returns 10",
      code: `const std = @import("std");

{{FUNC}}

pub fn main() void {
\tstd.debug.print("{d}\\n", .{clamp(15, 0, 10)});
}`,
      expected: "10\n",
    },
    {
      name: "clamp(-5, 0, 10) returns 0",
      code: `const std = @import("std");

{{FUNC}}

pub fn main() void {
\tstd.debug.print("{d}\\n", .{clamp(-5, 0, 10)});
}`,
      expected: "0\n",
    },
    {
      name: "clamp(5, 0, 10) returns 5 (within range)",
      code: `const std = @import("std");

{{FUNC}}

pub fn main() void {
\tstd.debug.print("{d}\\n", .{clamp(5, 0, 10)});
}`,
      expected: "5\n",
    },
  ],
};
