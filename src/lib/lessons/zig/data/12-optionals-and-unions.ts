import type { Lesson } from "../../types";

export const optionalsAndUnions: Lesson = {
  id: "optionals-and-unions",
  title: "Optionals & Unions",
  chapterId: "advanced",
  content: `## Handling Absence and Variant Types

Zig provides two powerful type system features for modeling data that can take on different forms: **optionals** for values that might be absent, and **tagged unions** for values that can be one of several types. Together, they eliminate entire categories of bugs that plague C and similar languages.

### Optional Types

An optional type \`?T\` can hold either a value of type \`T\` or \`null\`. This is Zig's replacement for null pointers and sentinel values:

\`\`\`zig
var maybe_number: ?i32 = 42;
maybe_number = null;  // Now it holds no value
\`\`\`

### Unwrapping Optionals with orelse

The \`orelse\` operator provides a default value when the optional is \`null\`:

\`\`\`zig
const value: ?i32 = null;
const result = value orelse 0;  // result is 0

const other: ?i32 = 42;
const result2 = other orelse 0;  // result2 is 42
\`\`\`

### Unwrapping with if

You can use \`if\` to unwrap an optional and access its payload:

\`\`\`zig
const maybe: ?i32 = 42;

if (maybe) |val| {
    std.debug.print("Got: {}\\n", .{val});
} else {
    std.debug.print("Nothing\\n", .{});
}
\`\`\`

The variable \`val\` only exists inside the \`if\` block and contains the unwrapped value. This pattern guarantees you never accidentally use \`null\`.

> Like Schrodinger's tribble, an optional value both exists and does not exist until you unwrap it. At least in Zig, the compiler makes sure you check before petting it.

### Optional Pointers

Optional pointers \`?*T\` are particularly useful. Unlike C, where any pointer might be null, Zig's \`*T\` is guaranteed non-null. When you need a nullable pointer, you opt in explicitly with \`?*T\`:

\`\`\`zig
fn findFirst(items: []const i32, target: i32) ?usize {
    for (items, 0..) |item, i| {
        if (item == target) return i;
    }
    return null;
}
\`\`\`

### Tagged Unions

A tagged union can hold one of several types, and Zig tracks which one is active. This is similar to sum types or algebraic data types in functional languages:

\`\`\`zig
const Shape = union(enum) {
    circle: f64,       // radius
    rectangle: struct { width: f64, height: f64 },
    triangle: struct { base: f64, height: f64 },
};
\`\`\`

### Creating Union Values

\`\`\`zig
const s1 = Shape{ .circle = 5.0 };
const s2 = Shape{ .rectangle = .{ .width = 4.0, .height = 3.0 } };
\`\`\`

### Switching on Unions

The \`switch\` statement is the primary way to work with tagged unions. Zig requires that you handle every variant, preventing you from forgetting a case:

\`\`\`zig
fn area(shape: Shape) f64 {
    return switch (shape) {
        .circle => |r| std.math.pi * r * r,
        .rectangle => |rect| rect.width * rect.height,
        .triangle => |tri| 0.5 * tri.base * tri.height,
    };
}
\`\`\`

If you add a new variant to the union later, the compiler will force you to handle it in every switch statement. This is one of the strongest guarantees a type system can provide.

### Combining Optionals and Unions

You can return optional union values, giving you expressive APIs:

\`\`\`zig
fn parseCommand(input: []const u8) ?Command {
    if (std.mem.eql(u8, input, "quit")) return .quit;
    return null;
}
\`\`\`

### Your Task

Write a function \`safeDivide(a: i32, b: i32) ?i32\` that returns \`a / b\` as an optional. If \`b\` is zero, return \`null\`.

Also write a function \`describeNumber(n: i32) []const u8\` that returns \`"positive"\`, \`"negative"\`, or \`"zero"\` depending on the value of \`n\`.`,

  starterCode: `const std = @import("std");

fn safeDivide(a: i32, b: i32) ?i32 {
\t// Your code here: return null if b is zero
\t_ = a;
\t_ = b;
\treturn null;
}

fn describeNumber(n: i32) []const u8 {
\t// Your code here: return "positive", "negative", or "zero"
\t_ = n;
\treturn "unknown";
}

pub fn main() !void {
\tconst result = safeDivide(10, 3);
\tif (result) |val| {
\t\tstd.debug.print("{}\\n", .{val});
\t} else {
\t\tstd.debug.print("division by zero\\n", .{});
\t}

\tstd.debug.print("{s}\\n", .{describeNumber(42)});
}
`,

  solution: `const std = @import("std");

fn safeDivide(a: i32, b: i32) ?i32 {
\tif (b == 0) return null;
\treturn @divTrunc(a, b);
}

fn describeNumber(n: i32) []const u8 {
\tif (n > 0) return "positive";
\tif (n < 0) return "negative";
\treturn "zero";
}

pub fn main() !void {
\tconst result = safeDivide(10, 3);
\tif (result) |val| {
\t\tstd.debug.print("{}\\n", .{val});
\t} else {
\t\tstd.debug.print("division by zero\\n", .{});
\t}

\tstd.debug.print("{s}\\n", .{describeNumber(42)});
}
`,

  tests: [
    {
      name: "safeDivide(10, 3) is 3",
      code: `const std = @import("std");

{{FUNC}}

pub fn main() !void {
\tconst result = safeDivide(10, 3) orelse -1;
\tstd.debug.print("{}\\n", .{result});
}
`,
      expected: "3\n",
    },
    {
      name: "safeDivide(10, 0) is null",
      code: `const std = @import("std");

{{FUNC}}

pub fn main() !void {
\tconst result = safeDivide(10, 0);
\tif (result) |_| {
\t\tstd.debug.print("has value\\n", .{});
\t} else {
\t\tstd.debug.print("null\\n", .{});
\t}
}
`,
      expected: "null\n",
    },
    {
      name: "describeNumber(42) is positive",
      code: `const std = @import("std");

{{FUNC}}

pub fn main() !void {
\tstd.debug.print("{s}\\n", .{describeNumber(42)});
}
`,
      expected: "positive\n",
    },
    {
      name: "describeNumber(-5) is negative",
      code: `const std = @import("std");

{{FUNC}}

pub fn main() !void {
\tstd.debug.print("{s}\\n", .{describeNumber(-5)});
}
`,
      expected: "negative\n",
    },
    {
      name: "describeNumber(0) is zero",
      code: `const std = @import("std");

{{FUNC}}

pub fn main() !void {
\tstd.debug.print("{s}\\n", .{describeNumber(0)});
}
`,
      expected: "zero\n",
    },
  ],
};
