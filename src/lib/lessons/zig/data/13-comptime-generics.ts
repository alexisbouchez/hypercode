import type { Lesson } from "../../types";

export const comptimeGenerics: Lesson = {
  id: "comptime-generics",
  title: "Comptime Generics",
  chapterId: "advanced",
  content: `## Generic Programming via Comptime

Zig does not have a separate generics syntax like C++ templates or Java generics. Instead, it uses \`comptime\` type parameters to achieve the same result --- and more, since you can run arbitrary code at compile time to generate types and functions.

### Generic Functions

A generic function accepts a \`comptime T: type\` parameter and uses \`T\` in its signature and body:

\`\`\`zig
fn max(comptime T: type, a: T, b: T) T {
    return if (a > b) a else b;
}

// Usage:
const m = max(i32, 10, 20); // 20
const n = max(f64, 3.14, 2.71); // 3.14
\`\`\`

The compiler generates a specialized version for each type used. There is no runtime overhead.

### Generic Data Structures

Functions that return \`type\` are Zig's way of defining generic structs. The returned struct is a completely normal type:

\`\`\`zig
fn Stack(comptime T: type) type {
    return struct {
        items: [256]T = undefined,
        len: usize = 0,

        const Self = @This();

        pub fn push(self: *Self, value: T) void {
            self.items[self.len] = value;
            self.len += 1;
        }

        pub fn pop(self: *Self) T {
            self.len -= 1;
            return self.items[self.len];
        }
    };
}

var stack = Stack(i32){};
stack.push(42);
const val = stack.pop(); // 42
\`\`\`

### @TypeOf and Type Reflection

\`@TypeOf(expr)\` returns the compile-time type of any expression. Combined with \`@typeInfo\`, you can inspect types and branch on their properties:

\`\`\`zig
fn isInteger(comptime T: type) bool {
    return switch (@typeInfo(T)) {
        .int, .comptime_int => true,
        else => false,
    };
}
\`\`\`

\`@TypeOf\` is particularly useful for writing helper functions that adapt to their input:

\`\`\`zig
fn double(value: anytype) @TypeOf(value) {
    return value * 2;
}

const a = double(@as(i32, 5));   // i32: 10
const b = double(@as(f64, 2.5)); // f64: 5.0
\`\`\`

### anytype Parameters

The \`anytype\` keyword tells the compiler to infer the type from the call site. It is syntactic sugar that avoids an explicit \`comptime T: type\` parameter when you only need one inferred type:

\`\`\`zig
fn debugPrint(value: anytype) void {
    const T = @TypeOf(value);
    if (@typeInfo(T) == .int) {
        std.debug.print("int: {}\\n", .{value});
    } else {
        std.debug.print("other\\n", .{});
    }
}
\`\`\`

### Your Task

1. Write a generic function \`fn Pair(comptime T: type) type\` that returns a struct with two fields: \`first: T\` and \`second: T\`, and a method \`pub fn sum(self: @This()) T\` that returns \`self.first + self.second\`.

2. Write a function \`fn typeName(comptime T: type) []const u8\` that returns \`"integer"\` if \`T\` is an integer type, \`"float"\` if it is a floating-point type, or \`"other"\` otherwise. Use \`@typeInfo\` to inspect the type.`,

  starterCode: `const std = @import("std");

fn Pair(comptime T: type) type {
\treturn struct {
\t\tfirst: T,
\t\tsecond: T,

\t\t// Add a sum method that returns first + second
\t};
}

fn typeName(comptime T: type) []const u8 {
\t// Return "integer", "float", or "other" based on T
\t_ = T;
\treturn "other";
}

pub fn main() !void {
\tconst p = Pair(i32){ .first = 10, .second = 20 };
\tstd.debug.print("{}\\n", .{p.sum()});

\tstd.debug.print("{s}\\n", .{typeName(i32)});
\tstd.debug.print("{s}\\n", .{typeName(f64)});
\tstd.debug.print("{s}\\n", .{typeName(bool)});
}
`,

  solution: `const std = @import("std");

fn Pair(comptime T: type) type {
\treturn struct {
\t\tfirst: T,
\t\tsecond: T,

\t\tpub fn sum(self: @This()) T {
\t\t\treturn self.first + self.second;
\t\t}
\t};
}

fn typeName(comptime T: type) []const u8 {
\treturn switch (@typeInfo(T)) {
\t\t.int, .comptime_int => "integer",
\t\t.float, .comptime_float => "float",
\t\telse => "other",
\t};
}

pub fn main() !void {
\tconst p = Pair(i32){ .first = 10, .second = 20 };
\tstd.debug.print("{}\\n", .{p.sum()});

\tstd.debug.print("{s}\\n", .{typeName(i32)});
\tstd.debug.print("{s}\\n", .{typeName(f64)});
\tstd.debug.print("{s}\\n", .{typeName(bool)});
}
`,

  tests: [
    {
      name: "Pair(i32).sum() returns 30",
      code: `const std = @import("std");

{{FUNC}}

pub fn main() !void {
\tconst p = Pair(i32){ .first = 10, .second = 20 };
\tstd.debug.print("{}\\n", .{p.sum()});
}
`,
      expected: "30\n",
    },
    {
      name: "typeName identifies integer, float, and other",
      code: `const std = @import("std");

{{FUNC}}

pub fn main() !void {
\tstd.debug.print("{s}\\n", .{typeName(i32)});
\tstd.debug.print("{s}\\n", .{typeName(f64)});
\tstd.debug.print("{s}\\n", .{typeName(bool)});
}
`,
      expected: "integer\nfloat\nother\n",
    },
  ],
};
