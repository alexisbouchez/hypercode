import type { Lesson } from "../../types";

export const arraysAndSlices: Lesson = {
  id: "arrays-and-slices",
  title: "Arrays & Slices",
  chapterId: "data-structures",
  content: `## Fixed-Size Arrays and Flexible Slices

Zig provides two closely related sequence types: **arrays** with a compile-time known length, and **slices** that represent a view into a contiguous block of memory. Understanding the distinction is essential to writing correct Zig code.

### Arrays

An array has a fixed size that is part of its type. \`[5]u8\` is a completely different type from \`[3]u8\`. The size is always known at compile time:

\`\`\`zig
// Array literal with explicit type
const a: [5]u8 = .{ 1, 2, 3, 4, 5 };

// The compiler can infer the length
const b = [_]u8{ 10, 20, 30 };

// An array of 100 zeros
const zeros = [_]u8{0} ** 100;

// An array initialized with a fill value
var buf: [256]u8 = undefined;
\`\`\`

You access elements with bracket notation, and the length is available through the \`len\` field:

\`\`\`zig
const arr = [_]i32{ 10, 20, 30, 40, 50 };
const first = arr[0];        // 10
const length = arr.len;      // 5
\`\`\`

Zig performs bounds checking at runtime in safe build modes. Accessing \`arr[5]\` on a 5-element array would trigger a panic, not silent undefined behavior.

### Slices

A slice is a pointer-and-length pair that refers to a portion of an array (or any contiguous memory). The type \`[]const u8\` means "a slice of constant bytes." Slices do not own their memory; they borrow it.

\`\`\`zig
const arr = [_]i32{ 1, 2, 3, 4, 5 };

// Slice the entire array
const all: []const i32 = &arr;

// Slice a subrange: indices 1, 2, 3 (end is exclusive)
const middle: []const i32 = arr[1..4];
\`\`\`

A mutable slice uses \`[]T\` instead of \`[]const T\`:

\`\`\`zig
var data = [_]u8{ 'h', 'e', 'l', 'l', 'o' };
var s: []u8 = &data;
s[0] = 'H';  // modifies the original array
\`\`\`

### Slice Length

Just like arrays, slices have a \`len\` field:

\`\`\`zig
const nums = [_]i32{ 10, 20, 30, 40, 50 };
const s = nums[1..4];
std.debug.print("length: {}\\n", .{s.len}); // length: 3
\`\`\`

### Iterating Over Arrays and Slices

Use a \`for\` loop to iterate. You can capture both the element and an optional index:

\`\`\`zig
const items = [_]i32{ 10, 20, 30 };
for (items) |item| {
    std.debug.print("{}\\n", .{item});
}

// With index
for (items, 0..) |item, i| {
    std.debug.print("[{}] = {}\\n", .{ i, item });
}
\`\`\`

### Passing Arrays to Functions

Functions typically accept slices rather than fixed-size arrays. This makes them generic over any length:

\`\`\`zig
fn sum(values: []const i32) i32 {
    var total: i32 = 0;
    for (values) |v| {
        total += v;
    }
    return total;
}
\`\`\`

To pass an array to a function that takes a slice, use the address-of operator \`&\`:

\`\`\`zig
const arr = [_]i32{ 1, 2, 3 };
const result = sum(&arr);
\`\`\`

### Your Task

Write two functions:

- \`sumSlice(values: []const i32) i32\` --- returns the sum of all elements in the slice.
- \`countNonZero(values: []const i32) usize\` --- returns the number of elements that are not zero.`,

  starterCode: `const std = @import("std");

fn sumSlice(values: []const i32) i32 {
\t// Your code here
\treturn 0;
}

fn countNonZero(values: []const i32) usize {
\t// Your code here
\treturn 0;
}

pub fn main() !void {
\tconst arr = [_]i32{ 1, 2, 0, 4, 0, 6 };
\tstd.debug.print("{}\\n", .{sumSlice(&arr)});
\tstd.debug.print("{}\\n", .{countNonZero(&arr)});
}
`,

  solution: `const std = @import("std");

fn sumSlice(values: []const i32) i32 {
\tvar total: i32 = 0;
\tfor (values) |v| {
\t\ttotal += v;
\t}
\treturn total;
}

fn countNonZero(values: []const i32) usize {
\tvar count: usize = 0;
\tfor (values) |v| {
\t\tif (v != 0) {
\t\t\tcount += 1;
\t\t}
\t}
\treturn count;
}

pub fn main() !void {
\tconst arr = [_]i32{ 1, 2, 0, 4, 0, 6 };
\tstd.debug.print("{}\\n", .{sumSlice(&arr)});
\tstd.debug.print("{}\\n", .{countNonZero(&arr)});
}
`,

  tests: [
    {
      name: "sumSlice of {1, 2, 0, 4, 0, 6}",
      code: `const std = @import("std");

{{FUNC}}

pub fn main() !void {
\tconst arr = [_]i32{ 1, 2, 0, 4, 0, 6 };
\tstd.debug.print("{}\\n", .{sumSlice(&arr)});
}
`,
      expected: "13\n",
    },
    {
      name: "sumSlice of empty slice",
      code: `const std = @import("std");

{{FUNC}}

pub fn main() !void {
\tconst arr = [_]i32{};
\tstd.debug.print("{}\\n", .{sumSlice(&arr)});
}
`,
      expected: "0\n",
    },
    {
      name: "countNonZero of {1, 2, 0, 4, 0, 6}",
      code: `const std = @import("std");

{{FUNC}}

pub fn main() !void {
\tconst arr = [_]i32{ 1, 2, 0, 4, 0, 6 };
\tstd.debug.print("{}\\n", .{countNonZero(&arr)});
}
`,
      expected: "4\n",
    },
    {
      name: "countNonZero of all zeros",
      code: `const std = @import("std");

{{FUNC}}

pub fn main() !void {
\tconst arr = [_]i32{ 0, 0, 0 };
\tstd.debug.print("{}\\n", .{countNonZero(&arr)});
}
`,
      expected: "0\n",
    },
  ],
};
