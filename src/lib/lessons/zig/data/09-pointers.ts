import type { Lesson } from "../../types";

export const pointers: Lesson = {
  id: "pointers",
  title: "Pointers",
  chapterId: "memory",
  content: `## Direct Memory Access with Pointers

Zig gives you direct control over memory through pointers. Unlike C, Zig's pointer system is designed with safety in mind: there are no null pointers for single-item pointers, pointer arithmetic is explicit, and the type system distinguishes between mutable and immutable access.

### Single-Item Pointers

The type \`*T\` is a pointer to a single value of type \`T\`. You get a pointer to a variable using the \`&\` operator, and you dereference it with \`.*\`:

\`\`\`zig
var x: i32 = 42;
const ptr: *i32 = &x;

// Read through the pointer
const value = ptr.*;  // 42

// Write through the pointer
ptr.* = 100;
// x is now 100
\`\`\`

Note the \`.*\` syntax for dereferencing. This is different from C's \`*ptr\`. Zig uses a postfix operator to keep the syntax consistent and unambiguous.

### Const Pointers

A \`*const T\` pointer allows reading but not writing. You get a const pointer when you take the address of a \`const\` variable:

\`\`\`zig
const y: i32 = 10;
const ptr: *const i32 = &y;
const value = ptr.*;  // 10
// ptr.* = 20;  // ERROR: cannot assign to constant
\`\`\`

A \`*T\` (mutable pointer) can be implicitly coerced to \`*const T\`, but not the other way around. This means functions that only need to read data should accept \`*const T\` for maximum flexibility.

### Passing Pointers to Functions

Pointers let functions modify the caller's data. Without a pointer, Zig passes values by copy:

\`\`\`zig
fn increment(ptr: *i32) void {
    ptr.* += 1;
}

var n: i32 = 5;
increment(&n);
// n is now 6
\`\`\`

### Pointers and Structs

When you have a pointer to a struct, you access fields the same way. Zig automatically dereferences through the pointer:

\`\`\`zig
const Point = struct { x: i32, y: i32 };

fn moveRight(p: *Point, amount: i32) void {
    p.x += amount;
}

var p = Point{ .x = 0, .y = 0 };
moveRight(&p, 5);
// p.x is now 5
\`\`\`

### Pointer Arithmetic and Slices

Zig does not allow arbitrary pointer arithmetic on single-item pointers. If you need to work with contiguous memory, use slices (\`[]T\`) or many-item pointers (\`[*]T\`). This prevents an entire class of bugs common in C programs.

### When to Use Pointers

Use pointers when you need to:
- **Modify the caller's data** --- the most common use case
- **Avoid copying large values** --- passing a pointer is cheaper than copying a large struct
- **Interface with C code** --- Zig's C interop uses pointer types

### Your Task

Write two functions:
- \`swap(a: *i32, b: *i32) void\` --- swaps the values that \`a\` and \`b\` point to.
- \`doubleAll(values: []i32) void\` --- doubles every element in the slice in place.`,

  starterCode: `const std = @import("std");

fn swap(a: *i32, b: *i32) void {
\t// Your code here
}

fn doubleAll(values: []i32) void {
\t// Your code here
}

pub fn main() !void {
\tvar x: i32 = 3;
\tvar y: i32 = 7;
\tswap(&x, &y);
\tstd.debug.print("{} {}\\n", .{ x, y });

\tvar arr = [_]i32{ 1, 2, 3 };
\tdoubleAll(&arr);
\tfor (arr) |v| {
\t\tstd.debug.print("{} ", .{v});
\t}
\tstd.debug.print("\\n", .{});
}
`,

  solution: `const std = @import("std");

fn swap(a: *i32, b: *i32) void {
\tconst tmp = a.*;
\ta.* = b.*;
\tb.* = tmp;
}

fn doubleAll(values: []i32) void {
\tfor (values) |*v| {
\t\tv.* *= 2;
\t}
}

pub fn main() !void {
\tvar x: i32 = 3;
\tvar y: i32 = 7;
\tswap(&x, &y);
\tstd.debug.print("{} {}\\n", .{ x, y });

\tvar arr = [_]i32{ 1, 2, 3 };
\tdoubleAll(&arr);
\tfor (arr) |v| {
\t\tstd.debug.print("{} ", .{v});
\t}
\tstd.debug.print("\\n", .{});
}
`,

  tests: [
    {
      name: "swap(3, 7) gives 7 3",
      code: `const std = @import("std");

{{FUNC}}

pub fn main() !void {
\tvar a: i32 = 3;
\tvar b: i32 = 7;
\tswap(&a, &b);
\tstd.debug.print("{} {}\\n", .{ a, b });
}
`,
      expected: "7 3\n",
    },
    {
      name: "swap(-1, 1) gives 1 -1",
      code: `const std = @import("std");

{{FUNC}}

pub fn main() !void {
\tvar a: i32 = -1;
\tvar b: i32 = 1;
\tswap(&a, &b);
\tstd.debug.print("{} {}\\n", .{ a, b });
}
`,
      expected: "1 -1\n",
    },
    {
      name: "doubleAll {1, 2, 3} gives 2 4 6",
      code: `const std = @import("std");

{{FUNC}}

pub fn main() !void {
\tvar arr = [_]i32{ 1, 2, 3 };
\tdoubleAll(&arr);
\tfor (arr) |v| {
\t\tstd.debug.print("{} ", .{v});
\t}
\tstd.debug.print("\\n", .{});
}
`,
      expected: "2 4 6 \n",
    },
    {
      name: "doubleAll {0, -5, 10} gives 0 -10 20",
      code: `const std = @import("std");

{{FUNC}}

pub fn main() !void {
\tvar arr = [_]i32{ 0, -5, 10 };
\tdoubleAll(&arr);
\tfor (arr) |v| {
\t\tstd.debug.print("{} ", .{v});
\t}
\tstd.debug.print("\\n", .{});
}
`,
      expected: "0 -10 20 \n",
    },
  ],
};
