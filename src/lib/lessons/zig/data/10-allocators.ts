import type { Lesson } from "../../types";

export const allocators: Lesson = {
  id: "allocators",
  title: "Allocators",
  chapterId: "memory",
  content: `## Explicit Memory Management with Allocators

One of Zig's most distinctive features is its allocator interface. Unlike C, which hides memory allocation behind \`malloc\` and \`free\`, and unlike garbage-collected languages that handle it automatically, Zig makes every allocation explicit and customizable. Every data structure that needs heap memory takes an allocator as a parameter.

### Why Allocators?

The allocator pattern gives you:
- **Transparency** --- you always know when memory is being allocated
- **Testability** --- you can swap in a testing allocator that tracks leaks
- **Flexibility** --- different parts of your program can use different allocation strategies
- **No hidden behavior** --- there is no global allocator lurking behind the scenes

> "Tea, Earl Grey, hot." Even Picard's replicator must allocate energy before materializing that cup. In Zig, every allocation is just as deliberate --- nothing appears out of thin air.

### The Page Allocator

The simplest allocator is \`std.heap.page_allocator\`. It requests memory directly from the operating system:

\`\`\`zig
const std = @import("std");
const allocator = std.heap.page_allocator;

pub fn main() !void {
    const buf = try allocator.alloc(u8, 1024);
    defer allocator.free(buf);

    // Use buf...
}
\`\`\`

The \`try\` keyword handles the possibility that allocation fails (out of memory). The \`defer\` keyword ensures the memory is freed when the current scope exits, no matter how it exits. This is Zig's answer to RAII and try-finally patterns.

### ArrayList

\`std.ArrayList\` is Zig's dynamic array, similar to \`std::vector\` in C++ or \`Vec\` in Rust. In Zig, the allocator is passed explicitly to every operation that might allocate:

\`\`\`zig
const std = @import("std");

pub fn main() !void {
    const allocator = std.heap.page_allocator;
    var list: std.ArrayList(i32) = .empty;
    defer list.deinit(allocator);

    try list.append(allocator, 10);
    try list.append(allocator, 20);
    try list.append(allocator, 30);

    for (list.items) |item| {
        std.debug.print("{}\\n", .{item});
    }
}
\`\`\`

Key ArrayList operations:
- \`.empty\` --- create an empty list (no allocator stored)
- \`deinit(allocator)\` --- free all memory (always pair with \`defer\`)
- \`append(allocator, value)\` --- add an element (may allocate, hence \`try\`)
- \`items\` --- access the underlying slice
- \`items.len\` --- get the current number of elements

Notice that the allocator is passed to \`append\` and \`deinit\` rather than being stored in the list. This makes allocation points explicit in your code and keeps the struct smaller.

### The defer Pattern

\`defer\` is essential for resource management in Zig. It guarantees cleanup code runs when the scope exits:

\`\`\`zig
var list: std.ArrayList(u8) = .empty;
defer list.deinit(allocator);  // Runs when function returns

// Even if an error occurs below, deinit() still runs
try list.append(allocator, 42);
try list.append(allocator, 99);
\`\`\`

Multiple \`defer\` statements execute in reverse order (last defer runs first), which naturally handles nested resource acquisition.

### Allocating Single Values

You can allocate a single value on the heap with \`create\` and free it with \`destroy\`:

\`\`\`zig
const ptr = try allocator.create(i32);
defer allocator.destroy(ptr);

ptr.* = 42;
\`\`\`

### Your Task

Write a function \`buildRange(allocator: std.mem.Allocator, start: i32, end: i32) !std.ArrayList(i32)\` that creates an ArrayList containing all integers from \`start\` (inclusive) to \`end\` (exclusive).

Also write a function \`sumList(list: std.ArrayList(i32)) i32\` that returns the sum of all elements in the ArrayList.`,

  starterCode: `const std = @import("std");

fn buildRange(allocator: std.mem.Allocator, start: i32, end: i32) !std.ArrayList(i32) {
\tvar list: std.ArrayList(i32) = .empty;
\t// Your code here: append integers from start to end (exclusive)
\treturn list;
}

fn sumList(list: std.ArrayList(i32)) i32 {
\t// Your code here: sum all elements in list.items
\treturn 0;
}

pub fn main() !void {
\tconst allocator = std.heap.page_allocator;
\tvar list = try buildRange(allocator, 1, 6);
\tdefer list.deinit(allocator);

\tstd.debug.print("{}\\n", .{sumList(list)});
}
`,

  solution: `const std = @import("std");

fn buildRange(allocator: std.mem.Allocator, start: i32, end: i32) !std.ArrayList(i32) {
\tvar list: std.ArrayList(i32) = .empty;
\tvar i = start;
\twhile (i < end) : (i += 1) {
\t\ttry list.append(allocator, i);
\t}
\treturn list;
}

fn sumList(list: std.ArrayList(i32)) i32 {
\tvar total: i32 = 0;
\tfor (list.items) |item| {
\t\ttotal += item;
\t}
\treturn total;
}

pub fn main() !void {
\tconst allocator = std.heap.page_allocator;
\tvar list = try buildRange(allocator, 1, 6);
\tdefer list.deinit(allocator);

\tstd.debug.print("{}\\n", .{sumList(list)});
}
`,

  tests: [
    {
      name: "buildRange(1, 6) summed is 15",
      code: `const std = @import("std");

{{FUNC}}

pub fn main() !void {
\tconst allocator = std.heap.page_allocator;
\tvar list = try buildRange(allocator, 1, 6);
\tdefer list.deinit(allocator);
\tstd.debug.print("{}\\n", .{sumList(list)});
}
`,
      expected: "15\n",
    },
    {
      name: "buildRange(0, 1) summed is 0",
      code: `const std = @import("std");

{{FUNC}}

pub fn main() !void {
\tconst allocator = std.heap.page_allocator;
\tvar list = try buildRange(allocator, 0, 1);
\tdefer list.deinit(allocator);
\tstd.debug.print("{}\\n", .{sumList(list)});
}
`,
      expected: "0\n",
    },
    {
      name: "buildRange(1, 1) is empty, sum is 0",
      code: `const std = @import("std");

{{FUNC}}

pub fn main() !void {
\tconst allocator = std.heap.page_allocator;
\tvar list = try buildRange(allocator, 1, 1);
\tdefer list.deinit(allocator);
\tstd.debug.print("{}\\n", .{sumList(list)});
}
`,
      expected: "0\n",
    },
    {
      name: "buildRange(1, 11) has 10 elements",
      code: `const std = @import("std");

{{FUNC}}

pub fn main() !void {
\tconst allocator = std.heap.page_allocator;
\tvar list = try buildRange(allocator, 1, 11);
\tdefer list.deinit(allocator);
\tstd.debug.print("{}\\n", .{list.items.len});
}
`,
      expected: "10\n",
    },
  ],
};
