import type { Lesson } from "../../types";

export const recursiveDataStructures: Lesson = {
  id: "recursive-data-structures",
  title: "Recursive Data Structures",
  chapterId: "data-structures",
  content: `## Building Linked Lists and Trees in Zig

Recursive data structures --- where a type refers to itself --- are fundamental in computer science. Linked lists, trees, and graphs all depend on them. In Zig, building these structures requires combining several concepts you have already learned: structs, optional pointers, and allocators.

### The Problem with Direct Recursion

In many languages, you can write something like \`struct Node { value: i32, next: Node }\`. In Zig, this is illegal because the compiler needs to know the size of every type, and a struct that contains itself would be infinitely large:

\`\`\`zig
// COMPILE ERROR: struct has infinite size
const Node = struct {
    value: i32,
    next: Node,
};
\`\`\`

### The Solution: Optional Pointers

The fix is to use a pointer instead. A pointer has a fixed size (8 bytes on a 64-bit system) regardless of what it points to. Combined with optionals, you get a nullable link:

\`\`\`zig
const Node = struct {
    value: i32,
    next: ?*Node,
};
\`\`\`

The type \`?*Node\` means "either a pointer to a Node, or null." This is how you represent the end of a linked list.

> In the Delta Quadrant, Voyager's supply chain was a linked list of trading posts --- each one pointing to the next, until the chain ended with null (or a hostile species).

### Creating Nodes on the Heap

Since recursive structures have unknown depth, you typically allocate nodes on the heap using an allocator:

\`\`\`zig
const std = @import("std");

const Node = struct {
    value: i32,
    next: ?*Node,
};

pub fn main() !void {
    const allocator = std.heap.page_allocator;

    const node3 = try allocator.create(Node);
    node3.* = Node{ .value = 30, .next = null };

    const node2 = try allocator.create(Node);
    node2.* = Node{ .value = 20, .next = node3 };

    const node1 = try allocator.create(Node);
    node1.* = Node{ .value = 10, .next = node2 };

    // Traverse: 10 -> 20 -> 30
    var current: ?*Node = node1;
    while (current) |node| {
        std.debug.print("{}\\n", .{node.value});
        current = node.next;
    }
}
\`\`\`

### Traversal Pattern

The idiomatic traversal uses \`while\` with optional unwrapping:

\`\`\`zig
var current: ?*Node = head;
while (current) |node| {
    // process node.value
    current = node.next;
}
\`\`\`

This is safe: the loop body only runs when \`current\` is non-null, and \`node\` is the unwrapped pointer. The loop ends when \`current\` becomes \`null\`.

### Binary Trees

The same pattern extends to binary trees by using two optional pointers:

\`\`\`zig
const TreeNode = struct {
    value: i32,
    left: ?*TreeNode,
    right: ?*TreeNode,
};
\`\`\`

You can then write recursive functions that operate on trees:

\`\`\`zig
fn treeSum(node: ?*const TreeNode) i32 {
    const n = node orelse return 0;
    return n.value + treeSum(n.left) + treeSum(n.right);
}
\`\`\`

### Freeing Recursive Structures

When you allocate nodes on the heap, you must free them. For a linked list, traverse and destroy each node:

\`\`\`zig
fn freeList(allocator: std.mem.Allocator, head: ?*Node) void {
    var current = head;
    while (current) |node| {
        const next = node.next;
        allocator.destroy(node);
        current = next;
    }
}
\`\`\`

Note that you must save \`node.next\` before destroying the node, since the memory becomes invalid after \`destroy\`.

### Your Task

Define a \`Node\` struct with fields \`value: i32\` and \`next: ?*Node\`.

Write two functions:

- \`listLength(head: ?*const Node) u32\` --- returns the number of nodes in the linked list. If \`head\` is null, return 0.
- \`listSum(head: ?*const Node) i32\` --- returns the sum of all values in the linked list. If \`head\` is null, return 0.`,

  starterCode: `const std = @import("std");

const Node = struct {
\tvalue: i32,
\tnext: ?*Node,
};

fn listLength(head: ?*const Node) u32 {
\t// Your code here: count the nodes
\t_ = head;
\treturn 0;
}

fn listSum(head: ?*const Node) i32 {
\t// Your code here: sum the values
\t_ = head;
\treturn 0;
}

pub fn main() !void {
\tconst allocator = std.heap.page_allocator;

\tconst n3 = try allocator.create(Node);
\tn3.* = Node{ .value = 30, .next = null };

\tconst n2 = try allocator.create(Node);
\tn2.* = Node{ .value = 20, .next = n3 };

\tconst n1 = try allocator.create(Node);
\tn1.* = Node{ .value = 10, .next = n2 };

\tstd.debug.print("{}\\n", .{listLength(n1)});
\tstd.debug.print("{}\\n", .{listSum(n1)});
}
`,

  solution: `const std = @import("std");

const Node = struct {
\tvalue: i32,
\tnext: ?*Node,
};

fn listLength(head: ?*const Node) u32 {
\tvar count: u32 = 0;
\tvar current = head;
\twhile (current) |node| {
\t\tcount += 1;
\t\tcurrent = node.next;
\t}
\treturn count;
}

fn listSum(head: ?*const Node) i32 {
\tvar total: i32 = 0;
\tvar current = head;
\twhile (current) |node| {
\t\ttotal += node.value;
\t\tcurrent = node.next;
\t}
\treturn total;
}

pub fn main() !void {
\tconst allocator = std.heap.page_allocator;

\tconst n3 = try allocator.create(Node);
\tn3.* = Node{ .value = 30, .next = null };

\tconst n2 = try allocator.create(Node);
\tn2.* = Node{ .value = 20, .next = n3 };

\tconst n1 = try allocator.create(Node);
\tn1.* = Node{ .value = 10, .next = n2 };

\tstd.debug.print("{}\\n", .{listLength(n1)});
\tstd.debug.print("{}\\n", .{listSum(n1)});
}
`,

  tests: [
    {
      name: "length of 3-node list is 3",
      code: `const std = @import("std");

{{FUNC}}

pub fn main() !void {
\tconst allocator = std.heap.page_allocator;
\tconst n3 = try allocator.create(Node);
\tn3.* = Node{ .value = 30, .next = null };
\tconst n2 = try allocator.create(Node);
\tn2.* = Node{ .value = 20, .next = n3 };
\tconst n1 = try allocator.create(Node);
\tn1.* = Node{ .value = 10, .next = n2 };
\tstd.debug.print("{}\\n", .{listLength(n1)});
}`,
      expected: "3\n",
    },
    {
      name: "length of single node is 1",
      code: `const std = @import("std");

{{FUNC}}

pub fn main() !void {
\tconst allocator = std.heap.page_allocator;
\tconst n1 = try allocator.create(Node);
\tn1.* = Node{ .value = 42, .next = null };
\tstd.debug.print("{}\\n", .{listLength(n1)});
}`,
      expected: "1\n",
    },
    {
      name: "length of null list is 0",
      code: `const std = @import("std");

{{FUNC}}

pub fn main() !void {
\tstd.debug.print("{}\\n", .{listLength(null)});
}`,
      expected: "0\n",
    },
    {
      name: "sum of 10+20+30 is 60",
      code: `const std = @import("std");

{{FUNC}}

pub fn main() !void {
\tconst allocator = std.heap.page_allocator;
\tconst n3 = try allocator.create(Node);
\tn3.* = Node{ .value = 30, .next = null };
\tconst n2 = try allocator.create(Node);
\tn2.* = Node{ .value = 20, .next = n3 };
\tconst n1 = try allocator.create(Node);
\tn1.* = Node{ .value = 10, .next = n2 };
\tstd.debug.print("{}\\n", .{listSum(n1)});
}`,
      expected: "60\n",
    },
    {
      name: "sum of single node is its value",
      code: `const std = @import("std");

{{FUNC}}

pub fn main() !void {
\tconst allocator = std.heap.page_allocator;
\tconst n1 = try allocator.create(Node);
\tn1.* = Node{ .value = 99, .next = null };
\tstd.debug.print("{}\\n", .{listSum(n1)});
}`,
      expected: "99\n",
    },
    {
      name: "sum of null list is 0",
      code: `const std = @import("std");

{{FUNC}}

pub fn main() !void {
\tstd.debug.print("{}\\n", .{listSum(null)});
}`,
      expected: "0\n",
    },
  ],
};
