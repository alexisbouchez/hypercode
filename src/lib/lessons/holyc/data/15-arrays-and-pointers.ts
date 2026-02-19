import type { Lesson } from "../../types";

export const arraysAndPointers: Lesson = {
  id: "arrays-and-pointers",
  title: "Arrays and Pointers",
  chapterId: "advanced-holyc",
  content: `## Arrays and Pointers

HolyC arrays and pointers work exactly like C. An array is a contiguous block of memory, and the array name decays to a pointer to its first element.

### Declaring Arrays

\`\`\`holyc
I64 numbers[5];          // 5 I64 values, uninitialized
I64 primes[4] = {2, 3, 5, 7};  // initialized
\`\`\`

### Accessing Elements

Use zero-based indexing with \`[]\`:

\`\`\`holyc
I64 arr[3] = {10, 20, 30};
Print("%d\\n", arr[0]);  // 10
Print("%d\\n", arr[1]);  // 20
arr[2] = 99;
Print("%d\\n", arr[2]);  // 99
\`\`\`

### Pointers

A pointer holds the memory address of a value. The \`&\` operator gets an address; the \`*\` operator dereferences it:

\`\`\`holyc
I64 x = 42;
I64 *ptr = &x;   // ptr points to x
Print("%d\\n", *ptr);   // 42 — dereference to get the value
*ptr = 100;
Print("%d\\n", x);      // 100 — x was modified through ptr
\`\`\`

### Pointer Arithmetic

Moving a pointer by an integer moves it by that many elements:

\`\`\`holyc
I64 arr[3] = {10, 20, 30};
I64 *p = arr;       // p points to arr[0]
Print("%d\\n", *p);       // 10
p++;                // advance to arr[1]
Print("%d\\n", *p);       // 20
\`\`\`

### Strings as U8 Pointers

Strings are \`U8 *\` — a pointer to an array of bytes terminated by \`0\`:

\`\`\`holyc
U8 *msg = "HolyC";
Print("%s\\n", msg);  // HolyC
\`\`\`

### Your Task

Declare an array \`I64 vals[5]\` and fill it with the squares of indices 0–4 (i.e., 0, 1, 4, 9, 16). Then print each value on its own line.

Expected output:
\`\`\`
0
1
4
9
16
\`\`\``,

  starterCode: `I64 vals[5];
// Fill vals with squares of indices 0..4, then print each
`,

  solution: `I64 vals[5];
for (I64 i = 0; i < 5; i++) {
  vals[i] = i * i;
}
for (I64 i = 0; i < 5; i++) {
  Print("%d\\n", vals[i]);
}
`,

  tests: [
    {
      name: "prints squares 0 to 4",
      expected: "0\n1\n4\n9\n16\n",
    },
  ],
};
