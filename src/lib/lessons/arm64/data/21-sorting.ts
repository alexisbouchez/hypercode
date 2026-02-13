import type { Lesson } from "../../types";

export const sorting: Lesson = {
  id: "sorting",
  title: "Sorting",
  chapterId: "putting-it-together",
  content: `## Bubble Sort in ARM64

Sorting is the ultimate test of assembly skills: it combines memory access, nested loops, comparisons, and conditional logic. We will implement bubble sort -- a simple O(n^2) algorithm that is easy to implement correctly.

### The Algorithm

Bubble sort repeatedly walks through the array, comparing adjacent elements and swapping them if they are out of order. After each pass, the largest unsorted element "bubbles up" to its correct position:

\`\`\`
for i = n-1 down to 1:      // outer loop: shrink unsorted region
    for j = 0 to i-1:       // inner loop: walk through unsorted part
        if arr[j] > arr[j+1]:
            swap arr[j] and arr[j+1]
\`\`\`

### Why Bubble Sort?

While not efficient for large datasets (O(n^2) comparisons), bubble sort is ideal for learning assembly because:
- It only needs adjacent element comparisons and swaps
- The memory access pattern is simple (sequential)
- The nested loop structure maps cleanly to assembly
- It sorts in-place (no extra memory needed)

### Swapping in Assembly

To swap two adjacent values in memory, load both, compare, and store them back in reversed positions:

\`\`\`asm
LDRB W2, [X0]        // a = arr[j]
LDRB W3, [X0, #1]    // b = arr[j+1]
CMP W2, W3
B.LE no_swap          // Already in order, skip
STRB W3, [X0]        // arr[j] = b
STRB W2, [X0, #1]    // arr[j+1] = a
no_swap:
ADD X0, X0, #1       // Advance to next pair
\`\`\`

### Nested Loop Structure

The outer loop controls how many passes we make. The inner loop does the actual comparing and swapping. The outer counter starts at n-1 and counts down:

\`\`\`asm
MOV X9, #4           // i = n-1 = 4 (for 5 elements)
outer:
    CBZ X9, done      // If i == 0, sorting is complete
    // ... inner loop (j = 0 to i-1) ...
    SUB X9, X9, #1   // i--
    B outer
done:
\`\`\`

> **Tip**: In the inner loop, reset the array pointer to the beginning on each outer iteration. Use the outer counter to limit how far the inner loop runs.

### Your Task

Sort the array \`[5, 3, 8, 1, 4]\` using bubble sort, then print each element as a digit followed by a newline: \`13458\\n\`.

The array is 5 bytes. After sorting it should be \`[1, 3, 4, 5, 8]\`.`,

  starterCode: `.data
arr:
\t.byte 5, 3, 8, 1, 4
buf:
\t.skip 6

.text
.global _start
_start:
\t// Implement bubble sort on arr (5 elements)
\t// After sorting, convert each byte to ASCII digit
\t// Store in buf with a newline at the end
\t// Print and exit
`,

  solution: `.data
arr:
\t.byte 5, 3, 8, 1, 4
buf:
\t.skip 6

.text
.global _start
_start:
\tMOV X9, #4

outer:
\tCBZ X9, sort_done
\tLDR X0, =arr
\tMOV X1, X9
\tMOV X10, #0

inner:
\tCBZ X1, inner_done
\tLDRB W2, [X0]
\tLDRB W3, [X0, #1]
\tCMP W2, W3
\tB.LE no_swap
\tSTRB W3, [X0]
\tSTRB W2, [X0, #1]

no_swap:
\tADD X0, X0, #1
\tSUB X1, X1, #1
\tB inner

inner_done:
\tSUB X9, X9, #1
\tB outer

sort_done:
\tLDR X0, =arr
\tLDR X4, =buf
\tMOV X5, #0

print_loop:
\tCMP X5, #5
\tB.GE print_done
\tLDRB W1, [X0, X5]
\tADD W1, W1, #48
\tSTRB W1, [X4, X5]
\tADD X5, X5, #1
\tB print_loop

print_done:
\tMOV W6, #10
\tSTRB W6, [X4, #5]

\tMOV X0, #1
\tLDR X1, =buf
\tMOV X2, #6
\tMOV X8, #64
\tSVC #0

\tMOV X0, #0
\tMOV X8, #93
\tSVC #0
`,

  tests: [
    {
      name: "prints sorted array",
      expected: "13458\n",
    },
  ],
};
