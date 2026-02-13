import type { Lesson } from "../../types";

export const sorting: Lesson = {
  id: "sorting",
  title: "Sorting",
  chapterId: "putting-it-together",
  content: `## Bubble Sort in ARM64

Sorting is the ultimate test of assembly skills: it combines memory access, loops, comparisons, and conditional logic. We will implement bubble sort.

### The Algorithm

Bubble sort repeatedly walks through the array, comparing adjacent elements and swapping them if they are out of order. After each pass, the largest unsorted element "bubbles" to its correct position.

\`\`\`
for i = n-1 down to 1:
    for j = 0 to i-1:
        if arr[j] > arr[j+1]:
            swap arr[j] and arr[j+1]
\`\`\`

### Swapping with Registers

To swap two values in memory:
1. Load both into registers
2. Compare them
3. If out of order, store them back in swapped positions

\`\`\`asm
LDRB W2, [X0]        // a = arr[j]
LDRB W3, [X0, #1]    // b = arr[j+1]
CMP W2, W3
B.LE no_swap
STRB W3, [X0]        // arr[j] = b
STRB W2, [X0, #1]    // arr[j+1] = a
no_swap:
\`\`\`

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
