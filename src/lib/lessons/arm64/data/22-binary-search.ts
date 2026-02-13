import type { Lesson } from "../../types";

export const binarySearch: Lesson = {
  id: "binary-search",
  title: "Binary Search",
  chapterId: "putting-it-together",
  content: `## Binary Search in ARM64

Binary search is one of the most fundamental algorithms in computer science. It finds a target value in a **sorted** array by repeatedly halving the search space. This lesson combines nearly every concept from the course: memory access, loops, comparisons, arithmetic, and functions.

### The Algorithm

Given a sorted array and a target value:

1. Set \`lo = 0\` and \`hi = length - 1\`
2. While \`lo <= hi\`:
   a. Compute \`mid = (lo + hi) / 2\` (use unsigned division)
   b. If \`arr[mid] == target\`, return \`mid\`
   c. If \`arr[mid] < target\`, set \`lo = mid + 1\`
   d. If \`arr[mid] > target\`, set \`hi = mid - 1\`
3. If the loop ends, the target was not found

### Pseudocode

\`\`\`
function binary_search(arr, len, target):
    lo = 0
    hi = len - 1
    while lo <= hi:
        mid = (lo + hi) / 2
        val = arr[mid]
        if val == target:
            return mid
        elif val < target:
            lo = mid + 1
        else:
            hi = mid - 1
    return -1  // not found
\`\`\`

### Implementation Notes

**Computing the midpoint**: Use \`ADD\` and \`LSR\` (logical shift right by 1 is the same as dividing by 2):

\`\`\`asm
ADD X3, X0, X1       // X3 = lo + hi
LSR X3, X3, #1       // X3 = (lo + hi) / 2
\`\`\`

**Loading from the array**: Use \`LDRB\` with register offset:

\`\`\`asm
LDRB W4, [X5, X3]   // Load arr[mid] (byte array)
\`\`\`

**Updating lo and hi**: After comparing \`arr[mid]\` with the target:

\`\`\`asm
CMP W4, W6           // compare arr[mid] with target
B.EQ found           // exact match!
B.LT go_higher       // arr[mid] < target, search right half
// Otherwise arr[mid] > target, search left half
SUB X1, X3, #1       // hi = mid - 1
B search_loop

go_higher:
ADD X0, X3, #1       // lo = mid + 1
B search_loop
\`\`\`

### Time Complexity

> "Scanning for life forms" -- like Data methodically scanning a planet, binary search eliminates half the possibilities with each comparison. Fascinatingly efficient.

Binary search runs in **O(log n)** time. For an array of 1 million elements, it needs at most 20 comparisons. Linear search would need up to 1 million. This is the power of halving the search space.

### Your Task

Implement binary search on the sorted byte array \`[2, 5, 8, 12, 16, 23, 38, 42, 55, 67]\` (10 elements). Search for the target value \`23\`. It is at index 5. Print the index followed by a newline.`,

  starterCode: `.data
arr:
\t.byte 2, 5, 8, 12, 16, 23, 38, 42, 55, 67
buf:
\t.skip 2

.text
.global _start
_start:
\tLDR X5, =arr
\tMOV X6, #23
\tMOV X0, #0
\tMOV X1, #9

\t// Binary search loop:
\t// Compute mid = (lo + hi) / 2
\t// Compare arr[mid] with target
\t// If equal, found it
\t// If less, lo = mid + 1
\t// If greater, hi = mid - 1
\t// Repeat

\t// Print the index and exit
`,

  solution: `.data
arr:
\t.byte 2, 5, 8, 12, 16, 23, 38, 42, 55, 67
buf:
\t.skip 2

.text
.global _start
_start:
\tLDR X5, =arr
\tMOV X6, #23
\tMOV X0, #0
\tMOV X1, #9

search_loop:
\tCMP X0, X1
\tB.GT not_found

\tADD X3, X0, X1
\tLSR X3, X3, #1

\tLDRB W4, [X5, X3]
\tCMP W4, W6
\tB.EQ found
\tB.LT go_higher

\tSUB X1, X3, #1
\tB search_loop

go_higher:
\tADD X0, X3, #1
\tB search_loop

found:
\tADD X3, X3, #48

\tLDR X9, =buf
\tSTRB W3, [X9]
\tMOV W10, #10
\tSTRB W10, [X9, #1]

\tMOV X0, #1
\tLDR X1, =buf
\tMOV X2, #2
\tMOV X8, #64
\tSVC #0

\tMOV X0, #0
\tMOV X8, #93
\tSVC #0

not_found:
\tMOV X0, #0
\tMOV X8, #93
\tSVC #0
`,

  tests: [
    {
      name: "finds 23 at index 5",
      expected: "5\n",
    },
  ],
};
