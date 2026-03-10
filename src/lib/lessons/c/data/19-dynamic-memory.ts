import type { Lesson } from "../../types";

export const dynamicMemory: Lesson = {
	id: "dynamic-memory",
	title: "Dynamic Memory Allocation",
	chapterId: "dynamic-memory",
	content: `## Dynamic Memory Allocation

So far, all variables have been allocated on the **stack** with a fixed size known at compile time. **Dynamic memory allocation** lets you request memory at runtime from the **heap**.

### Heap vs Stack

Stack and heap are two regions of memory with very different characteristics:

| | Stack | Heap |
|---|---|---|
| **Allocation** | Automatic (compiler manages) | Manual (\`malloc\`/\`free\`) |
| **Speed** | Very fast (just moves stack pointer) | Slower (must find free block) |
| **Lifetime** | Until function returns | Until you call \`free\` |
| **Size** | Small (typically 1-8 MB) | Large (limited by system RAM) |
| **Access** | Local variables only | Accessible via pointers from anywhere |
| **Fragmentation** | None | Can fragment over time |

Use the **stack** for small, short-lived variables. Use the **heap** when you need memory to outlive a function call, when the size is determined at runtime, or when you need large allocations.

\`\`\`c
void example() {
    int x = 10;                          // stack: fast, automatic
    int *p = (int *)malloc(sizeof(int)); // heap: manual, flexible
    *p = 20;
    free(p);                             // you must free heap memory
}   // x is automatically freed when function returns
\`\`\`

### malloc

\`malloc(size)\` allocates \`size\` bytes and returns a pointer to the memory:

\`\`\`c
#include <stdlib.h>

int *p = (int *)malloc(sizeof(int));
*p = 42;
printf("%d\\n", *p);  // 42
free(p);              // always free when done
\`\`\`

> The replicator: \`malloc\` materializes memory from thin air, and \`free\` recycles it back into energy.

### sizeof

Use \`sizeof\` to get the size of a type in bytes:

\`\`\`c
sizeof(int)     // 4 bytes
sizeof(char)    // 1 byte
sizeof(long)    // 8 bytes
\`\`\`

### Dynamic Arrays

\`malloc\` is often used to create arrays whose size is determined at runtime:

\`\`\`c
int n = 5;
int *arr = (int *)malloc(n * sizeof(int));
for (int i = 0; i < n; i++) {
    arr[i] = i * 10;
}
// arr[0]=0, arr[1]=10, arr[2]=20, ...
free(arr);
\`\`\`

### free

Every \`malloc\` must be paired with a \`free\` to avoid **memory leaks**:

\`\`\`c
int *p = (int *)malloc(sizeof(int));
*p = 99;
free(p);    // release the memory
// p is now a dangling pointer -- don't use it!
\`\`\`

### Memory Debugging with Valgrind

Memory bugs are among the hardest to track down. Common memory errors include:

- **Memory leak** -- forgetting to \`free\` allocated memory
- **Use after free** -- reading/writing memory after calling \`free\`
- **Double free** -- calling \`free\` on the same pointer twice
- **Buffer overflow** -- writing past the end of an allocated block

In a real development environment, **Valgrind** is the standard tool for catching these bugs:

\`\`\`bash
gcc -g my_program.c -o my_program
valgrind --leak-check=full ./my_program
\`\`\`

Valgrind will report exactly where leaks and invalid accesses occur. Always run it before shipping C code. A clean Valgrind report (0 errors, 0 leaks) is the gold standard for C memory safety.

### Your Task

Write a function \`int *make_range(int n)\` that allocates an array of \`n\` integers and fills it with values 1 through n. In main, call it with n=5, print each value on its own line, then free the memory.`,

	starterCode: `#include <stdio.h>
#include <stdlib.h>

int *make_range(int n) {
\t// Allocate array of n ints, fill with 1..n
\treturn NULL;
}

int main() {
\tint n = 5;
\tint *arr = make_range(n);
\tfor (int i = 0; i < n; i++) {
\t\tprintf("%d\\n", arr[i]);
\t}
\tfree(arr);
\treturn 0;
}
`,

	solution: `#include <stdio.h>
#include <stdlib.h>

int *make_range(int n) {
\tint *arr = (int *)malloc(n * sizeof(int));
\tfor (int i = 0; i < n; i++) {
\t\tarr[i] = i + 1;
\t}
\treturn arr;
}

int main() {
\tint n = 5;
\tint *arr = make_range(n);
\tfor (int i = 0; i < n; i++) {
\t\tprintf("%d\\n", arr[i]);
\t}
\tfree(arr);
\treturn 0;
}
`,

	tests: [
		{
			name: "prints range 1 to 5",
			expected: "1\n2\n3\n4\n5\n",
		},
		{
			name: "make_range(3)",
			code: `#include <stdio.h>
#include <stdlib.h>
{{FUNC}}
int main() {
\tint *arr = make_range(3);
\tfor (int i = 0; i < 3; i++) {
\t\tprintf("%d\\n", arr[i]);
\t}
\tfree(arr);
\treturn 0;
}`,
			expected: "1\n2\n3\n",
		},
		{
			name: "make_range(1)",
			code: `#include <stdio.h>
#include <stdlib.h>
{{FUNC}}
int main() {
\tint *arr = make_range(1);
\tprintf("%d\\n", arr[0]);
\tfree(arr);
\treturn 0;
}`,
			expected: "1\n",
		},
		{
			name: "make_range(8)",
			code: `#include <stdio.h>
#include <stdlib.h>
{{FUNC}}
int main() {
\tint *arr = make_range(8);
\tfor (int i = 0; i < 8; i++) {
\t\tprintf("%d\\n", arr[i]);
\t}
\tfree(arr);
\treturn 0;
}`,
			expected: "1\n2\n3\n4\n5\n6\n7\n8\n",
		},
		{
			name: "make_range(10) first and last",
			code: `#include <stdio.h>
#include <stdlib.h>
{{FUNC}}
int main() {
\tint *arr = make_range(10);
\tprintf("%d\\n", arr[0]);
\tprintf("%d\\n", arr[9]);
\tfree(arr);
\treturn 0;
}`,
			expected: "1\n10\n",
		},
	],
};
