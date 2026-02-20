import type { Lesson } from "../../types";

export const slab: Lesson = {
	id: "slab",
	title: "Slab Allocator",
	chapterId: "memory",
	content: `## The Slab Allocator

The Linux kernel allocates many short-lived, fixed-size objects — inodes, dentries, task_structs, file descriptors. Calling a general-purpose allocator for each one would be slow and cause fragmentation.

The **slab allocator** (introduced by Jeff Bonwick, used in Linux since 1996) solves this with pre-allocated pools of identically-sized objects. Each slab holds N objects and a **bitmap** marking which slots are free:

\`\`\`
slot:   0  1  2  3  4  5  6  7
used:   0  0  0  0  0  0  0  0   (0 = free, 1 = used)
\`\`\`

Allocating finds the first 0 bit and sets it to 1. Freeing clears the bit.

### Bitmap Operations

\`\`\`c
// Check if bit i is free
int bit_free(int bitmap, int i)   { return !(bitmap & (1 << i)); }

// Set bit i (mark used)
void bit_set(int *bitmap, int i)  { *bitmap |= (1 << i); }

// Clear bit i (mark free)
void bit_clr(int *bitmap, int i)  { *bitmap &= ~(1 << i); }
\`\`\`

### Your Implementation

Write \`int slab_alloc(int *bitmap, int n)\` that finds the first free slot (returns its index, or -1 if full), and \`void slab_free(int *bitmap, int idx)\` that frees a slot.

\`\`\`c
int slab_alloc(int *bitmap, int n) {
    for (int i = 0; i < n; i++)
        if (!(*bitmap & (1 << i))) { *bitmap |= (1 << i); return i; }
    return -1;
}

void slab_free(int *bitmap, int idx) {
    *bitmap &= ~(1 << idx);
}
\`\`\`

### Your Task

Implement \`slab_alloc\` and \`slab_free\`. Use an \`int\` as a 32-bit bitmap (supports up to 32 slots).`,

	starterCode: `#include <stdio.h>

int slab_alloc(int *bitmap, int n) {
\t// Find first free slot, mark it used, return its index (-1 if full)
\treturn -1;
}

void slab_free(int *bitmap, int idx) {
\t// Free the slot at idx
}

int main() {
\tint bm = 0;
\tint a = slab_alloc(&bm, 8);
\tint b = slab_alloc(&bm, 8);
\tint c = slab_alloc(&bm, 8);
\tprintf("%d %d %d\\n", a, b, c);
\tslab_free(&bm, b);
\tint d = slab_alloc(&bm, 8);
\tprintf("%d\\n", d);
\treturn 0;
}
`,

	solution: `#include <stdio.h>

int slab_alloc(int *bitmap, int n) {
\tfor (int i = 0; i < n; i++)
\t\tif (!(*bitmap & (1 << i))) { *bitmap |= (1 << i); return i; }
\treturn -1;
}

void slab_free(int *bitmap, int idx) {
\t*bitmap &= ~(1 << idx);
}

int main() {
\tint bm = 0;
\tint a = slab_alloc(&bm, 8);
\tint b = slab_alloc(&bm, 8);
\tint c = slab_alloc(&bm, 8);
\tprintf("%d %d %d\\n", a, b, c);
\tslab_free(&bm, b);
\tint d = slab_alloc(&bm, 8);
\tprintf("%d\\n", d);
\treturn 0;
}
`,

	tests: [
		{
			name: "allocates sequentially, freed slot is reused",
			expected: "0 1 2\n1\n",
		},
		{
			name: "returns -1 when slab is full",
			code: `#include <stdio.h>
{{FUNC}}
int main() {
\tint bm = 0;
\tfor (int i = 0; i < 4; i++) slab_alloc(&bm, 4);
\tprintf("%d\\n", slab_alloc(&bm, 4));
\treturn 0;
}`,
			expected: "-1\n",
		},
		{
			name: "free then alloc reuses the slot",
			code: `#include <stdio.h>
{{FUNC}}
int main() {
\tint bm = 0;
\tslab_alloc(&bm, 8);
\tslab_alloc(&bm, 8);
\tslab_free(&bm, 0);
\tprintf("%d\\n", slab_alloc(&bm, 8));
\treturn 0;
}`,
			expected: "0\n",
		},
		{
			name: "alloc from empty slab returns 0",
			code: `#include <stdio.h>
{{FUNC}}
int main() {
\tint bm = 0;
\tprintf("%d\\n", slab_alloc(&bm, 8));
\treturn 0;
}`,
			expected: "0\n",
		},
	],
};
