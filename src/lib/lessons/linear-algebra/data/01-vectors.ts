import type { Lesson } from "../../types";

export const vectors: Lesson = {
	id: "vectors",
	title: "Vectors",
	chapterId: "vectors",
	content: `## Vectors in NumPy

A **vector** is an ordered list of numbers. In NumPy, vectors are 1-dimensional arrays.

\`\`\`python
import numpy as np

v = np.array([3, 1, 4, 1, 5])
print(v)        # [3 1 4 1 5]
print(v.shape)  # (5,)  — 5 elements
print(v[0])     # 3     — zero-indexed
print(len(v))   # 5
\`\`\`

### Why NumPy?

Python lists can hold vectors, but NumPy arrays are:

- **Faster** — stored as contiguous memory, processed with SIMD instructions
- **Concise** — \`a + b\` adds element-wise (no loop needed)
- **Integrated** — all linear algebra functions expect NumPy arrays

### Shape

\`v.shape\` returns a tuple. For a 1D array with \`n\` elements, it is \`(n,)\`. The trailing comma distinguishes it from a scalar.

### Indexing

NumPy uses 0-based indexing. \`v[0]\` is the first element, \`v[-1]\` is the last.

### Your Task

Implement \`make_vector(values)\` that takes a Python list and returns a NumPy array.`,

	starterCode: `import numpy as np

def make_vector(values):
    # Return a NumPy array from the list of values
    pass

v = make_vector([3, 1, 4, 1, 5])
print(v)
print(v.shape)
print(v[0])
`,

	solution: `import numpy as np

def make_vector(values):
    return np.array(values)

v = make_vector([3, 1, 4, 1, 5])
print(v)
print(v.shape)
print(v[0])
`,

	tests: [
		{
			name: "make_vector returns [3 1 4 1 5], shape (5,), index 0 = 3",
			expected: "[3 1 4 1 5]\n(5,)\n3\n",
		},
		{
			name: "indexing into vector",
			code: `{{FUNC}}
v = make_vector([10, 20, 30])
print(v[1])
print(v[2])`,
			expected: "20\n30\n",
		},
		{
			name: "vector length",
			code: `{{FUNC}}
print(len(make_vector([1, 2, 3, 4])))`,
			expected: "4\n",
		},
		{
			name: "vector shape",
			code: `{{FUNC}}
print(make_vector([7, 8, 9]).shape)`,
			expected: "(3,)\n",
		},
	],
};
