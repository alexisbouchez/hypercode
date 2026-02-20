import type { Lesson } from "../../types";

export const vectors: Lesson = {
	id: "vectors",
	title: "Vectors",
	chapterId: "vectors",
	content: `## Vectors in Python

A **vector** is an ordered list of numbers. In Python, we represent vectors as plain lists.

\`\`\`python
v = [3, 1, 4, 1, 5]
print(v)        # [3, 1, 4, 1, 5]
print(len(v))   # 5  — number of elements
print(v[0])     # 3  — zero-indexed
\`\`\`

### Why Lists?

Python lists are flexible and sufficient for learning linear algebra concepts. For high-performance numerical work, libraries like NumPy provide array types with vectorized operations.

### Indexing

Python uses 0-based indexing. \`v[0]\` is the first element, \`v[-1]\` is the last.

### Length

\`len(v)\` returns the number of elements. For a vector in **ℝⁿ**, this is \`n\`.

### Your Task

Implement \`make_vector(values)\` that takes a Python list and returns it as a vector (list).`,

	starterCode: `def make_vector(values):
    # Return a list from the values
    pass

v = make_vector([3, 1, 4, 1, 5])
print(v)
print(len(v))
print(v[0])
`,

	solution: `def make_vector(values):
    return list(values)

v = make_vector([3, 1, 4, 1, 5])
print(v)
print(len(v))
print(v[0])
`,

	tests: [
		{
			name: "make_vector returns [3, 1, 4, 1, 5], length 5, index 0 = 3",
			expected: "[3, 1, 4, 1, 5]\n5\n3\n",
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
			name: "vector length of 3-element vector",
			code: `{{FUNC}}
print(len(make_vector([7, 8, 9])))`,
			expected: "3\n",
		},
	],
};
