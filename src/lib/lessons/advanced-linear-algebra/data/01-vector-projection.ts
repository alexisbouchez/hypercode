import type { Lesson } from "../../types";

export const vectorProjectionLesson: Lesson = {
	id: "vector-projection",
	title: "Vector Projection",
	chapterId: "orthogonality",
	content: `## Vector Projection

The **projection of v onto u** is the component of v that lies in the direction of u:

\`\`\`
proj_u(v) = (v·u / u·u) × u
\`\`\`

The scalar coefficient v·u / u·u scales u to exactly the "shadow" of v along u.

### Geometric Intuition

Imagine shining a light perpendicular to u — the shadow of v cast onto the line through u is exactly the projection. The perpendicular component v − proj_u(v) is orthogonal to u.

### Examples

| v | u | proj_u(v) |
|---|---|-----------|
| [3, 4, 0] | [1, 0, 0] | [3.0000, 0.0000, 0.0000] |
| [1, 2, 3] | [1, 1, 1] | [2.0000, 2.0000, 2.0000] |
| [0, 5] | [3, 4] | [2.4000, 3.2000] |

**Note:** For [1,2,3] onto [1,1,1]: the sum is 6, so the coefficient is 6/3 = 2, giving [2, 2, 2].

### Your Task

Implement \`dot(a, b)\` returning the dot product of two vectors, and \`project(v, u)\` returning the projection of v onto u.`,

	starterCode: `def dot(a, b):
    # Sum of elementwise products
    return 0

def project(v, u):
    # proj_u(v) = (v·u / u·u) × u
    return [0.0] * len(u)

v1, u1 = [3, 4, 0], [1, 0, 0]
p1 = project(v1, u1)
print(" ".join(f"{x:.4f}" for x in p1))

v2, u2 = [1, 2, 3], [1, 1, 1]
p2 = project(v2, u2)
print(" ".join(f"{x:.4f}" for x in p2))
`,

	solution: `def dot(a, b):
    return sum(x * y for x, y in zip(a, b))

def project(v, u):
    coeff = dot(v, u) / dot(u, u)
    return [coeff * x for x in u]

v1, u1 = [3, 4, 0], [1, 0, 0]
p1 = project(v1, u1)
print(" ".join(f"{x:.4f}" for x in p1))

v2, u2 = [1, 2, 3], [1, 1, 1]
p2 = project(v2, u2)
print(" ".join(f"{x:.4f}" for x in p2))
`,

	tests: [
		{
			name: "project [3,4,0] onto [1,0,0] → [3,0,0]",
			code: `{{FUNC}}
p = project([3, 4, 0], [1, 0, 0])
print(" ".join(f"{x:.4f}" for x in p))`,
			expected: "3.0000 0.0000 0.0000\n",
		},
		{
			name: "project [1,2,3] onto [1,1,1] → [2,2,2]",
			code: `{{FUNC}}
p = project([1, 2, 3], [1, 1, 1])
print(" ".join(f"{x:.4f}" for x in p))`,
			expected: "2.0000 2.0000 2.0000\n",
		},
		{
			name: "project [0,5] onto [3,4] → [2.4, 3.2]",
			code: `{{FUNC}}
p = project([0, 5], [3, 4])
print(" ".join(f"{x:.4f}" for x in p))`,
			expected: "2.4000 3.2000\n",
		},
		{
			name: "dot product [1,2,3]·[4,5,6] = 32",
			code: `{{FUNC}}
print(dot([1, 2, 3], [4, 5, 6]))`,
			expected: "32\n",
		},
	],
};
