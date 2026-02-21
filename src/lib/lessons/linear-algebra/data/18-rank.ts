import type { Lesson } from "../../types";

export const matrixRank: Lesson = {
	id: "matrix-rank",
	title: "Matrix Rank",
	chapterId: "decompositions",
	content: `## Matrix Rank

The **rank** of a matrix $A$ is the number of linearly independent rows (or equivalently, columns). It tells you:

- How many equations in $Ax = b$ are truly independent
- The dimension of the column space (image) and row space
- Whether $A$ is invertible (full rank iff $\\text{rank}(A) = n$ for square $A$)

### Key Facts

| Condition | Rank |
|-----------|------|
| All rows independent | $\\text{rank} = m$ (full row rank) |
| All columns independent | $\\text{rank} = n$ (full column rank) |
| Zero matrix | $\\text{rank} = 0$ |
| Redundant rows/columns | $\\text{rank} < \\min(m,n)$ |

### Computing Rank via Row Reduction

Row operations don't change the rank. Reduce $A$ to row-echelon form and **count non-zero pivot rows**:

$$A = \\begin{pmatrix}1 & 2 & 3 \\\\ 4 & 5 & 6 \\\\ 7 & 8 & 9\\end{pmatrix} \\xrightarrow{\\text{row reduce}} \\begin{pmatrix}1 & * & * \\\\ 0 & 1 & * \\\\ 0 & 0 & 0\\end{pmatrix} \\implies \\text{rank} = 2$$

Row 3 became all zeros — it was a linear combination of rows 1 and 2.

### Algorithm

\`\`\`python
def matrix_rank(A):
    m = [row[:] for row in A]
    rows, cols = len(m), len(m[0])
    rank = 0
    for col in range(cols):
        # find pivot in this column at or below current rank
        pivot = next((r for r in range(rank, rows)
                      if abs(m[r][col]) > 1e-10), -1)
        if pivot == -1:
            continue
        m[rank], m[pivot] = m[pivot], m[rank]
        s = m[rank][col]
        m[rank] = [x/s for x in m[rank]]
        for r in range(rows):
            if r != rank and abs(m[r][col]) > 1e-10:
                f = m[r][col]
                m[r] = [m[r][j] - f*m[rank][j] for j in range(cols)]
        rank += 1
    return rank
\`\`\`

### Your Task

Implement \`matrix_rank(A)\` by Gaussian elimination, counting the pivot rows.`,

	starterCode: `def matrix_rank(A):
    m = [row[:] for row in A]
    rows, cols = len(m), len(m[0])
    rank = 0
    # row-reduce and count non-zero pivots
    return rank

print(matrix_rank([[1,2,3],[4,5,6],[7,8,9]]))  # 2
print(matrix_rank([[1,0],[0,1]]))               # 2
print(matrix_rank([[1,2],[2,4]]))               # 1
print(matrix_rank([[0,0],[0,0]]))               # 0
`,

	solution: `def matrix_rank(A):
    m = [row[:] for row in A]
    rows, cols = len(m), len(m[0])
    rank = 0
    for col in range(cols):
        pivot = -1
        for r in range(rank, rows):
            if abs(m[r][col]) > 1e-10:
                pivot = r
                break
        if pivot == -1:
            continue
        m[rank], m[pivot] = m[pivot], m[rank]
        s = m[rank][col]
        m[rank] = [x / s for x in m[rank]]
        for r in range(rows):
            if r != rank and abs(m[r][col]) > 1e-10:
                f = m[r][col]
                m[r] = [m[r][j] - f * m[rank][j] for j in range(cols)]
        rank += 1
    return rank

print(matrix_rank([[1,2,3],[4,5,6],[7,8,9]]))
print(matrix_rank([[1,0],[0,1]]))
print(matrix_rank([[1,2],[2,4]]))
print(matrix_rank([[0,0],[0,0]]))
`,

	tests: [
		{
			name: "ranks: 2, 2, 1, 0",
			expected: "2\n2\n1\n0\n",
		},
		{
			name: "rank([[1,2,3],[4,5,6],[7,8,9]]) = 2",
			code: `{{FUNC}}
print(matrix_rank([[1,2,3],[4,5,6],[7,8,9]]))`,
			expected: "2\n",
		},
		{
			name: "rank([[1,0,0],[0,1,0],[0,0,1]]) = 3",
			code: `{{FUNC}}
print(matrix_rank([[1,0,0],[0,1,0],[0,0,1]]))`,
			expected: "3\n",
		},
		{
			name: "rank([[1,2],[3,6]]) = 1 (second row = 3× first)",
			code: `{{FUNC}}
print(matrix_rank([[1,2],[3,6]]))`,
			expected: "1\n",
		},
		{
			name: "rank([[2,4,6],[1,2,3]]) = 1 (rows proportional)",
			code: `{{FUNC}}
print(matrix_rank([[2,4,6],[1,2,3]]))`,
			expected: "1\n",
		},
	],
};
