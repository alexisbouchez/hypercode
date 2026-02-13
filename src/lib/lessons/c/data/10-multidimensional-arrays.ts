import type { Lesson } from "../../types";

export const multidimensionalArrays: Lesson = {
	id: "multidimensional-arrays",
	title: "Multi-dimensional Arrays",
	chapterId: "arrays-and-strings",
	content: `## Multi-dimensional Arrays

C supports arrays of arrays -- multi-dimensional arrays. The most common is the 2D array:

### Declaration

\`\`\`c
int matrix[2][3] = {
    {1, 2, 3},
    {4, 5, 6}
};
\`\`\`

This creates a 2-row, 3-column matrix. In memory, the rows are stored contiguously.

> Holodeck grids: a 2D array of emitters creating a 3D world. Every \`matrix[row][col]\` is one photon emitter in the grid.

### Accessing Elements

\`\`\`c
int val = matrix[0][2];  // row 0, column 2 = 3
matrix[1][0] = 99;       // set row 1, column 0 to 99
\`\`\`

### Iterating

Use nested loops:

\`\`\`c
for (int row = 0; row < 2; row++) {
    for (int col = 0; col < 3; col++) {
        printf("%d ", matrix[row][col]);
    }
    printf("\\n");
}
\`\`\`

### Memory Layout

A 2D array \`int m[R][C]\` is laid out as R*C contiguous integers. \`m[i][j]\` is at offset \`(i * C + j) * sizeof(int)\`.

### Your Task

Create a 3x3 identity matrix (1s on the diagonal, 0s elsewhere) and print it row by row. Each row should have numbers separated by spaces, followed by a newline.`,

	starterCode: `#include <stdio.h>

int main() {
\tint matrix[3][3] = {0};
\t// Set diagonal to 1
\t// Print the matrix
\treturn 0;
}
`,

	solution: `#include <stdio.h>

int main() {
\tint matrix[3][3] = {0};
\tfor (int i = 0; i < 3; i++) {
\t\tmatrix[i][i] = 1;
\t}
\tfor (int row = 0; row < 3; row++) {
\t\tfor (int col = 0; col < 3; col++) {
\t\t\tif (col > 0) printf(" ");
\t\t\tprintf("%d", matrix[row][col]);
\t\t}
\t\tprintf("\\n");
\t}
\treturn 0;
}
`,

	tests: [
		{
			name: "prints identity matrix",
			expected: "1 0 0\n0 1 0\n0 0 1\n",
		},
	],
};
