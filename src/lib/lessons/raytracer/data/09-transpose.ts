import type { Lesson } from "../../types";

export const transpose: Lesson = {
	id: "transpose",
	title: "The Identity Matrix and Transpose",
	chapterId: "matrices",
	content: `## The Identity Matrix and Transpose

### The Identity Matrix

The identity matrix is the matrix equivalent of the number 1 — multiplying any matrix or tuple by it returns the original:

\`\`\`
I = [1 0 0 0]
    [0 1 0 0]
    [0 0 1 0]
    [0 0 0 1]
\`\`\`

A useful factory function:
\`\`\`cpp
Matrix4 identity() {
    return Matrix4(1,0,0,0, 0,1,0,0, 0,0,1,0, 0,0,0,1);
}
\`\`\`

### Transposing a Matrix

Transposing a matrix flips it along its diagonal — rows become columns:

\`\`\`
M^T[i][j] = M[j][i]
\`\`\`

\`\`\`cpp
Matrix4 transpose(Matrix4 M) {
    return Matrix4(
        M.get(0,0), M.get(1,0), M.get(2,0), M.get(3,0),
        M.get(0,1), M.get(1,1), M.get(2,1), M.get(3,1),
        M.get(0,2), M.get(1,2), M.get(2,2), M.get(3,2),
        M.get(0,3), M.get(1,3), M.get(2,3), M.get(3,3)
    );
}
\`\`\`

Note: \`transpose(identity()) = identity()\`.

Transposing is used internally when computing surface normals for transformed shapes.

### Your Task

Implement \`transpose(Matrix4 M)\`.

Given \`M = [1,2,3,4; 5,6,7,8; 9,8,7,6; 5,4,3,2]\`:
- \`M^T[0][1]\` should equal \`M[1][0] = 5\`
- \`M^T[1][0]\` should equal \`M[0][1] = 2\`

Expected output:
\`\`\`
5
2
\`\`\``,

	starterCode: `#include <iostream>
using namespace std;

class Matrix4 {
public:
	Matrix4(double a, double b, double c, double d, double e, double f, double g, double h, double i, double j, double k, double l, double m, double n, double o, double p) {
		this->data = [a,b,c,d, e,f,g,h, i,j,k,l, m,n,o,p];
	}
	double get(int row, int col) const {
		return this->data[row * 4 + col];
	}
};

// Implement transpose(Matrix4 M)
// M^T[i][j] = M[j][i]

int main() {
	Matrix4 M(1,2,3,4, 5,6,7,8, 9,8,7,6, 5,4,3,2);
	Matrix4 T = transpose(M);
	cout << T.get(0, 1) << endl;
	cout << T.get(1, 0) << endl;
	return 0;
}
`,

	solution: `#include <iostream>
using namespace std;

class Matrix4 {
public:
	Matrix4(double a, double b, double c, double d, double e, double f, double g, double h, double i, double j, double k, double l, double m, double n, double o, double p) {
		this->data = [a,b,c,d, e,f,g,h, i,j,k,l, m,n,o,p];
	}
	double get(int row, int col) const {
		return this->data[row * 4 + col];
	}
};

Matrix4 transpose(Matrix4 M) {
	return Matrix4(
		M.get(0,0), M.get(1,0), M.get(2,0), M.get(3,0),
		M.get(0,1), M.get(1,1), M.get(2,1), M.get(3,1),
		M.get(0,2), M.get(1,2), M.get(2,2), M.get(3,2),
		M.get(0,3), M.get(1,3), M.get(2,3), M.get(3,3)
	);
}

int main() {
	Matrix4 M(1,2,3,4, 5,6,7,8, 9,8,7,6, 5,4,3,2);
	Matrix4 T = transpose(M);
	cout << T.get(0, 1) << endl;
	cout << T.get(1, 0) << endl;
	return 0;
}
`,

	tests: [
		{
			name: "transposes a matrix",
			expected: "5\n2\n",
		},
	],
};
