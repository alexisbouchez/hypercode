import type { Lesson } from "../../types";

export const matrices: Lesson = {
	id: "matrices",
	title: "4×4 Matrices",
	chapterId: "matrices",
	content: `## 4×4 Matrices

Matrices are the engine of 3D transformations. A 4×4 matrix transforms a tuple (a point or vector) by multiplying them together.

### Storing a Matrix

We'll store a 4×4 matrix as 16 values in a flat array (row-major order):

\`\`\`cpp
class Matrix4 {
public:
    Matrix4(double a, double b, double c, double d,
            double e, double f, double g, double h,
            double i, double j, double k, double l,
            double m, double n, double o, double p) {
        this->data = [a,b,c,d, e,f,g,h, i,j,k,l, m,n,o,p];
    }
    double get(int row, int col) const {
        return this->data[row * 4 + col];
    }
};
\`\`\`

\`get(row, col)\` retrieves element at row \`row\` and column \`col\`.

### Matrix Multiplication

Multiplying two 4×4 matrices: element \`[row][col]\` of the result is the dot product of row \`row\` from A with column \`col\` from B:

\`\`\`cpp
double matElement(Matrix4 A, Matrix4 B, int row, int col) {
    double sum = 0;
    for (int k = 0; k < 4; k++) {
        sum += A.get(row, k) * B.get(k, col);
    }
    return sum;
}
\`\`\`

### Your Task

Implement \`matElement(Matrix4 A, Matrix4 B, int row, int col)\` that computes a single element of the matrix product A × B.

From the book's test case:
\`\`\`
A = [1,2,3,4; 5,6,7,8; 9,8,7,6; 5,4,3,2]
B = [-2,1,2,3; 3,2,1,-1; 4,3,6,5; 1,2,7,8]
A*B[0][0] = 20, A*B[1][2] = 114
\`\`\`

Expected output:
\`\`\`
20
114
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

// Implement matElement(Matrix4 A, Matrix4 B, int row, int col)
// Returns the [row][col] element of A * B
// Hint: sum A.get(row, k) * B.get(k, col) for k = 0..3

int main() {
	Matrix4 A(1,2,3,4, 5,6,7,8, 9,8,7,6, 5,4,3,2);
	Matrix4 B(-2,1,2,3, 3,2,1,-1, 4,3,6,5, 1,2,7,8);
	cout << matElement(A, B, 0, 0) << endl;
	cout << matElement(A, B, 1, 2) << endl;
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

double matElement(Matrix4 A, Matrix4 B, int row, int col) {
	double sum = 0;
	for (int k = 0; k < 4; k++) {
		sum += A.get(row, k) * B.get(k, col);
	}
	return sum;
}

int main() {
	Matrix4 A(1,2,3,4, 5,6,7,8, 9,8,7,6, 5,4,3,2);
	Matrix4 B(-2,1,2,3, 3,2,1,-1, 4,3,6,5, 1,2,7,8);
	cout << matElement(A, B, 0, 0) << endl;
	cout << matElement(A, B, 1, 2) << endl;
	return 0;
}
`,

	tests: [
		{
			name: "computes matrix product elements",
			expected: "20\n114\n",
		},
	],
};
