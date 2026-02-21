import type { Lesson } from "../../types";

export const translation: Lesson = {
	id: "translation",
	title: "Translation",
	chapterId: "transforms",
	content: `## Translation

A **translation** moves a point in space. In matrix form, a translation by (x, y, z) is:

\`\`\`
[1 0 0 x]
[0 1 0 y]
[0 0 1 z]
[0 0 0 1]
\`\`\`

\`\`\`cpp
Matrix4 translation(double x, double y, double z) {
    return Matrix4(1,0,0,x, 0,1,0,y, 0,0,1,z, 0,0,0,1);
}
\`\`\`

### Applying to Tuples

Multiply the matrix by the tuple:

\`\`\`cpp
Tuple applyMatrix(Matrix4 M, Tuple t) {
    return Tuple(
        M.get(0,0)*t.x + M.get(0,1)*t.y + M.get(0,2)*t.z + M.get(0,3)*t.w,
        M.get(1,0)*t.x + M.get(1,1)*t.y + M.get(1,2)*t.z + M.get(1,3)*t.w,
        M.get(2,0)*t.x + M.get(2,1)*t.y + M.get(2,2)*t.z + M.get(2,3)*t.w,
        M.get(3,0)*t.x + M.get(3,1)*t.y + M.get(3,2)*t.z + M.get(3,3)*t.w
    );
}
\`\`\`

### Key Property: Vectors Are Unaffected

Since vectors have \`w = 0\`, the translation terms (\`x*w, y*w, z*w\`) vanish. Translation only moves points, never directions.

\`\`\`
translation(5,-3,2) * point(-3,4,5) = point(2, 1, 7)   ✓
translation(5,-3,2) * vector(-3,4,5) = vector(-3, 4, 5) (unchanged!)
\`\`\`

### Your Task

Implement \`translation(double x, double y, double z)\` and \`applyMatrix(Matrix4 M, Tuple t)\`.

Expected output:
\`\`\`
2 1 7 1
-3 4 5 0
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

class Tuple {
public:
	double x, y, z, w;
	Tuple(double x, double y, double z, double w) : x(x), y(y), z(z), w(w) {}
};

// Implement translation(double x, double y, double z)
// Returns a Matrix4 that translates by (x, y, z)

// Implement applyMatrix(Matrix4 M, Tuple t)
// Returns M * t as a new Tuple

int main() {
	Matrix4 T = translation(5, -3, 2);
	Tuple p(-3, 4, 5, 1);
	Tuple v(-3, 4, 5, 0);

	Tuple tp = applyMatrix(T, p);
	cout << tp.x << " " << tp.y << " " << tp.z << " " << tp.w << endl;

	Tuple tv = applyMatrix(T, v);
	cout << tv.x << " " << tv.y << " " << tv.z << " " << tv.w << endl;
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

class Tuple {
public:
	double x, y, z, w;
	Tuple(double x, double y, double z, double w) : x(x), y(y), z(z), w(w) {}
};

Matrix4 translation(double x, double y, double z) {
	return Matrix4(1,0,0,x, 0,1,0,y, 0,0,1,z, 0,0,0,1);
}

Tuple applyMatrix(Matrix4 M, Tuple t) {
	return Tuple(
		M.get(0,0)*t.x + M.get(0,1)*t.y + M.get(0,2)*t.z + M.get(0,3)*t.w,
		M.get(1,0)*t.x + M.get(1,1)*t.y + M.get(1,2)*t.z + M.get(1,3)*t.w,
		M.get(2,0)*t.x + M.get(2,1)*t.y + M.get(2,2)*t.z + M.get(2,3)*t.w,
		M.get(3,0)*t.x + M.get(3,1)*t.y + M.get(3,2)*t.z + M.get(3,3)*t.w
	);
}

int main() {
	Matrix4 T = translation(5, -3, 2);
	Tuple p(-3, 4, 5, 1);
	Tuple v(-3, 4, 5, 0);

	Tuple tp = applyMatrix(T, p);
	cout << tp.x << " " << tp.y << " " << tp.z << " " << tp.w << endl;

	Tuple tv = applyMatrix(T, v);
	cout << tv.x << " " << tv.y << " " << tv.z << " " << tv.w << endl;
	return 0;
}
`,

	tests: [
		{
			name: "translates a point but not a vector",
			expected: "2 1 7 1\n-3 4 5 0\n",
		},
	],
};
