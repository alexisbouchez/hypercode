import type { Lesson } from "../../types";

export const rotation: Lesson = {
	id: "rotation",
	title: "Rotation",
	chapterId: "transforms",
	content: `## Rotation

Rotation matrices use sine and cosine to rotate points around the x, y, and z axes. C++ provides \`cos\` and \`sin\` from \`<cmath>\`, and the constant \`M_PI\` for π.

### Rotation Around the Y Axis

\`\`\`
rotation_y(θ) = [cos θ   0  sin θ  0]
                [0       1  0      0]
                [-sin θ  0  cos θ  0]
                [0       0  0      1]
\`\`\`

\`\`\`cpp
Matrix4 rotation_y(double radians) {
    double c = cos(radians);
    double s = sin(radians);
    return Matrix4(c,0,s,0, 0,1,0,0, -s,0,c,0, 0,0,0,1);
}
\`\`\`

Similarly for x and z axes:
\`\`\`cpp
Matrix4 rotation_x(double radians) {
    double c = cos(radians);
    double s = sin(radians);
    return Matrix4(1,0,0,0, 0,c,-s,0, 0,s,c,0, 0,0,0,1);
}

Matrix4 rotation_z(double radians) {
    double c = cos(radians);
    double s = sin(radians);
    return Matrix4(c,-s,0,0, s,c,0,0, 0,0,1,0, 0,0,0,1);
}
\`\`\`

### Floating-Point Precision

Due to floating-point arithmetic, \`cos(π/2)\` is not exactly 0. Use this helper to round small values:

\`\`\`cpp
double round5(double x) {
    return floor(x * 100000.0 + 0.5) / 100000.0;
}
\`\`\`

### Your Task

Implement \`rotation_y(double radians)\`. Then rotate \`point(0, 0, 1)\` by π/2 radians (90°).

Expected result: \`point(1, 0, 0)\`

Expected output:
\`\`\`
1 0 0
\`\`\``,

	starterCode: `#include <iostream>
#include <cmath>
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

Tuple applyMatrix(Matrix4 M, Tuple t) {
	return Tuple(
		M.get(0,0)*t.x + M.get(0,1)*t.y + M.get(0,2)*t.z + M.get(0,3)*t.w,
		M.get(1,0)*t.x + M.get(1,1)*t.y + M.get(1,2)*t.z + M.get(1,3)*t.w,
		M.get(2,0)*t.x + M.get(2,1)*t.y + M.get(2,2)*t.z + M.get(2,3)*t.w,
		M.get(3,0)*t.x + M.get(3,1)*t.y + M.get(3,2)*t.z + M.get(3,3)*t.w
	);
}

double round5(double x) {
	return floor(x * 100000.0 + 0.5) / 100000.0;
}

// Implement rotation_y(double radians)
// rotation_y matrix: [c,0,s,0, 0,1,0,0, -s,0,c,0, 0,0,0,1]
// where c = cos(radians), s = sin(radians)

int main() {
	Tuple p(0, 0, 1, 1);
	Matrix4 R = rotation_y(M_PI / 2.0);
	Tuple result = applyMatrix(R, p);
	cout << round5(result.x) << " " << round5(result.y) << " " << round5(result.z) << endl;
	return 0;
}
`,

	solution: `#include <iostream>
#include <cmath>
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

Tuple applyMatrix(Matrix4 M, Tuple t) {
	return Tuple(
		M.get(0,0)*t.x + M.get(0,1)*t.y + M.get(0,2)*t.z + M.get(0,3)*t.w,
		M.get(1,0)*t.x + M.get(1,1)*t.y + M.get(1,2)*t.z + M.get(1,3)*t.w,
		M.get(2,0)*t.x + M.get(2,1)*t.y + M.get(2,2)*t.z + M.get(2,3)*t.w,
		M.get(3,0)*t.x + M.get(3,1)*t.y + M.get(3,2)*t.z + M.get(3,3)*t.w
	);
}

double round5(double x) {
	return floor(x * 100000.0 + 0.5) / 100000.0;
}

Matrix4 rotation_y(double radians) {
	double c = cos(radians);
	double s = sin(radians);
	return Matrix4(c,0,s,0, 0,1,0,0, -s,0,c,0, 0,0,0,1);
}

int main() {
	Tuple p(0, 0, 1, 1);
	Matrix4 R = rotation_y(M_PI / 2.0);
	Tuple result = applyMatrix(R, p);
	cout << round5(result.x) << " " << round5(result.y) << " " << round5(result.z) << endl;
	return 0;
}
`,

	tests: [
		{
			name: "rotates a point 90 degrees around y-axis",
			expected: "1 0 0\n",
		},
	],
};
