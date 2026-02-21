import type { Lesson } from "../../types";

export const scaling: Lesson = {
	id: "scaling",
	title: "Scaling",
	chapterId: "transforms",
	content: `## Scaling

A **scaling** transformation stretches or shrinks along each axis independently:

\`\`\`
[x 0 0 0]
[0 y 0 0]
[0 0 z 0]
[0 0 0 1]
\`\`\`

\`\`\`cpp
Matrix4 scaling(double x, double y, double z) {
    return Matrix4(x,0,0,0, 0,y,0,0, 0,0,z,0, 0,0,0,1);
}
\`\`\`

### Properties

- **Uniform scaling**: use the same value for all three axes → sphere stays a sphere
- **Non-uniform scaling**: different values → stretches a sphere into an ellipsoid
- **Reflection**: use a negative scale factor to flip along an axis:
  \`scaling(-1, 1, 1)\` reflects across the yz-plane

### Scaling Vectors and Points

Unlike translation, scaling affects **both** points and vectors:
\`\`\`
scaling(2,3,4) * point(-4,6,8) = point(-8, 18, 32)
scaling(2,3,4) * vector(-4,6,8) = vector(-8, 18, 32)
\`\`\`

### Building Complex Objects

Combine translation and scaling to position objects in your scene. A unit sphere at origin scaled by 2 gives a sphere of radius 2:
\`\`\`cpp
Matrix4 transform = scaling(2, 2, 2);
// Apply to each ray: transform the ray by the inverse transform instead
\`\`\`

### Your Task

Implement \`scaling(double x, double y, double z)\` using the Matrix4 and applyMatrix infrastructure from the previous lesson.

Expected output:
\`\`\`
-8 18 32 1
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

Tuple applyMatrix(Matrix4 M, Tuple t) {
	return Tuple(
		M.get(0,0)*t.x + M.get(0,1)*t.y + M.get(0,2)*t.z + M.get(0,3)*t.w,
		M.get(1,0)*t.x + M.get(1,1)*t.y + M.get(1,2)*t.z + M.get(1,3)*t.w,
		M.get(2,0)*t.x + M.get(2,1)*t.y + M.get(2,2)*t.z + M.get(2,3)*t.w,
		M.get(3,0)*t.x + M.get(3,1)*t.y + M.get(3,2)*t.z + M.get(3,3)*t.w
	);
}

// Implement scaling(double x, double y, double z)

int main() {
	Matrix4 S = scaling(2, 3, 4);
	Tuple p(-4, 6, 8, 1);
	Tuple result = applyMatrix(S, p);
	cout << result.x << " " << result.y << " " << result.z << " " << result.w << endl;
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

Tuple applyMatrix(Matrix4 M, Tuple t) {
	return Tuple(
		M.get(0,0)*t.x + M.get(0,1)*t.y + M.get(0,2)*t.z + M.get(0,3)*t.w,
		M.get(1,0)*t.x + M.get(1,1)*t.y + M.get(1,2)*t.z + M.get(1,3)*t.w,
		M.get(2,0)*t.x + M.get(2,1)*t.y + M.get(2,2)*t.z + M.get(2,3)*t.w,
		M.get(3,0)*t.x + M.get(3,1)*t.y + M.get(3,2)*t.z + M.get(3,3)*t.w
	);
}

Matrix4 scaling(double x, double y, double z) {
	return Matrix4(x,0,0,0, 0,y,0,0, 0,0,z,0, 0,0,0,1);
}

int main() {
	Matrix4 S = scaling(2, 3, 4);
	Tuple p(-4, 6, 8, 1);
	Tuple result = applyMatrix(S, p);
	cout << result.x << " " << result.y << " " << result.z << " " << result.w << endl;
	return 0;
}
`,

	tests: [
		{
			name: "scales a point",
			expected: "-8 18 32 1\n",
		},
	],
};
