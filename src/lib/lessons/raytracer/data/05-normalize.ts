import type { Lesson } from "../../types";

export const normalize: Lesson = {
	id: "normalize",
	title: "Normalizing a Vector",
	chapterId: "vectors",
	content: `## Normalizing a Vector

A **unit vector** (or normalized vector) has a magnitude of 1. Normalizing converts any vector to a unit vector pointing in the same direction.

\`\`\`cpp
Tuple normalize(Tuple v) {
    double m = magnitude(v);
    return Tuple(v.x / m, v.y / m, v.z / m, v.w / m);
}
\`\`\`

### Why Normalize?

Many ray tracer computations require unit vectors — lighting calculations, ray directions, surface normals. Normalizing keeps geometry calculations consistent regardless of the original vector's length.

### Examples

\`\`\`
normalize(vector(4, 0, 0)) = vector(1, 0, 0)
normalize(vector(1, 2, 3)) ≈ vector(0.26726, 0.53452, 0.80178)
\`\`\`

After normalizing, the magnitude of the result should always be 1.

### Your Task

Implement \`normalize(Tuple v)\` using the \`magnitude\` function.

Expected output:
\`\`\`
1 0 0 0
\`\`\``,

	starterCode: `#include <iostream>
#include <cmath>
using namespace std;

class Tuple {
public:
	double x, y, z, w;
	Tuple(double x, double y, double z, double w) : x(x), y(y), z(z), w(w) {}
};

double magnitude(Tuple v) {
	return sqrt(v.x*v.x + v.y*v.y + v.z*v.z + v.w*v.w);
}

// Implement normalize(Tuple v)
// Divide each component by magnitude(v)

int main() {
	Tuple v(4, 0, 0, 0);
	Tuple n = normalize(v);
	cout << n.x << " " << n.y << " " << n.z << " " << n.w << endl;
	return 0;
}
`,

	solution: `#include <iostream>
#include <cmath>
using namespace std;

class Tuple {
public:
	double x, y, z, w;
	Tuple(double x, double y, double z, double w) : x(x), y(y), z(z), w(w) {}
};

double magnitude(Tuple v) {
	return sqrt(v.x*v.x + v.y*v.y + v.z*v.z + v.w*v.w);
}

Tuple normalize(Tuple v) {
	double m = magnitude(v);
	return Tuple(v.x / m, v.y / m, v.z / m, v.w / m);
}

int main() {
	Tuple v(4, 0, 0, 0);
	Tuple n = normalize(v);
	cout << n.x << " " << n.y << " " << n.z << " " << n.w << endl;
	return 0;
}
`,

	tests: [
		{
			name: "normalizes a vector",
			expected: "1 0 0 0\n",
		},
	],
};
