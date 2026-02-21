import type { Lesson } from "../../types";

export const dotCross: Lesson = {
	id: "dot-cross",
	title: "Dot and Cross Products",
	chapterId: "vectors",
	content: `## Dot and Cross Products

Two fundamental vector operations power almost all lighting math in the ray tracer.

### Dot Product

The dot product returns a **scalar** — the sum of component-wise products:

\`\`\`
dot(a, b) = a.x*b.x + a.y*b.y + a.z*b.z
\`\`\`

\`\`\`cpp
double dot(Tuple a, Tuple b) {
    return a.x*b.x + a.y*b.y + a.z*b.z + a.w*b.w;
}
\`\`\`

The dot product has a geometric meaning: \`dot(a, b) = |a| * |b| * cos(θ)\` where θ is the angle between them. For normalized vectors:
- \`dot = 1\` → same direction
- \`dot = 0\` → perpendicular
- \`dot = -1\` → opposite directions

### Cross Product

The cross product returns a **vector** perpendicular to both inputs:

\`\`\`
cross(a, b).x = a.y*b.z - a.z*b.y
cross(a, b).y = a.z*b.x - a.x*b.z
cross(a, b).z = a.x*b.y - a.y*b.x
\`\`\`

\`\`\`cpp
Tuple cross(Tuple a, Tuple b) {
    return Tuple(
        a.y*b.z - a.z*b.y,
        a.z*b.x - a.x*b.z,
        a.x*b.y - a.y*b.x,
        0
    );
}
\`\`\`

Note: \`cross(a, b) = -cross(b, a)\`. Order matters!

### Your Task

Implement both \`dot\` and \`cross\` functions.

Expected output:
\`\`\`
Dot: 20
Cross: -1 2 -1
\`\`\``,

	starterCode: `#include <iostream>
using namespace std;

class Tuple {
public:
	double x, y, z, w;
	Tuple(double x, double y, double z, double w) : x(x), y(y), z(z), w(w) {}
};

// Implement double dot(Tuple a, Tuple b)
// Implement Tuple cross(Tuple a, Tuple b)

int main() {
	Tuple a(1, 2, 3, 0);
	Tuple b(2, 3, 4, 0);
	cout << "Dot: " << dot(a, b) << endl;
	Tuple c = cross(a, b);
	cout << "Cross: " << c.x << " " << c.y << " " << c.z << endl;
	return 0;
}
`,

	solution: `#include <iostream>
using namespace std;

class Tuple {
public:
	double x, y, z, w;
	Tuple(double x, double y, double z, double w) : x(x), y(y), z(z), w(w) {}
};

double dot(Tuple a, Tuple b) {
	return a.x*b.x + a.y*b.y + a.z*b.z + a.w*b.w;
}

Tuple cross(Tuple a, Tuple b) {
	return Tuple(
		a.y*b.z - a.z*b.y,
		a.z*b.x - a.x*b.z,
		a.x*b.y - a.y*b.x,
		0
	);
}

int main() {
	Tuple a(1, 2, 3, 0);
	Tuple b(2, 3, 4, 0);
	cout << "Dot: " << dot(a, b) << endl;
	Tuple c = cross(a, b);
	cout << "Cross: " << c.x << " " << c.y << " " << c.z << endl;
	return 0;
}
`,

	tests: [
		{
			name: "computes dot and cross products",
			expected: "Dot: 20\nCross: -1 2 -1\n",
		},
	],
};
