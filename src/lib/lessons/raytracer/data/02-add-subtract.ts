import type { Lesson } from "../../types";

export const addSubtract: Lesson = {
	id: "add-subtract",
	title: "Adding and Subtracting Tuples",
	chapterId: "vectors",
	content: `## Adding and Subtracting Tuples

Tuples support component-wise arithmetic. Adding a vector to a point gives a new point. Subtracting two points gives a vector.

### Addition

\`\`\`
point + vector = point  (w: 1 + 0 = 1)
vector + vector = vector  (w: 0 + 0 = 0)
\`\`\`

\`\`\`cpp
// Adding tuple(3,-2,5,1) + tuple(-2,3,1,0):
// → tuple(1, 1, 6, 1)  (a point)
\`\`\`

### Subtraction

\`\`\`
point - point = vector  (w: 1 - 1 = 0, a direction between two points)
point - vector = point  (w: 1 - 0 = 1)
\`\`\`

\`\`\`cpp
// Subtracting point(3,2,1) - point(5,6,7):
// → vector(-2, -4, -6, 0)
\`\`\`

### Implementation

\`\`\`cpp
Tuple add(Tuple a, Tuple b) {
    return Tuple(a.x + b.x, a.y + b.y, a.z + b.z, a.w + b.w);
}

Tuple subtract(Tuple a, Tuple b) {
    return Tuple(a.x - b.x, a.y - b.y, a.z - b.z, a.w - b.w);
}
\`\`\`

### Your Task

Implement \`add(Tuple a, Tuple b)\` and \`subtract(Tuple a, Tuple b)\` free functions that return new Tuples.

Expected output:
\`\`\`
1 1 6 1
-2 -4 -6 0
\`\`\``,

	starterCode: `#include <iostream>
using namespace std;

class Tuple {
public:
	double x, y, z, w;
	Tuple(double x, double y, double z, double w) : x(x), y(y), z(z), w(w) {}
};

// Implement add(Tuple a, Tuple b) — component-wise addition
// Implement subtract(Tuple a, Tuple b) — component-wise subtraction

int main() {
	Tuple a(3, -2, 5, 1);
	Tuple b(-2, 3, 1, 0);
	Tuple sum = add(a, b);
	cout << sum.x << " " << sum.y << " " << sum.z << " " << sum.w << endl;

	Tuple p1(3, 2, 1, 1);
	Tuple p2(5, 6, 7, 1);
	Tuple diff = subtract(p1, p2);
	cout << diff.x << " " << diff.y << " " << diff.z << " " << diff.w << endl;
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

Tuple add(Tuple a, Tuple b) {
	return Tuple(a.x + b.x, a.y + b.y, a.z + b.z, a.w + b.w);
}

Tuple subtract(Tuple a, Tuple b) {
	return Tuple(a.x - b.x, a.y - b.y, a.z - b.z, a.w - b.w);
}

int main() {
	Tuple a(3, -2, 5, 1);
	Tuple b(-2, 3, 1, 0);
	Tuple sum = add(a, b);
	cout << sum.x << " " << sum.y << " " << sum.z << " " << sum.w << endl;

	Tuple p1(3, 2, 1, 1);
	Tuple p2(5, 6, 7, 1);
	Tuple diff = subtract(p1, p2);
	cout << diff.x << " " << diff.y << " " << diff.z << " " << diff.w << endl;
	return 0;
}
`,

	tests: [
		{
			name: "adds and subtracts tuples",
			expected: "1 1 6 1\n-2 -4 -6 0\n",
		},
	],
};
