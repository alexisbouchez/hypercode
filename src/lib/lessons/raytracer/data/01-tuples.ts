import type { Lesson } from "../../types";

export const tuples: Lesson = {
	id: "tuples",
	title: "Tuples: Points and Vectors",
	chapterId: "vectors",
	content: `## Tuples: Points and Vectors

The ray tracer is built on a single mathematical primitive: the **tuple**. A tuple is an ordered list of four numbers — \`(x, y, z, w)\`.

The \`w\` component is the key:
- \`w = 1\` means the tuple is a **point** — a location in space
- \`w = 0\` means the tuple is a **vector** — a direction with magnitude

This distinction matters because points and vectors behave differently under transformations. Translating a point moves it; translating a vector does nothing.

### The Tuple Class

\`\`\`cpp
class Tuple {
public:
    double x, y, z, w;
    Tuple(double x, double y, double z, double w) : x(x), y(y), z(z), w(w) {}

    bool isPoint() const { return this->w == 1.0; }
    bool isVector() const { return this->w == 0.0; }
};
\`\`\`

Member variables are accessed with \`this->\` inside methods.

### Floating-Point Equality

Since coordinates are stored as \`double\`, comparing with \`==\` works for w (which is always exactly 0 or 1), but for other comparisons you'll use an epsilon:

\`\`\`cpp
const double EPSILON = 0.00001;
bool equal(double a, double b) {
    return abs(a - b) < EPSILON;
}
\`\`\`

### Your Task

Implement the \`Tuple\` class with fields \`x\`, \`y\`, \`z\`, \`w\` and methods \`isPoint()\` and \`isVector()\`.

Then create a point \`p(4, -4, 3, 1)\` and a vector \`v(4, -4, 3, 0)\` and print:
- \`p.w=1 isPoint=1\`
- \`v.w=0 isVector=1\``,

	starterCode: `#include <iostream>
using namespace std;

// Define a Tuple class with:
// - double x, y, z, w (four components)
// - Constructor: Tuple(double x, double y, double z, double w)
// - bool isPoint() const  — returns true if w == 1.0
// - bool isVector() const — returns true if w == 0.0

int main() {
	Tuple p(4, -4, 3, 1);
	Tuple v(4, -4, 3, 0);
	cout << "p.w=" << p.w << " isPoint=" << p.isPoint() << endl;
	cout << "v.w=" << v.w << " isVector=" << v.isVector() << endl;
	return 0;
}
`,

	solution: `#include <iostream>
using namespace std;

class Tuple {
public:
	double x, y, z, w;
	Tuple(double x, double y, double z, double w) : x(x), y(y), z(z), w(w) {}
	bool isPoint() const { return this->w == 1.0; }
	bool isVector() const { return this->w == 0.0; }
};

int main() {
	Tuple p(4, -4, 3, 1);
	Tuple v(4, -4, 3, 0);
	cout << "p.w=" << p.w << " isPoint=" << p.isPoint() << endl;
	cout << "v.w=" << v.w << " isVector=" << v.isVector() << endl;
	return 0;
}
`,

	tests: [
		{
			name: "identifies points and vectors",
			expected: "p.w=1 isPoint=1\nv.w=0 isVector=1\n",
		},
	],
};
