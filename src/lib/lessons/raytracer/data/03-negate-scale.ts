import type { Lesson } from "../../types";

export const negateScale: Lesson = {
	id: "negate-scale",
	title: "Negating and Scaling Tuples",
	chapterId: "vectors",
	content: `## Negating and Scaling Tuples

Two more fundamental tuple operations: negation and scalar multiplication.

### Negation

Negating a tuple flips all components. For a vector, this reverses its direction:

\`\`\`cpp
// -tuple(1, -2, 3, -4) = tuple(-1, 2, -3, 4)
Tuple negate(Tuple a) {
    return Tuple(-a.x, -a.y, -a.z, -a.w);
}
\`\`\`

### Scalar Multiplication

Multiplying by a scalar scales all components. For a vector, this changes its length:

\`\`\`cpp
// tuple(1, -2, 3, -4) * 3.5 = tuple(3.5, -7, 10.5, -14)
Tuple scale(Tuple a, double t) {
    return Tuple(a.x * t, a.y * t, a.z * t, a.w * t);
}
\`\`\`

You can also divide by a scalar — just multiply by its reciprocal:
\`\`\`cpp
Tuple divide(Tuple a, double t) {
    return scale(a, 1.0 / t);
}
\`\`\`

### Your Task

Implement \`negate(Tuple a)\` and \`scale(Tuple a, double t)\`.

Expected output:
\`\`\`
-1 2 -3 4
3.5 -7 10.5 -14
\`\`\``,

	starterCode: `#include <iostream>
using namespace std;

class Tuple {
public:
	double x, y, z, w;
	Tuple(double x, double y, double z, double w) : x(x), y(y), z(z), w(w) {}
};

// Implement negate(Tuple a) — flip all signs
// Implement scale(Tuple a, double t) — multiply each component by t

int main() {
	Tuple a(1, -2, 3, -4);
	Tuple neg = negate(a);
	cout << neg.x << " " << neg.y << " " << neg.z << " " << neg.w << endl;

	Tuple scaled = scale(a, 3.5);
	cout << scaled.x << " " << scaled.y << " " << scaled.z << " " << scaled.w << endl;
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

Tuple negate(Tuple a) {
	return Tuple(-a.x, -a.y, -a.z, -a.w);
}

Tuple scale(Tuple a, double t) {
	return Tuple(a.x * t, a.y * t, a.z * t, a.w * t);
}

int main() {
	Tuple a(1, -2, 3, -4);
	Tuple neg = negate(a);
	cout << neg.x << " " << neg.y << " " << neg.z << " " << neg.w << endl;

	Tuple scaled = scale(a, 3.5);
	cout << scaled.x << " " << scaled.y << " " << scaled.z << " " << scaled.w << endl;
	return 0;
}
`,

	tests: [
		{
			name: "negates and scales tuples",
			expected: "-1 2 -3 4\n3.5 -7 10.5 -14\n",
		},
	],
};
