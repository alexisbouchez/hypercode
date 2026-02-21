import type { Lesson } from "../../types";

export const magnitude: Lesson = {
	id: "magnitude",
	title: "Magnitude: The Length of a Vector",
	chapterId: "vectors",
	content: `## Magnitude: The Length of a Vector

The **magnitude** (or length) of a vector is computed using the Pythagorean theorem extended to 3D:

\`\`\`
|v| = √(x² + y² + z²)
\`\`\`

In C++, use \`sqrt\` from \`<cmath>\`:

\`\`\`cpp
#include <cmath>

double magnitude(Tuple v) {
    return sqrt(v.x*v.x + v.y*v.y + v.z*v.z + v.w*v.w);
}
\`\`\`

The w component is included in the formula for completeness, though vectors always have w=0.

### Examples

\`\`\`
magnitude(vector(1, 0, 0)) = 1
magnitude(vector(0, 1, 0)) = 1
magnitude(vector(3, 4, 0)) = √(9 + 16) = √25 = 5
magnitude(vector(1, 2, 3)) = √14 ≈ 3.7417
\`\`\`

The **3-4-5 right triangle** is useful: vectors with components 3 and 4 in any two axes have magnitude 5.

### Your Task

Implement \`magnitude(Tuple v)\` using \`sqrt\`.

Expected output:
\`\`\`
1
5
\`\`\``,

	starterCode: `#include <iostream>
#include <cmath>
using namespace std;

class Tuple {
public:
	double x, y, z, w;
	Tuple(double x, double y, double z, double w) : x(x), y(y), z(z), w(w) {}
};

// Implement magnitude(Tuple v)
// Returns sqrt(x*x + y*y + z*z + w*w)

int main() {
	Tuple v1(1, 0, 0, 0);
	Tuple v2(3, 4, 0, 0);
	cout << magnitude(v1) << endl;
	cout << magnitude(v2) << endl;
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

int main() {
	Tuple v1(1, 0, 0, 0);
	Tuple v2(3, 4, 0, 0);
	cout << magnitude(v1) << endl;
	cout << magnitude(v2) << endl;
	return 0;
}
`,

	tests: [
		{
			name: "computes vector magnitudes",
			expected: "1\n5\n",
		},
	],
};
