import type { Lesson } from "../../types";

export const colors: Lesson = {
	id: "colors",
	title: "Colors",
	chapterId: "colors",
	content: `## Colors

Colors in the ray tracer are represented as \`(red, green, blue)\` triples where 0 is no intensity and 1 is full intensity. Values can exceed 1 (very bright) — they're clamped when writing to images.

### The Color Class

\`\`\`cpp
class Color {
public:
    double r, g, b;
    Color(double r, double g, double b) : r(r), g(g), b(b) {}
};
\`\`\`

### Color Operations

Colors support the same arithmetic as tuples:

\`\`\`cpp
// Addition: mix two lights
Color addColors(Color a, Color b) {
    return Color(a.r + b.r, a.g + b.g, a.b + b.b);
}
\`\`\`

The **Hadamard product** (component-wise multiplication) blends a surface color with a light color:

\`\`\`cpp
// Hadamard product: surface color filtered by light color
Color hadamard(Color a, Color b) {
    return Color(a.r * b.r, a.g * b.g, a.b * b.b);
}
\`\`\`

### Example

A red surface \`Color(1, 0, 0)\` illuminated by white light \`Color(1, 1, 1)\`:
\`\`\`
hadamard(red, white) = Color(1*1, 0*1, 0*1) = Color(1, 0, 0)
\`\`\`
The surface appears red. Under a green light \`Color(0, 1, 0)\`:
\`\`\`
hadamard(red, green) = Color(1*0, 0*1, 0*0) = Color(0, 0, 0)
\`\`\`
The surface appears black — no reflected light.

### Your Task

Implement \`addColors\` and \`hadamard\`.

Expected output:
\`\`\`
1 1 0
2 6 12
\`\`\``,

	starterCode: `#include <iostream>
using namespace std;

class Color {
public:
	double r, g, b;
	Color(double r, double g, double b) : r(r), g(g), b(b) {}
};

// Implement addColors(Color a, Color b) — component-wise addition
// Implement hadamard(Color a, Color b) — component-wise multiplication

int main() {
	Color c1(1, 0, 0);
	Color c2(0, 1, 0);
	Color sum = addColors(c1, c2);
	cout << sum.r << " " << sum.g << " " << sum.b << endl;

	Color c3(1, 2, 3);
	Color c4(2, 3, 4);
	Color prod = hadamard(c3, c4);
	cout << prod.r << " " << prod.g << " " << prod.b << endl;
	return 0;
}
`,

	solution: `#include <iostream>
using namespace std;

class Color {
public:
	double r, g, b;
	Color(double r, double g, double b) : r(r), g(g), b(b) {}
};

Color addColors(Color a, Color b) {
	return Color(a.r + b.r, a.g + b.g, a.b + b.b);
}

Color hadamard(Color a, Color b) {
	return Color(a.r * b.r, a.g * b.g, a.b * b.b);
}

int main() {
	Color c1(1, 0, 0);
	Color c2(0, 1, 0);
	Color sum = addColors(c1, c2);
	cout << sum.r << " " << sum.g << " " << sum.b << endl;

	Color c3(1, 2, 3);
	Color c4(2, 3, 4);
	Color prod = hadamard(c3, c4);
	cout << prod.r << " " << prod.g << " " << prod.b << endl;
	return 0;
}
`,

	tests: [
		{
			name: "adds colors and computes hadamard product",
			expected: "1 1 0\n2 6 12\n",
		},
	],
};
