import type { Lesson } from "../../types";

export const sphereIntersect: Lesson = {
	id: "sphere-intersect",
	title: "Ray-Sphere Intersection",
	chapterId: "rays",
	content: `## Ray-Sphere Intersection

The heart of any ray tracer: does a ray hit an object? For a unit sphere centered at the origin, the math is elegant.

### The Discriminant

Substituting the ray equation into the sphere equation \`x² + y² + z² = 1\` gives a quadratic in \`t\`:

\`\`\`
a*t² + b*t + c = 0
\`\`\`

Where:
\`\`\`
a = dot(direction, direction)
b = 2 * dot(direction, sphere_to_ray)
c = dot(sphere_to_ray, sphere_to_ray) - 1
sphere_to_ray = ray.origin - point(0,0,0)
\`\`\`

The **discriminant** \`b² - 4ac\` tells you everything:
- \`< 0\`: ray misses the sphere (no real solutions)
- \`= 0\`: ray is tangent (one intersection)
- \`> 0\`: ray passes through (two intersections)

\`\`\`cpp
int intersectCount(double rox, double roy, double roz,
                   double rdx, double rdy, double rdz) {
    double a = rdx*rdx + rdy*rdy + rdz*rdz;
    double b = 2*(rdx*rox + rdy*roy + rdz*roz);
    double c = rox*rox + roy*roy + roz*roz - 1;
    double disc = b*b - 4*a*c;
    if (disc < 0) return 0;
    return 2;
}
\`\`\`

### The Intersection Points

If \`disc >= 0\`:
\`\`\`
t1 = (-b - sqrt(disc)) / (2*a)
t2 = (-b + sqrt(disc)) / (2*a)
\`\`\`

The **hit** is the smallest non-negative t — the closest visible intersection.

### Your Task

Implement \`intersectCount\` that returns the number of intersections (0 or 2).

Expected output:
\`\`\`
2
0
\`\`\``,

	starterCode: `#include <iostream>
#include <cmath>
using namespace std;

// Implement intersectCount(rox, roy, roz, rdx, rdy, rdz)
// Returns 2 if the ray hits the unit sphere, 0 if it misses
// Compute: a = rdx^2 + rdy^2 + rdz^2
//          b = 2*(rdx*rox + rdy*roy + rdz*roz)
//          c = rox^2 + roy^2 + roz^2 - 1
//          discriminant = b*b - 4*a*c
// Return 0 if discriminant < 0, else 2

int main() {
	// Ray through center: hits at t=4 and t=6
	cout << intersectCount(0, 0, -5, 0, 0, 1) << endl;
	// Ray misses: origin y=2 is outside sphere
	cout << intersectCount(0, 2, -5, 0, 0, 1) << endl;
	return 0;
}
`,

	solution: `#include <iostream>
#include <cmath>
using namespace std;

int intersectCount(double rox, double roy, double roz,
                   double rdx, double rdy, double rdz) {
	double a = rdx*rdx + rdy*rdy + rdz*rdz;
	double b = 2*(rdx*rox + rdy*roy + rdz*roz);
	double c = rox*rox + roy*roy + roz*roz - 1;
	double disc = b*b - 4*a*c;
	if (disc < 0) return 0;
	return 2;
}

int main() {
	cout << intersectCount(0, 0, -5, 0, 0, 1) << endl;
	cout << intersectCount(0, 2, -5, 0, 0, 1) << endl;
	return 0;
}
`,

	tests: [
		{
			name: "counts ray-sphere intersections",
			expected: "2\n0\n",
		},
	],
};
