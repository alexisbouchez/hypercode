import type { Lesson } from "../../types";

export const rays: Lesson = {
	id: "rays",
	title: "Rays and Positions",
	chapterId: "rays",
	content: `## Rays and Positions

A **ray** is a line with an origin and a direction:

\`\`\`
ray = origin + direction * t
\`\`\`

The parameter \`t\` moves you along the ray: \`t = 0\` is the origin, \`t = 1\` is one step in the direction, negative \`t\` is behind the origin.

### The Ray Class

\`\`\`cpp
class Ray {
public:
    double ox, oy, oz;  // origin
    double dx, dy, dz;  // direction
    Ray(double ox, double oy, double oz,
        double dx, double dy, double dz)
        : ox(ox), oy(oy), oz(oz), dx(dx), dy(dy), dz(dz) {}
};
\`\`\`

### Computing Position

\`\`\`cpp
// Position along ray at parameter t
double posX(Ray r, double t) { return r.ox + r.dx * t; }
double posY(Ray r, double t) { return r.oy + r.dy * t; }
double posZ(Ray r, double t) { return r.oz + r.dz * t; }
\`\`\`

Or add position methods directly to the class:
\`\`\`cpp
double posX(double t) const { return this->ox + this->dx * t; }
\`\`\`

### Example

\`\`\`
ray(origin=point(2,3,4), direction=vector(1,0,0)):
  position(0)   = point(2, 3, 4)   (origin)
  position(1)   = point(3, 3, 4)   (one step right)
  position(-1)  = point(1, 3, 4)   (one step left)
  position(2.5) = point(4.5, 3, 4)
\`\`\`

### Your Task

Add \`posX\`, \`posY\`, \`posZ\` methods to the Ray class.

Expected output:
\`\`\`
4.5 3 4
\`\`\``,

	starterCode: `#include <iostream>
using namespace std;

// Add posX(double t), posY(double t), posZ(double t) methods
// that compute the position along the ray at parameter t

class Ray {
public:
	double ox, oy, oz;
	double dx, dy, dz;
	Ray(double ox, double oy, double oz, double dx, double dy, double dz) : ox(ox), oy(oy), oz(oz), dx(dx), dy(dy), dz(dz) {}
};

int main() {
	Ray r(2, 3, 4, 1, 0, 0);
	cout << r.posX(2.5) << " " << r.posY(2.5) << " " << r.posZ(2.5) << endl;
	return 0;
}
`,

	solution: `#include <iostream>
using namespace std;

class Ray {
public:
	double ox, oy, oz;
	double dx, dy, dz;
	Ray(double ox, double oy, double oz, double dx, double dy, double dz) : ox(ox), oy(oy), oz(oz), dx(dx), dy(dy), dz(dz) {}
	double posX(double t) const { return this->ox + this->dx * t; }
	double posY(double t) const { return this->oy + this->dy * t; }
	double posZ(double t) const { return this->oz + this->dz * t; }
};

int main() {
	Ray r(2, 3, 4, 1, 0, 0);
	cout << r.posX(2.5) << " " << r.posY(2.5) << " " << r.posZ(2.5) << endl;
	return 0;
}
`,

	tests: [
		{
			name: "computes position along a ray",
			expected: "4.5 3 4\n",
		},
	],
};
