import type { Lesson } from "../../types";

export const phong: Lesson = {
	id: "phong",
	title: "Phong Lighting",
	chapterId: "lighting",
	content: `## Phong Lighting

The **Phong reflection model** breaks lighting into three components:

1. **Ambient** — constant background illumination (prevents pure black shadows)
2. **Diffuse** — scattered light based on surface orientation toward light
3. **Specular** — mirror-like highlight based on the reflection direction

### The Formula

\`\`\`
color = ambient + diffuse + specular
\`\`\`

Given:
- **eyev**: unit vector from surface toward camera
- **normalv**: unit normal vector of the surface
- **lightv**: unit vector from surface toward light

\`\`\`
ambient = material.ambient

lightDotNormal = dot(lightv, normalv)
if (lightDotNormal < 0):
    diffuse = 0  // light is behind the surface
else:
    diffuse = material.diffuse * lightDotNormal

reflectv = reflect(-lightv, normalv)  // mirror direction
reflectDotEye = dot(reflectv, eyev)
if (reflectDotEye <= 0):
    specular = 0
else:
    factor = pow(reflectDotEye, material.shininess)
    specular = material.specular * factor
\`\`\`

### Reflect Formula

\`\`\`
reflect(in, normal) = in - normal * 2 * dot(in, normal)
\`\`\`

### Test Cases from the Book

Using: color=(1,1,1), ambient=0.1, diffuse=0.9, specular=0.9, shininess=200

| Scene | Result |
|-------|--------|
| Eye directly facing light, light directly above | **1.9** |
| Light behind surface | **0.1** |

### Your Task

Implement \`phong(eyex, eyey, eyez, nx, ny, nz, lx, ly, lz)\` that returns the combined lighting intensity.

Expected output:
\`\`\`
1.9
0.1
\`\`\``,

	starterCode: `#include <iostream>
#include <cmath>
using namespace std;

double dot3(double ax, double ay, double az,
            double bx, double by, double bz) {
	return ax*bx + ay*by + az*bz;
}

// Implement phong(eyex, eyey, eyez, nx, ny, nz, lx, ly, lz)
// Returns ambient + diffuse + specular
// Material: ambient=0.1, diffuse=0.9, specular=0.9, shininess=200
// Steps:
//   1. Normalize light vector: lvx, lvy, lvz
//   2. ldn = dot(lightv, normalv)
//   3. if ldn < 0, return 0.1 (ambient only)
//   4. diffuse = 0.9 * ldn
//   5. inx, iny, inz = -lvx, -lvy, -lvz
//   6. idn = dot(inv, normalv)
//   7. reflx = inx - nx*2*idn, etc.
//   8. rde = dot(reflv, eyev)
//   9. if rde > 0: specular = 0.9 * pow(rde, 200)
//  10. return 0.1 + diffuse + specular

int main() {
	// Eye facing light, light in front of surface
	double r1 = phong(0, 0, -1, 0, 0, -1, 0, 0, -10);
	cout << r1 << endl;

	// Light behind surface
	double r2 = phong(0, 0, -1, 0, 0, -1, 0, 0, 10);
	cout << r2 << endl;
	return 0;
}
`,

	solution: `#include <iostream>
#include <cmath>
using namespace std;

double dot3(double ax, double ay, double az,
            double bx, double by, double bz) {
	return ax*bx + ay*by + az*bz;
}

double phong(double ex, double ey, double ez,
             double nx, double ny, double nz,
             double lx, double ly, double lz) {
	double ambient = 0.1;

	double lightLen = sqrt(lx*lx + ly*ly + lz*lz);
	double lvx = lx / lightLen;
	double lvy = ly / lightLen;
	double lvz = lz / lightLen;

	double ldn = dot3(lvx, lvy, lvz, nx, ny, nz);
	if (ldn < 0) return ambient;

	double diffuse = 0.9 * ldn;

	double inx = -lvx, iny = -lvy, inz = -lvz;
	double idn = dot3(inx, iny, inz, nx, ny, nz);
	double reflx = inx - nx * 2.0 * idn;
	double refly = iny - ny * 2.0 * idn;
	double reflz = inz - nz * 2.0 * idn;

	double rde = dot3(reflx, refly, reflz, ex, ey, ez);
	double specular = 0;
	if (rde > 0) {
		double factor = pow(rde, 200.0);
		specular = 0.9 * factor;
	}

	return ambient + diffuse + specular;
}

int main() {
	double r1 = phong(0, 0, -1, 0, 0, -1, 0, 0, -10);
	cout << r1 << endl;

	double r2 = phong(0, 0, -1, 0, 0, -1, 0, 0, 10);
	cout << r2 << endl;
	return 0;
}
`,

	tests: [
		{
			name: "computes Phong lighting",
			expected: "1.9\n0.1\n",
		},
	],
};
