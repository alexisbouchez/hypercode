import type { Lesson } from "../../types";

export const photonSphere: Lesson = {
	id: "photon-sphere",
	title: "Photon Sphere and ISCO",
	chapterId: "schwarzschild",
	content: `## Photon Sphere and ISCO

The Schwarzschild geometry supports several special orbital radii beyond the event horizon. Two of the most important are the **photon sphere** and the **innermost stable circular orbit (ISCO)**.

### Photon Sphere

At $r_{\\text{ph}} = \\tfrac{3}{2} r_s = 3GM/c^2$, photons can orbit the black hole in unstable circular orbits. This radius is called the **photon sphere**. Although these orbits are unstable (a tiny perturbation sends the photon spiralling in or escaping), the photon sphere determines the apparent size of a black hole's shadow.

$$r_{\\text{ph}} = \\frac{3GM}{c^2} = \\frac{3}{2} r_s$$

### Black Hole Shadow

An observer far away sees a dark disk (the shadow) whose apparent radius is larger than the photon sphere due to gravitational lensing:

$$r_{\\text{shadow}} = 3\\sqrt{3}\\,\\frac{GM}{c^2}$$

This formula was used to predict the apparent size of M87* before the Event Horizon Telescope imaged it in 2019.

### ISCO — Innermost Stable Circular Orbit

For massive particles, circular orbits are possible for $r > r_s$, but stable orbits only exist for:

$$r_{\\text{ISCO}} = \\frac{6GM}{c^2} = 3 r_s$$

Inside the ISCO, any perturbation causes matter to spiral inward. This radius sets the inner edge of accretion disks around black holes.

| Orbit | Radius |
|-------|--------|
| Event horizon | $r_s = 2GM/c^2$ |
| Photon sphere | $r_{\\text{ph}} = 3GM/c^2$ |
| ISCO | $r_{\\text{ISCO}} = 6GM/c^2$ |

### Your Task

Implement these functions with all constants defined **inside** each function:

- \`photon_sphere_radius(M)\` — returns $r_{\\text{ph}} = 3GM/c^2$
- \`isco_radius(M)\` — returns $r_{\\text{ISCO}} = 6GM/c^2$
- \`shadow_radius(M)\` — returns $r_{\\text{shadow}} = 3\\sqrt{3}\\,GM/c^2$`,

	starterCode: `import math

def photon_sphere_radius(M):
    G = 6.674e-11
    c = 299792458.0
    # TODO: return 3 * G * M / c**2
    pass

def isco_radius(M):
    G = 6.674e-11
    c = 299792458.0
    # TODO: return 6 * G * M / c**2
    pass

def shadow_radius(M):
    G = 6.674e-11
    c = 299792458.0
    # TODO: return 3 * math.sqrt(3) * G * M / c**2
    pass

M_sun = 1.989e30
print(round(photon_sphere_radius(M_sun), 2))
print(round(isco_radius(M_sun), 2))
print(round(shadow_radius(M_sun), 2))
M_M87 = 6.5e9 * 1.989e30
print(round(shadow_radius(M_M87), 0))
`,

	solution: `import math

def photon_sphere_radius(M):
    G = 6.674e-11
    c = 299792458.0
    return 3 * G * M / c**2

def isco_radius(M):
    G = 6.674e-11
    c = 299792458.0
    return 6 * G * M / c**2

def shadow_radius(M):
    G = 6.674e-11
    c = 299792458.0
    return 3 * math.sqrt(3) * G * M / c**2

M_sun = 1.989e30
print(round(photon_sphere_radius(M_sun), 2))
print(round(isco_radius(M_sun), 2))
print(round(shadow_radius(M_sun), 2))
M_M87 = 6.5e9 * 1.989e30
print(round(shadow_radius(M_M87), 0))
`,

	tests: [
		{
			name: "photon_sphere_radius of Sun ≈ 4430.99 m",
			code: `{{FUNC}}
print(round(photon_sphere_radius(1.989e30), 2))`,
			expected: "4430.99\n",
		},
		{
			name: "isco_radius of Sun ≈ 8861.98 m",
			code: `{{FUNC}}
print(round(isco_radius(1.989e30), 2))`,
			expected: "8861.98\n",
		},
		{
			name: "shadow_radius of Sun ≈ 7674.7 m",
			code: `{{FUNC}}
print(round(shadow_radius(1.989e30), 2))`,
			expected: "7674.7\n",
		},
		{
			name: "shadow_radius of M87* (6.5e9 solar masses) ≈ 4.9885556148703e+13 m",
			code: `{{FUNC}}
print(round(shadow_radius(6.5e9 * 1.989e30), 0))`,
			expected: "49885556148703.0\n",
		},
	],
};
