import type { Lesson } from "../../types";

export const fourVectors: Lesson = {
	id: "four-vectors",
	title: "Four-Vectors",
	chapterId: "dynamics",
	content: `## Four-Vectors

In special relativity, space and time mix under Lorentz transformations. The natural language for this is **4-vectors** — objects with one time component and three space components that transform covariantly.

### Common 4-Vectors

| Name | Components |
|------|-----------|
| 4-position | $(ct,\\ x,\\ y,\\ z)$ |
| 4-velocity | $\\gamma(c,\\ v_x,\\ v_y,\\ v_z)$ |
| 4-momentum | $(E/c,\\ p_x,\\ p_y,\\ p_z)$ |

### Minkowski Norm

Using the $+{-}{-}{-}$ signature, the **Minkowski norm squared** is:

$$|A|^2 = A_t^2 - A_x^2 - A_y^2 - A_z^2$$

This quantity is **Lorentz-invariant** — all observers agree on it.

### Invariant Mass

For the 4-momentum, the norm gives the invariant mass:

$$|p|^2 = (E/c)^2 - |\\vec{p}|^2 = (mc)^2$$

A **lightlike** vector (photon) has $|A|^2 = 0$. A **timelike** vector has $|A|^2 > 0$.

### Your Task

Implement:
- \`minkowski_norm_sq(at, ax, ay, az)\` — computes $A_t^2 - A_x^2 - A_y^2 - A_z^2$
- \`invariant_mass(E, px, py, pz)\` — returns mass in kg given $E$ in J and momentum in kg⋅m/s

Use $c = 299792458.0$ m/s defined **inside** each function that needs it.`,

	starterCode: `import math

def minkowski_norm_sq(at, ax, ay, az):
    # at^2 - ax^2 - ay^2 - az^2
    pass

def invariant_mass(E, px, py, pz):
    c = 299792458.0
    # sqrt(max(0, E^2 - |p|^2 * c^2)) / c^2
    pass

print(minkowski_norm_sq(1.0, 1.0, 0.0, 0.0))
print(minkowski_norm_sq(2.0, 1.0, 0.0, 0.0))
print(round(invariant_mass(299792458.0 ** 2, 0.0, 0.0, 0.0), 4))
print(round(invariant_mass(1.25 * 299792458.0 ** 2, 0.75 * 299792458.0, 0.0, 0.0), 4))
`,

	solution: `import math

def minkowski_norm_sq(at, ax, ay, az):
    return at ** 2 - ax ** 2 - ay ** 2 - az ** 2

def invariant_mass(E, px, py, pz):
    c = 299792458.0
    p_sq = px ** 2 + py ** 2 + pz ** 2
    return math.sqrt(max(0, E ** 2 - p_sq * c ** 2)) / c ** 2

print(minkowski_norm_sq(1.0, 1.0, 0.0, 0.0))
print(minkowski_norm_sq(2.0, 1.0, 0.0, 0.0))
print(round(invariant_mass(299792458.0 ** 2, 0.0, 0.0, 0.0), 4))
print(round(invariant_mass(1.25 * 299792458.0 ** 2, 0.75 * 299792458.0, 0.0, 0.0), 4))
`,

	tests: [
		{
			name: "Lightlike 4-vector (photon): norm² = 0.0",
			code: `{{FUNC}}
print(minkowski_norm_sq(1.0, 1.0, 0.0, 0.0))`,
			expected: "0.0\n",
		},
		{
			name: "Timelike 4-vector: norm² = 4 − 1 = 3.0",
			code: `{{FUNC}}
print(minkowski_norm_sq(2.0, 1.0, 0.0, 0.0))`,
			expected: "3.0\n",
		},
		{
			name: "Invariant mass at rest (E=mc², p=0): m = 1.0 kg",
			code: `{{FUNC}}
print(round(invariant_mass(299792458.0 ** 2, 0.0, 0.0, 0.0), 4))`,
			expected: "1.0\n",
		},
		{
			name: "Invariant mass at v=0.6c (E=1.25c², px=0.75c): m = 1.0 kg",
			code: `{{FUNC}}
print(round(invariant_mass(1.25 * 299792458.0 ** 2, 0.75 * 299792458.0, 0.0, 0.0), 4))`,
			expected: "1.0\n",
		},
	],
};
