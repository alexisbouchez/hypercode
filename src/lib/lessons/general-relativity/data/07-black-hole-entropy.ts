import type { Lesson } from "../../types";

export const blackHoleEntropy: Lesson = {
	id: "black-hole-entropy",
	title: "Black Hole Entropy",
	chapterId: "black-holes",
	content: `## Black Hole Entropy

One of the most profound results in theoretical physics is that black holes carry **entropy** proportional to their surface area — not their volume. This is the Bekenstein–Hawking entropy:

$$S = \\frac{k_B A}{4 \\ell_P^2}$$

where $A$ is the event horizon area and $\\ell_P$ is the **Planck length**:

$$\\ell_P = \\sqrt{\\frac{\\hbar G}{c^3}} \\approx 1.616 \\times 10^{-35} \\text{ m}$$

Substituting $\\ell_P^2 = \\hbar G / c^3$, this becomes:

$$S = \\frac{k_B c^3 A}{4 G \\hbar}$$

### Event Horizon Area

For a Schwarzschild black hole, the event horizon (Schwarzschild radius) is:

$$r_s = \\frac{2GM}{c^2}$$

The surface area of a sphere of radius $r_s$ is:

$$A = 4\\pi r_s^2 = \\frac{16\\pi G^2 M^2}{c^4}$$

### Entropy Formula

Substituting $A$ into the entropy formula:

$$S = \\frac{k_B c^3}{4G\\hbar} \\cdot \\frac{16\\pi G^2 M^2}{c^4} = \\frac{4\\pi G M^2 k_B}{\\hbar c}$$

A solar-mass black hole has entropy $\\approx 10^{54}$ J/K — astronomically larger than a normal star of the same mass ($\\sim 10^{34}$ J/K). This entropy-area relation hints at holography: all information inside may be encoded on the 2D surface.

### Your Task

Implement three functions. All physical constants must be defined **inside** each function body.

- \`planck_length()\` — returns $\\ell_P = \\sqrt{\\hbar G / c^3}$ in meters
- \`event_horizon_area(M)\` — returns $A = 4\\pi (2GM/c^2)^2$ in m²
- \`black_hole_entropy(M)\` — returns $S = 4\\pi G M^2 k_B / (\\hbar c)$ in J/K`,

	starterCode: `import math

def planck_length():
    hbar = 1.0546e-34
    G = 6.674e-11
    c = 299792458.0
    # l_P = sqrt(hbar * G / c^3)
    pass

def event_horizon_area(M):
    G = 6.674e-11
    c = 299792458.0
    # A = 4 * pi * (2*G*M/c^2)^2
    pass

def black_hole_entropy(M):
    hbar = 1.0546e-34
    c = 299792458.0
    G = 6.674e-11
    k_B = 1.381e-23
    # S = 4 * pi * G * M^2 * k_B / (hbar * c)
    pass

print(round(planck_length(), 40))
print(round(event_horizon_area(1.989e30), 4))
`,

	solution: `import math

def planck_length():
    hbar = 1.0546e-34
    G = 6.674e-11
    c = 299792458.0
    return math.sqrt(hbar * G / c**3)

def event_horizon_area(M):
    G = 6.674e-11
    c = 299792458.0
    return 4 * math.pi * (2 * G * M / c**2)**2

def black_hole_entropy(M):
    hbar = 1.0546e-34
    c = 299792458.0
    G = 6.674e-11
    k_B = 1.381e-23
    return 4 * math.pi * G * M**2 * k_B / (hbar * c)

print(round(planck_length(), 40))
print(round(event_horizon_area(1.989e30), 4))
`,

	tests: [
		{
			name: "planck_length() ≈ 1.616e-35 m",
			code: `{{FUNC}}
print(round(planck_length(), 40))`,
			expected: "1.61624e-35\n",
		},
		{
			name: "event_horizon_area(solar mass) ≈ 1.097e8 m²",
			code: `{{FUNC}}
print(round(event_horizon_area(1.989e30), 4))`,
			expected: "109655145.2558\n",
		},
		{
			name: "black_hole_entropy(solar mass) ≈ 1.45e54 J/K",
			code: `{{FUNC}}
print(round(black_hole_entropy(1.989e30), 4))`,
			expected: "1.4492751353382122e+54\n",
		},
		{
			name: "black_hole_entropy(0.5 solar mass) ≈ 3.62e53 J/K (scales as M²)",
			code: `{{FUNC}}
print(round(black_hole_entropy(0.5 * 1.989e30), 4))`,
			expected: "3.6231878383455305e+53\n",
		},
	],
};
