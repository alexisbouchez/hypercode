import type { Lesson } from "../../types";

export const solenoidInductanceLesson: Lesson = {
	id: "solenoid-inductance",
	title: "Solenoid Inductance",
	chapterId: "magnetism",
	content: `## Inductance of a Solenoid

A **solenoid** is a coil of N turns wound over length l. When current flows, it creates a nearly uniform magnetic field inside. Its inductance is:

$$L = \frac{\mu_0 N^2 A}{l}$$

- **L** — inductance (henrys, H)
- $\mu_0 = 4\pi \times 10^{-7}$ H/m — permeability of free space
- **N** — total number of turns
- **A** — cross-sectional area (m²)
- **l** — length of the solenoid (m)

### Why N²?

Each turn contributes to the field, and the total flux linkage is $N \times$ (flux per turn). Since flux per turn is also proportional to N (more turns = stronger field), inductance grows as $N^2$.

### Applications

Solenoids are the basis of inductors in filters and oscillators, electromagnets, transformers, and electric motors. Inductance opposes changes in current — a solenoid stores energy in its magnetic field.

### Examples

| N | A (m²) | l (m) | L (H) |
|---|--------|-------|-------|
| 1000 | 1×10⁻⁴ | 0.1 | **1.26e-03** |
| 100 | 1×10⁻⁴ | 0.1 | **1.26e-05** |
| 500 | 2×10⁻⁴ | 0.2 | **3.14e-04** |
| 200 | 5×10⁻⁴ | 0.5 | **5.03e-05** |

### Your Task

Implement \`solenoid_inductance(N, A, l)\` returning inductance in henrys.`,

	starterCode: `import math

MU0 = 4 * math.pi * 1e-7

def solenoid_inductance(N, A, l):
    # L = MU0 * N^2 * A / l
    return 0

print(f"{solenoid_inductance(1000, 1e-4, 0.1):.2e}")   # 1.26e-03
print(f"{solenoid_inductance(100, 1e-4, 0.1):.2e}")    # 1.26e-05
print(f"{solenoid_inductance(500, 2e-4, 0.2):.2e}")    # 3.14e-04
print(f"{solenoid_inductance(200, 5e-4, 0.5):.2e}")    # 5.03e-05
`,

	solution: `import math

MU0 = 4 * math.pi * 1e-7

def solenoid_inductance(N, A, l):
    return MU0 * N * N * A / l

print(f"{solenoid_inductance(1000, 1e-4, 0.1):.2e}")   # 1.26e-03
print(f"{solenoid_inductance(100, 1e-4, 0.1):.2e}")    # 1.26e-05
print(f"{solenoid_inductance(500, 2e-4, 0.2):.2e}")    # 3.14e-04
print(f"{solenoid_inductance(200, 5e-4, 0.5):.2e}")    # 5.03e-05
`,

	tests: [
		{
			name: "N=1000, A=1e-4, l=0.1 → 1.26e-03 H",
			code: `{{FUNC}}
print(f"{solenoid_inductance(1000, 1e-4, 0.1):.2e}")`,
			expected: "1.26e-03\n",
		},
		{
			name: "N=100, A=1e-4, l=0.1 → 1.26e-05 H",
			code: `{{FUNC}}
print(f"{solenoid_inductance(100, 1e-4, 0.1):.2e}")`,
			expected: "1.26e-05\n",
		},
		{
			name: "N=500, A=2e-4, l=0.2 → 3.14e-04 H",
			code: `{{FUNC}}
print(f"{solenoid_inductance(500, 2e-4, 0.2):.2e}")`,
			expected: "3.14e-04\n",
		},
		{
			name: "N=200, A=5e-4, l=0.5 → 5.03e-05 H",
			code: `{{FUNC}}
print(f"{solenoid_inductance(200, 5e-4, 0.5):.2e}")`,
			expected: "5.03e-05\n",
		},
	],
};
