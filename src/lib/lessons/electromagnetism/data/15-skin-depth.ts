import type { Lesson } from "../../types";

export const skinDepthLesson: Lesson = {
	id: "skin-depth",
	title: "Skin Depth",
	chapterId: "em-induction",
	content: `## The Skin Effect

At high frequencies, alternating current doesn't flow uniformly through a conductor — it concentrates near the surface. The **skin depth** $\delta$ is the depth at which the current density falls to $1/e \approx 37\%$ of its surface value:

$$\delta = \sqrt{\frac{2\rho}{\omega\mu}} = \sqrt{\frac{\rho}{\pi f \mu}}$$

- $\delta$ — skin depth (m)
- $\rho$ — resistivity of the conductor (Ω·m)
- **f** — frequency (Hz)
- $\mu = \mu_0 \mu_r$ — magnetic permeability (H/m)

For non-magnetic materials $\mu \approx \mu_0 = 4\pi \times 10^{-7}$ H/m.

### Practical Consequences

| Material | f | δ |
|----------|---|---|
| Copper ($\rho = 1.68\times10^{-8}$) at 50 Hz | ~9 mm |
| Copper at 1 MHz | ~66 μm |
| Copper at 1 GHz | ~2 μm |

At RF frequencies, only a thin surface layer carries current. High-frequency cables are plated with silver (lower resistivity) to reduce loss.

### Examples (copper, $\rho = 1.68\times10^{-8}$ Ω·m, $\mu = \mu_0$)

| f (Hz) | δ (m) |
|--------|-------|
| 50 | **9.33e-03** |
| 1000 | **2.09e-03** |
| 1×10⁶ | **6.61e-05** |
| 1×10⁹ | **2.09e-06** |

### Your Task

Implement \`skin_depth(rho, f, mu)\` returning $\delta$ in metres.`,

	starterCode: `import math

MU0 = 4 * math.pi * 1e-7

def skin_depth(rho, f, mu):
    # delta = sqrt(rho / (pi * f * mu))
    return 0

rho_cu = 1.72e-8   # copper resistivity

print(f"{skin_depth(rho_cu, 50, MU0):.2e}")      # 9.33e-03
print(f"{skin_depth(rho_cu, 1000, MU0):.2e}")    # 2.09e-03
print(f"{skin_depth(rho_cu, 1e6, MU0):.2e}")     # 6.60e-05
print(f"{skin_depth(rho_cu, 1e9, MU0):.2e}")     # 2.09e-06
`,

	solution: `import math

MU0 = 4 * math.pi * 1e-7

def skin_depth(rho, f, mu):
    return math.sqrt(rho / (math.pi * f * mu))

rho_cu = 1.72e-8   # copper resistivity

print(f"{skin_depth(rho_cu, 50, MU0):.2e}")      # 9.33e-03
print(f"{skin_depth(rho_cu, 1000, MU0):.2e}")    # 2.09e-03
print(f"{skin_depth(rho_cu, 1e6, MU0):.2e}")     # 6.60e-05
print(f"{skin_depth(rho_cu, 1e9, MU0):.2e}")     # 2.09e-06
`,

	tests: [
		{
			name: "copper at 50 Hz → δ ≈ 9.33e-03 m",
			code: `{{FUNC}}
import math
MU0 = 4 * math.pi * 1e-7
print(f"{skin_depth(1.68e-8, 50, MU0):.2e}")`,
			expected: "9.33e-03\n",
		},
		{
			name: "copper at 1000 Hz → δ ≈ 2.09e-03 m",
			code: `{{FUNC}}
import math
MU0 = 4 * math.pi * 1e-7
print(f"{skin_depth(1.68e-8, 1000, MU0):.2e}")`,
			expected: "2.09e-03\n",
		},
		{
			name: "copper at 1 MHz → δ ≈ 6.61e-05 m",
			code: `{{FUNC}}
import math
MU0 = 4 * math.pi * 1e-7
print(f"{skin_depth(1.68e-8, 1e6, MU0):.2e}")`,
			expected: "6.60e-05\n",
		},
		{
			name: "copper at 1 GHz → δ ≈ 2.09e-06 m",
			code: `{{FUNC}}
import math
MU0 = 4 * math.pi * 1e-7
print(f"{skin_depth(1.68e-8, 1e9, MU0):.2e}")`,
			expected: "2.09e-06\n",
		},
	],
};
