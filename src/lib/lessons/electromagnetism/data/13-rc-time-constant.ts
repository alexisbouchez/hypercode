import type { Lesson } from "../../types";

export const rcTimeConstantLesson: Lesson = {
	id: "rc-time-constant",
	title: "RC Time Constant",
	chapterId: "em-induction",
	content: `## RC Circuits

A resistor and capacitor in series form an **RC circuit**. When charged or discharged, the voltage decays exponentially with time constant:

$$\tau = RC$$

- $\tau$ (tau) — time constant (seconds)
- **R** — resistance (Ω)
- **C** — capacitance (farads, F)

### What τ Means

After one time constant $\tau$, the voltage has decayed to **1/e ≈ 36.8%** of its initial value. After $5\tau$ it is essentially zero (0.7%).

| Time | Voltage remaining |
|------|------------------|
| 0 | 100% |
| $\tau$ | 36.8% |
| $2\tau$ | 13.5% |
| $5\tau$ | 0.7% (fully discharged) |

### Charging and Discharging

$$\text{Discharging: } V(t) = V_0 e^{-t/\tau}$$

$$\text{Charging: } V(t) = V_0 \left(1 - e^{-t/\tau}\right)$$

### Applications

RC time constants set the frequency response of filters and timing circuits — camera flashes, audio tone controls, oscillator timing, and signal debouncing.

### Examples

| R (Ω) | C (F) | τ (s) |
|-------|-------|-------|
| 1000 | 1×10⁻³ | **1.0000** |
| 10000 | 100×10⁻⁶ | **1.0000** |
| 1000 | 1×10⁻⁶ | **0.0010** |
| 470 | 100×10⁻⁶ | **0.0470** |

### Your Task

Implement \`rc_time_constant(R, C)\` returning $\tau$ in seconds.`,

	starterCode: `def rc_time_constant(R, C):
    # tau = R * C
    return 0

print(f"{rc_time_constant(1000, 1e-3):.4f}")      # 1.0000
print(f"{rc_time_constant(10000, 100e-6):.4f}")   # 1.0000
print(f"{rc_time_constant(1000, 1e-6):.4f}")      # 0.0010
print(f"{rc_time_constant(470, 100e-6):.4f}")     # 0.0470
`,

	solution: `def rc_time_constant(R, C):
    return R * C

print(f"{rc_time_constant(1000, 1e-3):.4f}")      # 1.0000
print(f"{rc_time_constant(10000, 100e-6):.4f}")   # 1.0000
print(f"{rc_time_constant(1000, 1e-6):.4f}")      # 0.0010
print(f"{rc_time_constant(470, 100e-6):.4f}")     # 0.0470
`,

	tests: [
		{
			name: "R=1000 Ω, C=1 mF → τ=1.0000 s",
			code: `{{FUNC}}
print(f"{rc_time_constant(1000, 1e-3):.4f}")`,
			expected: "1.0000\n",
		},
		{
			name: "R=10000 Ω, C=100 μF → τ=1.0000 s",
			code: `{{FUNC}}
print(f"{rc_time_constant(10000, 100e-6):.4f}")`,
			expected: "1.0000\n",
		},
		{
			name: "R=1000 Ω, C=1 μF → τ=0.0010 s",
			code: `{{FUNC}}
print(f"{rc_time_constant(1000, 1e-6):.4f}")`,
			expected: "0.0010\n",
		},
		{
			name: "R=470 Ω, C=100 μF → τ=0.0470 s",
			code: `{{FUNC}}
print(f"{rc_time_constant(470, 100e-6):.4f}")`,
			expected: "0.0470\n",
		},
	],
};
