import type { Lesson } from "../../types";

export const newtonsCooling: Lesson = {
	id: "newtons-cooling",
	title: "Newton's Law of Cooling",
	chapterId: "first-order-models",
	content: `## Newton's Law of Cooling

The rate of heat loss of an object is proportional to the difference between the object's temperature and the ambient (surrounding) temperature:

$$\\frac{dT}{dt} = -k \\cdot (T - T_{\\text{env}})$$

- $T$: object temperature
- $T_{\\text{env}}$: ambient temperature (constant)
- $k > 0$: cooling constant (depends on material and environment)

### Exact Solution

$$T(t) = T_{\\text{env}} + (T_0 - T_{\\text{env}}) \\cdot e^{-kt}$$

The object approaches $T_{\\text{env}}$ exponentially. This is just exponential decay applied to the temperature difference $(T - T_{\\text{env}})$.

### Applications

- Forensics: estimating time of death from body temperature
- Engineering: thermal management of electronics
- Food science: how quickly food cools to safe serving temperature
- Metallurgy: quenching and tempering of metals

### Worked Example

A cup of coffee at 90°C in a 20°C room with $k = 0.1$:

$$T(t) = 20 + 70 \\cdot e^{-0.1t}$$
$$T(10) = 20 + 70 \\cdot e^{-1} \\approx 20 + 25.7 = 45.7°C$$
$$T(30) \\approx 20 + 3.5 = 23.5°C$$

### Your Task

Implement \`cooling(k, T_env, T0, t_end, n)\` using Euler's method. Return the final temperature.`,

	starterCode: `def cooling(k, T_env, T0, t_end, n):
    h = t_end / n
    T = float(T0)
    for _ in range(n):
        T = T + h * (-k * (T - T_env))
    return T

# Object at 100°C, ambient 0°C, k=1: T(1) = e^(-1) ≈ 0.3679
print(round(cooling(1, 0, 1.0, 1.0, 100000), 4))

# Object already at ambient temperature stays there
print(round(cooling(1, 20, 20.0, 10, 1000), 5))
`,

	solution: `def cooling(k, T_env, T0, t_end, n):
    h = t_end / n
    T = float(T0)
    for _ in range(n):
        T = T + h * (-k * (T - T_env))
    return T

print(round(cooling(1, 0, 1.0, 1.0, 100000), 4))
print(round(cooling(1, 20, 20.0, 10, 1000), 5))
`,

	tests: [
		{
			name: "at ambient temperature, no change",
			code: `{{FUNC}}
print(round(cooling(1, 20, 20.0, 10, 1000), 5))`,
			expected: "20.0\n",
		},
		{
			name: "hot object cools toward ambient",
			code: `{{FUNC}}
print(round(cooling(1, 0, 100.0, 100, 100000), 1))`,
			expected: "0.0\n",
		},
		{
			name: "cold object warms toward ambient",
			code: `{{FUNC}}
print(cooling(0.5, 100, 20.0, 100, 10000) > 95)`,
			expected: "True\n",
		},
		{
			name: "exact: T(1) = e^(-1) for T_env=0, T0=1, k=1",
			code: `{{FUNC}}
print(round(cooling(1, 0, 1.0, 1.0, 100000), 4))`,
			expected: "0.3679\n",
		},
	],
};
