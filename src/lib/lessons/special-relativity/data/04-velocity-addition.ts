import type { Lesson } from "../../types";

export const velocityAddition: Lesson = {
	id: "velocity-addition",
	title: "Relativistic Velocity Addition",
	chapterId: "kinematics",
	content: `## Relativistic Velocity Addition

In classical mechanics, velocities simply add: if a train moves at $v$ relative to the ground, and you throw a ball at $u'$ relative to the train, the ball moves at $u = u' + v$ relative to the ground. This breaks down at relativistic speeds.

### The Relativistic Formula

Suppose frame $S'$ moves at velocity $v$ relative to frame $S$ (along the same axis). An object moves at velocity $u'$ in $S'$. Its velocity in $S$ is:

$$u = \\frac{u' + v}{1 + \\dfrac{u'v}{c^2}}$$

The denominator $1 + u'v/c^2$ is what prevents the result from exceeding $c$.

### Key Properties

**Low-speed limit**: When $u' \\ll c$ and $v \\ll c$, the denominator $\\approx 1$ and we recover Galilean addition $u \\approx u' + v$.

**Light speed is invariant**: If $u' = c$:

$$u = \\frac{c + v}{1 + v/c} = \\frac{c(1 + v/c)}{1 + v/c} = c$$

No matter how fast the source moves, light still travels at $c$.

**No velocity exceeds $c$**: Combining any two sub-luminal velocities always gives a sub-luminal result.

### Example: 0.5c + 0.5c

Classically: $0.5c + 0.5c = c$. Relativistically:

$$u = \\frac{0.5c + 0.5c}{1 + (0.5)(0.5)} = \\frac{c}{1.25} = 0.8c$$

### Your Task

Implement \`velocity_addition(u_prime, v)\` that returns the velocity $u$ in frame $S$ when an object moves at $u'$ in frame $S'$, and $S'$ moves at $v$ relative to $S$. All velocities in m/s. Use $c = 299792458.0$ m/s inside the function.`,

	starterCode: `def velocity_addition(u_prime, v):
    c = 299792458.0
    # TODO: return (u_prime + v) / (1 + u_prime * v / c**2)
    pass

print(round(velocity_addition(100.0, 200.0), 4))
print(round(velocity_addition(0.5 * 299792458.0, 0.5 * 299792458.0) / 299792458.0, 4))
print(round(velocity_addition(299792458.0, 0.9 * 299792458.0) / 299792458.0, 6))
print(round(velocity_addition(0.6 * 299792458.0, 0.6 * 299792458.0) / 299792458.0, 4))
`,

	solution: `def velocity_addition(u_prime, v):
    c = 299792458.0
    return (u_prime + v) / (1 + u_prime * v / c ** 2)

print(round(velocity_addition(100.0, 200.0), 4))
print(round(velocity_addition(0.5 * 299792458.0, 0.5 * 299792458.0) / 299792458.0, 4))
print(round(velocity_addition(299792458.0, 0.9 * 299792458.0) / 299792458.0, 6))
print(round(velocity_addition(0.6 * 299792458.0, 0.6 * 299792458.0) / 299792458.0, 4))
`,

	tests: [
		{
			name: "100 m/s + 200 m/s ≈ 300.0 m/s (Galilean limit for v << c)",
			code: `{{FUNC}}
print(round(velocity_addition(100.0, 200.0), 4))`,
			expected: "300.0\n",
		},
		{
			name: "0.5c + 0.5c = 0.8c (not c)",
			code: `{{FUNC}}
print(round(velocity_addition(0.5 * 299792458.0, 0.5 * 299792458.0) / 299792458.0, 4))`,
			expected: "0.8\n",
		},
		{
			name: "c + 0.9c = c (light speed is invariant)",
			code: `{{FUNC}}
print(round(velocity_addition(299792458.0, 0.9 * 299792458.0) / 299792458.0, 6))`,
			expected: "1.0\n",
		},
		{
			name: "0.6c + 0.6c ≈ 0.8824c",
			code: `{{FUNC}}
print(round(velocity_addition(0.6 * 299792458.0, 0.6 * 299792458.0) / 299792458.0, 4))`,
			expected: "0.8824\n",
		},
	],
};
