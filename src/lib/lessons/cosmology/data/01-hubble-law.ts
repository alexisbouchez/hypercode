import type { Lesson } from "../../types";

export const hubbleLaw: Lesson = {
	id: "hubble-law",
	title: "Hubble's Law",
	chapterId: "expansion",
	content: `## Hubble's Law

In 1929, Edwin Hubble discovered that galaxies are receding from us at velocities proportional to their distance — the observational cornerstone of the expanding universe.

### The Law

$$v = H_0 \\cdot d$$

where $H_0 \\approx 70$ km/s/Mpc is the **Hubble constant** today, $d$ is the distance in megaparsecs (Mpc), and $v$ is the recession velocity in km/s.

One parsec is 3.0857 × 10¹⁶ m (about 3.26 light-years). A megaparsec is 10⁶ pc.

### The Hubble Time

A rough estimate of the age of the universe is the **Hubble time** — the time it would take for a galaxy to reach its current distance if it had always moved at today's velocity:

$$t_H = \\frac{1}{H_0}$$

To convert $H_0$ from km/s/Mpc to SI units (s⁻¹):

$$H_0^{\\text{SI}} = H_0 \\times \\frac{1000}{3.0857 \\times 10^{22}}$$

Then $t_H = 1/H_0^{\\text{SI}}$ in seconds. Dividing by $3.156 \\times 10^{16}$ s/Gyr gives gigayears.

### The Hubble Distance

The **Hubble distance** $D_H = c / H_0$ sets the characteristic length scale of the observable universe:

$$D_H = \\frac{c}{H_0} \\approx 4283 \\text{ Mpc}$$

using $c = 299792.458$ km/s and $H_0 = 70$ km/s/Mpc.

| Quantity | Formula | Value ($H_0 = 70$) |
|----------|---------|---------------------|
| Recession velocity | $v = H_0 d$ | 7000 km/s at 100 Mpc |
| Hubble time | $t_H = 1/H_0$ | ≈ 13.97 Gyr |
| Hubble distance | $D_H = c/H_0$ | ≈ 4282.75 Mpc |

### Your Task

Implement three functions. All constants must be defined **inside** each function body.

- \`hubble_velocity(H0_km_s_Mpc, d_Mpc)\` — returns recession velocity in km/s
- \`hubble_time_Gyr(H0_km_s_Mpc)\` — returns Hubble time in Gyr
- \`hubble_distance_Mpc(H0_km_s_Mpc)\` — returns Hubble distance in Mpc using $c = 299792.458$ km/s`,

	starterCode: `def hubble_velocity(H0_km_s_Mpc, d_Mpc):
    # v = H0 * d
    pass

def hubble_time_Gyr(H0_km_s_Mpc):
    Mpc_m = 3.0857e22      # metres per Mpc
    s_per_Gyr = 3.156e16   # seconds per Gyr
    # Convert H0 to SI: H0_SI = H0 * 1000 / Mpc_m
    # t_H = 1 / H0_SI  (seconds)
    # return t_H / s_per_Gyr
    pass

def hubble_distance_Mpc(H0_km_s_Mpc):
    c = 299792.458  # km/s
    # return c / H0
    pass

print(hubble_velocity(70, 100))
print(round(hubble_time_Gyr(70), 2))
print(round(hubble_distance_Mpc(70), 2))
`,

	solution: `def hubble_velocity(H0_km_s_Mpc, d_Mpc):
    return H0_km_s_Mpc * d_Mpc

def hubble_time_Gyr(H0_km_s_Mpc):
    Mpc_m = 3.0857e22
    s_per_Gyr = 3.156e16
    H0_SI = H0_km_s_Mpc * 1000 / Mpc_m
    t_H = 1 / H0_SI
    return t_H / s_per_Gyr

def hubble_distance_Mpc(H0_km_s_Mpc):
    c = 299792.458  # km/s
    return c / H0_km_s_Mpc

print(hubble_velocity(70, 100))
print(round(hubble_time_Gyr(70), 2))
print(round(hubble_distance_Mpc(70), 2))
`,

	tests: [
		{
			name: "hubble_velocity(70, 100) = 7000 km/s",
			code: `{{FUNC}}
print(hubble_velocity(70, 100))`,
			expected: "7000\n",
		},
		{
			name: "hubble_time_Gyr(70) ≈ 13.97 Gyr",
			code: `{{FUNC}}
print(round(hubble_time_Gyr(70), 2))`,
			expected: "13.97\n",
		},
		{
			name: "hubble_distance_Mpc(70) ≈ 4282.75 Mpc",
			code: `{{FUNC}}
print(round(hubble_distance_Mpc(70), 2))`,
			expected: "4282.75\n",
		},
		{
			name: "hubble_velocity(67.4, 1) = 67.4 km/s (Planck H0)",
			code: `{{FUNC}}
print(hubble_velocity(67.4, 1))`,
			expected: "67.4\n",
		},
	],
};
