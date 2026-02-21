import type { Lesson } from "../../types";

export const kalmanFilterLesson: Lesson = {
  id: "kalman-filter",
  title: "Kalman Filter (1D)",
  chapterId: "volatility-models",
  content: `## Kalman Filter (1D)

The **Kalman filter** is an optimal recursive estimator for linear systems with Gaussian noise. In 1D, it tracks a hidden state from noisy observations.

### State space model

\`\`\`
State:       x[t] = x[t-1] + noise_q    (process noise variance q)
Observation: z[t] = x[t] + noise_r      (observation noise variance r)
\`\`\`

### One-step update

Given current estimate \`x_est\` with uncertainty \`p_est\`:

**Predict:**
\`\`\`
x_pred = x_est
p_pred = p_est + q
\`\`\`

**Update (using observation z):**
\`\`\`
K = p_pred / (p_pred + r)          (Kalman gain)
x_new = x_pred + K * (z - x_pred)  (updated estimate)
p_new = (1 - K) * p_pred           (updated uncertainty)
\`\`\`

### Task

Implement:
- \`kalman_update(x_est, p_est, z, q, r_noise)\` → returns (x_new, p_new)
- \`kalman_filter(observations, q, r_noise, x0, p0)\` → list of filtered state estimates
`,
  starterCode: `def kalman_update(x_est, p_est, z, q, r_noise):
    # Predict then update
    # Return (x_new, p_new)
    pass

def kalman_filter(observations, q, r_noise, x0, p0):
    # Apply kalman_update for each observation
    # Return list of estimates
    pass
`,
  solution: `def kalman_update(x_est, p_est, z, q, r_noise):
    x_pred = x_est
    p_pred = p_est + q
    K = p_pred / (p_pred + r_noise)
    x_new = x_pred + K * (z - x_pred)
    p_new = (1 - K) * p_pred
    return x_new, p_new

def kalman_filter(observations, q, r_noise, x0, p0):
    x_est, p_est = x0, p0
    estimates = []
    for z in observations:
        x_est, p_est = kalman_update(x_est, p_est, z, q, r_noise)
        estimates.append(x_est)
    return estimates
`,
  tests: [
    {
      name: "kalman_update single step x_est=0",
      code: `{{FUNC}}\nx_new, p_new = kalman_update(0.0, 1.0, 1.0, 0.1, 0.5)\nprint(round(x_new, 4), round(p_new, 4))`,
      expected: "0.6875 0.3438\n",
    },
    {
      name: "kalman_update single step x_est=5",
      code: `{{FUNC}}\nx_new, p_new = kalman_update(5.0, 2.0, 6.0, 0.2, 1.0)\nprint(round(x_new, 4), round(p_new, 4))`,
      expected: "5.6875 0.6875\n",
    },
    {
      name: "kalman_filter on [1,2,3,4,5]",
      code: `{{FUNC}}\nprint([round(v, 4) for v in kalman_filter([1.0, 2.0, 3.0, 4.0, 5.0], 0.1, 0.5, 0.0, 1.0)])`,
      expected: "[0.6875, 1.3046, 1.9849, 2.7416, 3.5665]\n",
    },
    {
      name: "kalman_filter on noisy observations",
      code: `{{FUNC}}\nprint([round(v, 4) for v in kalman_filter([10.0, 11.0, 9.0, 12.0], 0.5, 2.0, 10.0, 1.0)])`,
      expected: "[10.0, 10.4043, 9.8489, 10.6927]\n",
    },
  ],
};
