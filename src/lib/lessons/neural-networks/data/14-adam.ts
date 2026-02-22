import type { Lesson } from "../../types";

export const adam: Lesson = {
	id: "adam",
	title: "Adam Optimizer",
	chapterId: "advanced",
	content: `## Adaptive Moment Estimation

Plain gradient descent uses the same learning rate for all parameters. **Adam** adapts the learning rate per-parameter by tracking two running statistics:

- **$m_t$** — the first moment (exponential moving average of gradients)
- **$v_t$** — the second moment (exponential moving average of squared gradients)

### Update Rules

At step $t$, given gradient $g_t$:

$$m_t = \\beta_1 m_{t-1} + (1 - \\beta_1) g_t$$

$$v_t = \\beta_2 v_{t-1} + (1 - \\beta_2) g_t^2$$

**Bias correction** (prevents large updates at $t=1$ when $m$ and $v$ start at 0):

$$\\hat{m}_t = \\frac{m_t}{1 - \\beta_1^t} \\qquad \\hat{v}_t = \\frac{v_t}{1 - \\beta_2^t}$$

**Parameter update**:

$$\\theta_t = \\theta_{t-1} - \\eta \\cdot \\frac{\\hat{m}_t}{\\sqrt{\\hat{v}_t} + \\varepsilon}$$

### Default Hyperparameters

The paper suggests: $\\beta_1 = 0.9$, $\\beta_2 = 0.999$, $\\varepsilon = 10^{-8}$, $\\eta = 0.001$.

- $\\beta_1 = 0.9$: gradient momentum decays with a 10-step memory
- $\\beta_2 = 0.999$: squared gradient has ~1000-step memory
- $\\varepsilon$: prevents division by zero

Adam is the default optimizer for most deep learning — it works well across a wide range of architectures and learning rates.

### Your Task

Implement \`adam_update(param, grad, m, v, t, lr, beta1, beta2, eps)\` returning \`(new_param, new_m, new_v)\`.`,

	starterCode: `def adam_update(param, grad, m, v, t, lr=0.001, beta1=0.9, beta2=0.999, eps=1e-8):
    # Update biased moments
    m = beta1 * m + (1 - beta1) * grad
    v = beta2 * v + (1 - beta2) * grad ** 2
    # Bias-corrected estimates
    m_hat = m / (1 - beta1 ** t)
    v_hat = v / (1 - beta2 ** t)
    # Update parameter
    param = param  # TODO: apply the update
    return param, m, v

param, m, v = 1.0, 0.0, 0.0
param, m, v = adam_update(param, 0.5, m, v, t=1)
print(round(param, 6))  # 0.999
print(round(m, 6))      # 0.05
print(round(v, 6))      # 0.00025
`,

	solution: `def adam_update(param, grad, m, v, t, lr=0.001, beta1=0.9, beta2=0.999, eps=1e-8):
    m = beta1 * m + (1 - beta1) * grad
    v = beta2 * v + (1 - beta2) * grad ** 2
    m_hat = m / (1 - beta1 ** t)
    v_hat = v / (1 - beta2 ** t)
    param = param - lr * m_hat / (v_hat ** 0.5 + eps)
    return param, m, v

param, m, v = 1.0, 0.0, 0.0
param, m, v = adam_update(param, 0.5, m, v, t=1)
print(round(param, 6))
print(round(m, 6))
print(round(v, 6))
`,

	tests: [
		{
			name: "first Adam step: param, first moment, second moment",
			expected: "0.999\n0.05\n0.00025\n",
		},
		{
			name: "second Adam step continues descent",
			code: `{{FUNC}}
param, m, v = 1.0, 0.0, 0.0
param, m, v = adam_update(param, 0.5, m, v, t=1)
param, m, v = adam_update(param, 0.5, m, v, t=2)
print(round(param, 6))`,
			expected: "0.998\n",
		},
		{
			name: "zero gradient leaves param unchanged",
			code: `{{FUNC}}
param, m, v = 5.0, 0.0, 0.0
param2, m2, v2 = adam_update(param, 0.0, m, v, t=1)
print(param2 == param)`,
			expected: "True\n",
		},
	],
};
