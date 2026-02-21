import type { Lesson } from "../../types";

export const shannonEntropy: Lesson = {
  id: "shannon-entropy",
  title: "Shannon Entropy",
  chapterId: "information",
  content: `# Shannon Entropy

**Information theory**, founded by Claude Shannon in 1948, gives us a rigorous mathematical framework for quantifying uncertainty, randomness, and the capacity to communicate.

## Shannon Entropy

Given a discrete probability distribution $\\{p_i\\}$, the **Shannon entropy** is:

$$H(X) = -\\sum_i p_i \\log_2 p_i \\quad \\text{(bits)}$$

- A fair coin ($p = 0.5, 0.5$) has $H = 1$ bit
- A fair die ($p_i = 1/6$ for $i = 1..6$) has $H = \\log_2 6 \\approx 2.585$ bits
- A certain outcome ($p = 1$) has $H = 0$ bits
- The maximum entropy for $N$ outcomes is $\\log_2 N$ (achieved by the uniform distribution)

By convention, $0 \\log_2 0 = 0$ (zero-probability outcomes are skipped).

## Joint Entropy and Mutual Information

For a joint distribution $p(x, y)$, the **joint entropy** is:

$$H(X, Y) = -\\sum_{i,j} p(x_i, y_j) \\log_2 p(x_i, y_j)$$

The **marginal** distributions are obtained by summing over the other variable:

$$p(x_i) = \\sum_j p(x_i, y_j), \\qquad p(y_j) = \\sum_i p(x_i, y_j)$$

**Mutual information** measures how much knowing $X$ reduces uncertainty about $Y$:

$$I(X; Y) = H(X) + H(Y) - H(X, Y)$$

If $X$ and $Y$ are independent, $H(X, Y) = H(X) + H(Y)$ and therefore $I(X; Y) = 0$.

## KL Divergence

The **Kullback-Leibler divergence** (relative entropy) measures how different distribution $Q$ is from a reference $P$:

$$D_{\\mathrm{KL}}(P \\| Q) = \\sum_i p_i \\log_2 \\frac{p_i}{q_i}$$

Properties:
- $D_{\\mathrm{KL}}(P \\| Q) \\geq 0$ always (equality iff $P = Q$)
- Not symmetric: $D_{\\mathrm{KL}}(P \\| Q) \\neq D_{\\mathrm{KL}}(Q \\| P)$ in general

## Your Task

Implement four functions:
- \`shannon_entropy(probs)\` — compute $H$ in bits; skip zero entries
- \`joint_entropy(joint_probs)\` — compute $H(X,Y)$ from a 2D list of joint probabilities
- \`mutual_information(joint_probs)\` — compute $I(X;Y)$ from a 2D list of joint probabilities
- \`kl_divergence(p, q)\` — compute $D_{\\mathrm{KL}}(P \\| Q)$ in bits
`,
  starterCode: `import math

def shannon_entropy(probs):
    pass

def joint_entropy(joint_probs):
    pass

def mutual_information(joint_probs):
    pass

def kl_divergence(p, q):
    pass
`,
  solution: `import math

def shannon_entropy(probs):
    h = 0.0
    for p in probs:
        if p > 0:
            h -= p * math.log2(p)
    return h

def joint_entropy(joint_probs):
    h = 0.0
    for row in joint_probs:
        for p in row:
            if p > 0:
                h -= p * math.log2(p)
    return h

def mutual_information(joint_probs):
    rows = len(joint_probs)
    cols = len(joint_probs[0])
    p_x = [sum(joint_probs[i][j] for j in range(cols)) for i in range(rows)]
    p_y = [sum(joint_probs[i][j] for i in range(rows)) for j in range(cols)]
    h_x = shannon_entropy(p_x)
    h_y = shannon_entropy(p_y)
    h_xy = joint_entropy(joint_probs)
    return h_x + h_y - h_xy

def kl_divergence(p, q):
    result = 0.0
    for pi, qi in zip(p, q):
        if pi > 0:
            result += pi * math.log2(pi / qi)
    return result
`,
  tests: [
    {
      name: "shannon_entropy([0.5, 0.5]) equals 1.0 bit",
      expected: "1.0000\n",
      code: `{{FUNC}}
print(f"{shannon_entropy([0.5, 0.5]):.4f}")`,
    },
    {
      name: "shannon_entropy([0.25, 0.25, 0.25, 0.25]) equals 2.0 bits",
      expected: "2.0000\n",
      code: `{{FUNC}}
print(f"{shannon_entropy([0.25, 0.25, 0.25, 0.25]):.4f}")`,
    },
    {
      name: "kl_divergence([0.5, 0.5], [0.25, 0.75])",
      expected: "0.2075\n",
      code: `{{FUNC}}
print(f"{kl_divergence([0.5, 0.5], [0.25, 0.75]):.4f}")`,
    },
    {
      name: "mutual_information([[0.25,0.25],[0.25,0.25]]) equals 0 (independent)",
      expected: "0.0000\n",
      code: `{{FUNC}}
print(f"{mutual_information([[0.25, 0.25], [0.25, 0.25]]):.4f}")`,
    },
  ],
};
