import type { Lesson } from "../../types";

export const vasicekModel: Lesson = {
  id: "vasicek-model",
  title: "Vasicek Interest Rate Model",
  chapterId: "yield-curves",
  content: `## Vasicek Interest Rate Model

The **Vasicek model** (1977) is the first equilibrium interest rate model. It describes how short rates evolve over time with mean reversion.

### Stochastic Differential Equation

$$dr = \\kappa(\\theta - r) \\, dt + \\sigma \\, dW$$

Where:
- $r$ = current short rate
- $\\kappa$ = mean reversion speed
- $\\theta$ = long-run mean (equilibrium rate)
- $\\sigma$ = volatility
- $dW$ = Brownian motion increment ($\\sim \\mathcal{N}(0, dt)$)

### Discrete-Time Approximation

For a small time step $\\Delta t$ with a standard normal draw $\\Delta W$:

$$r_{t+\\Delta t} = r_t + \\kappa(\\theta - r_t)\\Delta t + \\sigma\\sqrt{\\Delta t} \\cdot \\Delta W$$

### Analytical Zero-Coupon Bond Price

The Vasicek model has a closed-form bond price:

$$P(0,T) = A(T) \\cdot e^{-B(T) \\cdot r_0}$$

$$B(T) = \\frac{1 - e^{-\\kappa T}}{\\kappa}$$

$$A(T) = \\exp\\left[\\left(\\theta - \\frac{\\sigma^2}{2\\kappa^2}\\right)(B(T) - T) - \\frac{\\sigma^2 B(T)^2}{4\\kappa}\\right]$$

Use \`math.exp()\` and \`math.sqrt()\` for implementation.`,
  starterCode: `import math

def vasicek_rate(r0, kappa, theta, sigma, dt, dW):
    # One-step Vasicek rate update
    # r0 + kappa*(theta-r0)*dt + sigma*sqrt(dt)*dW
    pass

def vasicek_bond_price(r0, kappa, theta, sigma, T):
    # Analytical zero-coupon bond price under Vasicek model
    # B(T) = (1 - exp(-kappa*T)) / kappa
    # A(T) = exp((theta - sigma^2/(2*kappa^2))*(B-T) - sigma^2*B^2/(4*kappa))
    # P = A * exp(-B * r0)
    pass`,
  solution: `import math

def vasicek_rate(r0, kappa, theta, sigma, dt, dW):
    return r0 + kappa * (theta - r0) * dt + sigma * math.sqrt(dt) * dW

def vasicek_bond_price(r0, kappa, theta, sigma, T):
    B = (1 - math.exp(-kappa * T)) / kappa
    A = math.exp((theta - sigma ** 2 / (2 * kappa ** 2)) * (B - T) - sigma ** 2 * B ** 2 / (4 * kappa))
    return A * math.exp(-B * r0)`,
  tests: [
    {
      name: "vasicek_rate(0.05, 0.1, 0.05, 0.02, 1/252, 0.5) ≈ 0.05063",
      code: `{{FUNC}}\nprint(round(vasicek_rate(0.05, 0.1, 0.05, 0.02, 1/252, 0.5), 6))`,
      expected: "0.05063\n",
    },
    {
      name: "vasicek_bond_price(0.05, 0.5, 0.05, 0.1, 1.0) ≈ 0.952338",
      code: `{{FUNC}}\nprint(round(vasicek_bond_price(0.05, 0.5, 0.05, 0.1, 1.0), 6))`,
      expected: "0.952338\n",
    },
    {
      name: "vasicek_rate(0.03, 0.2, 0.06, 0.01, 1/252, -0.5) ≈ 0.029709",
      code: `{{FUNC}}\nprint(round(vasicek_rate(0.03, 0.2, 0.06, 0.01, 1/252, -0.5), 6))`,
      expected: "0.029709\n",
    },
    {
      name: "vasicek_bond_price(0.03, 0.3, 0.06, 0.05, 2.0) ≈ 0.929876",
      code: `{{FUNC}}\nprint(round(vasicek_bond_price(0.03, 0.3, 0.06, 0.05, 2.0), 6))`,
      expected: "0.929876\n",
    },
  ],
};
