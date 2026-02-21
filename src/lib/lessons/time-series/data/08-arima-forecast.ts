import type { Lesson } from "../../types";

export const arimaForecastLesson: Lesson = {
  id: "arima-forecast",
  title: "ARIMA Forecast",
  chapterId: "stationarity",
  content: `## ARIMA(p,d,q) Forecast

**ARIMA** (AutoRegressive Integrated Moving Average) extends ARMA to handle non-stationary series by differencing \`d\` times before applying ARMA.

The workflow:
1. **Difference** the series \`d\` times → get a stationary differenced series
2. **Forecast** the differenced series using AR(p) (for simplicity, assuming zero noise and no MA term in future steps)
3. **Undifference** the forecasts back to the original scale

### Forecasting Steps

Given series \`xs\`, parameters \`p\`, \`d\`, \`q\`, AR coefficients \`phi\`, and steps ahead \`n_ahead\`:

1. Compute \`dxs = difference(xs, d)\`
2. For each future step, forecast next differenced value: \`ŷ_diff = Σ φⱼ · dxs[-(j+1)]\`
3. Append forecast to \`dxs\` (iterating forward)
4. Undifference: \`last = xs[-1]\`, then \`last += fd\` for each forecasted difference

### Task

Implement \`arima_forecast(xs, p, d, q, phi, theta, n_ahead)\` that returns a list of \`n_ahead\` forecasted values.
`,
  starterCode: `def difference(xs, d=1):
    result = list(xs)
    for _ in range(d):
        result = [result[i+1] - result[i] for i in range(len(result)-1)]
    return result

def arima_forecast(xs, p, d, q, phi, theta, n_ahead):
    # 1. Difference d times
    # 2. Forecast n_ahead steps on differenced series using AR(p)
    # 3. Undifference back to original scale (start from xs[-1])
    pass
`,
  solution: `def difference(xs, d=1):
    result = list(xs)
    for _ in range(d):
        result = [result[i+1] - result[i] for i in range(len(result)-1)]
    return result

def arima_forecast(xs, p, d, q, phi, theta, n_ahead):
    dxs = list(difference(xs, d))
    forecasted_diff = []
    current = list(dxs)
    for _ in range(n_ahead):
        ar_val = sum(phi[j] * current[-(j+1)] for j in range(min(len(phi), len(current))))
        current.append(ar_val)
        forecasted_diff.append(ar_val)
    result = []
    last = xs[-1]
    for fd in forecasted_diff:
        last = last + fd
        result.append(last)
    return result
`,
  tests: [
    {
      name: "arima_forecast AR(1) d=1 on linear trend phi=0.8",
      code: `{{FUNC}}\nxs = [1.0, 2.0, 3.0, 4.0, 5.0]\nprint([round(v, 4) for v in arima_forecast(xs, 1, 1, 0, [0.8], [], 3)])`,
      expected: "[5.8, 6.44, 6.952]\n",
    },
    {
      name: "arima_forecast AR(1) d=1 on quadratic series phi=0.5",
      code: `{{FUNC}}\nxs = [2.0, 4.0, 7.0, 11.0]\nprint([round(v, 4) for v in arima_forecast(xs, 1, 1, 0, [0.5], [], 2)])`,
      expected: "[13.0, 14.0]\n",
    },
    {
      name: "arima_forecast AR(1) d=1 phi=1.0 follows constant difference",
      code: `{{FUNC}}\nxs = [1.0, 3.0, 5.0, 7.0]\nprint([round(v, 4) for v in arima_forecast(xs, 1, 1, 0, [1.0], [], 2)])`,
      expected: "[9.0, 11.0]\n",
    },
    {
      name: "arima_forecast AR(1) d=1 exponentially growing differences",
      code: `{{FUNC}}\nxs = [1.0, 2.0, 4.0, 8.0]\nprint([round(v, 4) for v in arima_forecast(xs, 1, 1, 0, [0.9], [], 2)])`,
      expected: "[11.6, 14.84]\n",
    },
  ],
};
