import type { Lesson } from "../../types";

export const confidenceInterval: Lesson = {
	id: "confidence-interval",
	title: "Confidence Intervals",
	chapterId: "inference",
	content: `## Estimating the True Mean

A **confidence interval (CI)** gives a range of plausible values for the population mean, based on a sample.

\`\`\`python
from scipy import stats
import numpy as np

data = [1, 2, 3, 4, 5]
ci = stats.t.interval(
    confidence=0.95,
    df=len(data) - 1,
    loc=np.mean(data),
    scale=stats.sem(data)
)
print(round(ci[0], 2))   # 1.04
print(round(ci[1], 2))   # 4.96
\`\`\`

### Interpretation

A 95% CI means: if you repeated this sampling process 100 times, approximately **95 of the resulting CIs** would contain the true population mean.

**Common misconception**: It does NOT mean "there's a 95% chance the true mean is in this interval." The true mean is fixed; the interval is random.

### Width of the CI

The CI gets narrower (more precise) when:
- Sample size **n** increases
- Data variability **σ** decreases
- Confidence level decreases (e.g., 90% CI is narrower than 95%)

### t vs z

We use the **t-distribution** (not normal) because we estimate σ from the sample. As n → ∞, the t-distribution approaches the normal distribution.

### Your Task

Implement \`confidence_interval(data, confidence)\` that prints the **lower bound** and **upper bound** of the confidence interval, each rounded to 2 decimal places.`,

	starterCode: `from scipy import stats
import numpy as np

def confidence_interval(data, confidence):
    # Print lower and upper bounds of the CI, each rounded to 2 decimal places
    pass

confidence_interval([1, 2, 3, 4, 5], 0.95)
`,

	solution: `from scipy import stats
import numpy as np

def confidence_interval(data, confidence):
    ci = stats.t.interval(
        confidence=confidence,
        df=len(data) - 1,
        loc=np.mean(data),
        scale=stats.sem(data)
    )
    print(round(float(ci[0]), 2))
    print(round(float(ci[1]), 2))

confidence_interval([1, 2, 3, 4, 5], 0.95)
`,

	tests: [
		{
			name: "confidence_interval([1..5], 0.95) → [1.04, 4.96]",
			expected: "1.04\n4.96\n",
		},
		{
			name: "confidence_interval([10..50], 0.95) → [10.37, 49.63]",
			code: `{{FUNC}}
confidence_interval([10, 20, 30, 40, 50], 0.95)`,
			expected: "10.37\n49.63\n",
		},
		{
			name: "CI lower < mean < CI upper",
			code: `{{FUNC}}
import numpy as np
data = [1, 2, 3, 4, 5]
from scipy import stats
ci = stats.t.interval(0.95, df=4, loc=3.0, scale=stats.sem(data))
print(ci[0] < np.mean(data) < ci[1])`,
			expected: "True\n",
		},
		{
			name: "90% CI is narrower than 95% CI",
			code: `{{FUNC}}
from scipy import stats
import numpy as np
data = [1, 2, 3, 4, 5]
ci90 = stats.t.interval(0.90, df=4, loc=3.0, scale=stats.sem(data))
ci95 = stats.t.interval(0.95, df=4, loc=3.0, scale=stats.sem(data))
w90 = ci90[1] - ci90[0]
w95 = ci95[1] - ci95[0]
print(w90 < w95)`,
			expected: "True\n",
		},
	],
};
