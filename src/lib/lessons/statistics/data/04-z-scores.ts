import type { Lesson } from "../../types";

export const zScores: Lesson = {
	id: "z-scores",
	title: "Z-Scores",
	chapterId: "descriptive",
	content: `## Standardization

A **z-score** measures how many standard deviations a value is from the mean:

\`\`\`
z = (x - μ) / σ
\`\`\`

\`\`\`python
from scipy import stats
import numpy as np

data = [1, 2, 3, 4, 5]
z = stats.zscore(data)  # uses population std (ddof=0)
print(np.round(z, 4))
# [-1.4142 -0.7071  0.      0.7071  1.4142]
\`\`\`

### Properties

A standardized dataset always has:
- **Mean = 0**
- **Standard deviation = 1**

### Interpreting Z-Scores

| Z-score | Meaning |
|---------|---------|
| z = 0 | Exactly at the mean |
| z = 1 | One std above the mean |
| z = -2 | Two stds below the mean |
| \|z\| > 3 | Likely an outlier |

### Use Cases

- Compare values from different scales (e.g., height and weight)
- Detect outliers
- Normalize features before machine learning

### Your Task

Implement \`z_scores(data)\` that returns the z-scores of the data using \`scipy.stats.zscore\`.`,

	starterCode: `from scipy import stats
import numpy as np

def z_scores(data):
    # Return scipy.stats.zscore of data
    pass

z = z_scores([1, 2, 3, 4, 5])
print(round(float(z[0]), 4))
print(round(float(z[-1]), 4))
`,

	solution: `from scipy import stats
import numpy as np

def z_scores(data):
    return stats.zscore(data)

z = z_scores([1, 2, 3, 4, 5])
print(round(float(z[0]), 4))
print(round(float(z[-1]), 4))
`,

	tests: [
		{
			name: "z_scores([1,2,3,4,5]) → first=-1.4142, last=1.4142",
			expected: "-1.4142\n1.4142\n",
		},
		{
			name: "mean of z-scores is ~0",
			code: `{{FUNC}}
z = z_scores([2, 4, 4, 4, 5, 5, 7, 9])
print(abs(round(float(z.mean()), 4)) == 0)`,
			expected: "True\n",
		},
		{
			name: "std of z-scores is 1.0",
			code: `{{FUNC}}
z = z_scores([2, 4, 4, 4, 5, 5, 7, 9])
print(round(float(z.std()), 4) == 1.0)`,
			expected: "True\n",
		},
		{
			name: "z-score of value at mean is 0.0",
			code: `{{FUNC}}
z = z_scores([1, 2, 3, 4, 5])
print(round(float(z[2]), 4))`,
			expected: "0.0\n",
		},
	],
};
