import type { Lesson } from "../../types";

export const percentiles: Lesson = {
	id: "percentiles",
	title: "Percentiles & IQR",
	chapterId: "descriptive",
	content: `## Percentiles

The **p-th percentile** is the value below which $p$% of the data falls.

\`\`\`python
def percentile(data, p):
    s = sorted(data)
    n = len(s)
    i = (p / 100) * (n - 1)
    lo = int(i)
    hi = min(lo + 1, n - 1)
    return s[lo] + (i - lo) * (s[hi] - s[lo])

data = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]

q1 = percentile(data, 25)   # 25th percentile (Q1)
q3 = percentile(data, 75)   # 75th percentile (Q3)
iqr = q3 - q1               # Interquartile range

print(round(q1, 2))    # 3.25
print(round(q3, 2))    # 7.75
print(round(iqr, 2))   # 4.5
\`\`\`

### Interquartile Range (IQR)

The **IQR** is the range of the middle 50% of the data:

$$\text{IQR} = Q_3 - Q_1$$

It is resistant to outliers, making it a robust measure of spread.

### Outlier Detection

A common rule: a value is an outlier if it falls outside the fences:

$$Q_1 - 1.5 \times \text{IQR} \quad \text{or} \quad Q_3 + 1.5 \times \text{IQR}$$

\`\`\`python
lower_fence = q1 - 1.5 * iqr
upper_fence = q3 + 1.5 * iqr
\`\`\`

### Median = 50th Percentile

\`percentile(data, 50)\` is equivalent to the median.

### Your Task

Implement \`quartiles(data)\` that prints $Q_1$, $Q_3$, and IQR, each rounded to 2 decimal places.`,

	starterCode: `def percentile(data, p):
    s = sorted(data)
    n = len(s)
    i = (p / 100) * (n - 1)
    lo = int(i)
    hi = min(lo + 1, n - 1)
    return s[lo] + (i - lo) * (s[hi] - s[lo])

def quartiles(data):
    # Print Q1, Q3, and IQR (each rounded to 2 decimal places)
    pass

quartiles([1, 2, 3, 4, 5, 6, 7, 8, 9, 10])
`,

	solution: `def percentile(data, p):
    s = sorted(data)
    n = len(s)
    i = (p / 100) * (n - 1)
    lo = int(i)
    hi = min(lo + 1, n - 1)
    return s[lo] + (i - lo) * (s[hi] - s[lo])

def quartiles(data):
    q1 = percentile(data, 25)
    q3 = percentile(data, 75)
    iqr = q3 - q1
    print(round(q1, 2))
    print(round(q3, 2))
    print(round(iqr, 2))

quartiles([1, 2, 3, 4, 5, 6, 7, 8, 9, 10])
`,

	tests: [
		{
			name: "quartiles([1..10]) → Q1=3.25, Q3=7.75, IQR=4.5",
			expected: "3.25\n7.75\n4.5\n",
		},
		{
			name: "quartiles([1,1,1,1]) → Q1=1.0, Q3=1.0, IQR=0.0",
			code: `{{FUNC}}
quartiles([1, 1, 1, 1])`,
			expected: "1.0\n1.0\n0.0\n",
		},
		{
			name: "quartiles([0,10,20,30,40,50]) → Q1=12.5, Q3=37.5, IQR=25.0",
			code: `{{FUNC}}
quartiles([0, 10, 20, 30, 40, 50])`,
			expected: "12.5\n37.5\n25.0\n",
		},
		{
			name: "IQR is non-negative for any data",
			code: `{{FUNC}}
data = [5, 1, 8, 3, 7, 2, 9, 4, 6]
q1 = percentile(data, 25)
q3 = percentile(data, 75)
print(q3 - q1 >= 0)`,
			expected: "True\n",
		},
	],
};
