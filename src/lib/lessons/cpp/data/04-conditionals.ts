import type { Lesson } from "../../types";

export const conditionals: Lesson = {
	id: "conditionals",
	title: "Conditionals",
	chapterId: "control-flow",
	content: `## Conditionals in C++

C++ conditionals work the same as in C: \`if\`, \`else if\`, \`else\`, and the ternary operator.

### if / else if / else

\`\`\`cpp
int temperature = 25;

if (temperature > 30) {
    cout << "Hot" << endl;
} else if (temperature > 20) {
    cout << "Warm" << endl;
} else if (temperature > 10) {
    cout << "Cool" << endl;
} else {
    cout << "Cold" << endl;
}
\`\`\`

### Comparing Strings

With C++ \`string\`, you can use \`==\` directly:

\`\`\`cpp
string color = "red";
if (color == "red") {
    cout << "Stop" << endl;
} else if (color == "green") {
    cout << "Go" << endl;
}
\`\`\`

### Ternary Operator

A compact one-line conditional:

\`\`\`cpp
int x = 10;
string result = (x > 0) ? "positive" : "non-positive";
cout << result << endl;
\`\`\`

### Logical Operators

| Operator | Meaning |
|----------|---------|
| \`&&\` | AND |
| \`||\` | OR |
| \`!\` | NOT |

\`\`\`cpp
int age = 25;
bool hasID = true;

if (age >= 18 && hasID) {
    cout << "Welcome" << endl;
}
\`\`\`

### Your Task

Given a score of 85:
1. Print the score
2. Assign a letter grade (A ≥ 90, B ≥ 80, C ≥ 70, otherwise F)
3. Print the grade`,

	starterCode: `#include <iostream>
#include <string>
using namespace std;

int main() {
	int score = 85;
	string grade;

	// Assign grade based on score
	// A >= 90, B >= 80, C >= 70, otherwise F

	cout << "Score: " << score << endl;
	cout << "Grade: " << grade << endl;
	return 0;
}
`,

	solution: `#include <iostream>
#include <string>
using namespace std;

int main() {
	int score = 85;
	string grade;

	if (score >= 90) {
		grade = "A";
	} else if (score >= 80) {
		grade = "B";
	} else if (score >= 70) {
		grade = "C";
	} else {
		grade = "F";
	}

	cout << "Score: " << score << endl;
	cout << "Grade: " << grade << endl;
	return 0;
}
`,

	tests: [
		{
			name: "prints score and grade B",
			expected: "Score: 85\nGrade: B\n",
		},
	],
};
