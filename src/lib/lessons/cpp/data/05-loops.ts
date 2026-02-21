import type { Lesson } from "../../types";

export const loops: Lesson = {
	id: "loops",
	title: "Loops",
	chapterId: "control-flow",
	content: `## Loops in C++

C++ has all the loops from C — \`for\`, \`while\`, \`do-while\` — plus a powerful C++11 addition: the **range-based for loop**.

### Traditional for Loop

\`\`\`cpp
for (int i = 0; i < 5; i++) {
    cout << i << endl;
}
\`\`\`

### while Loop

\`\`\`cpp
int n = 1;
while (n <= 10) {
    cout << n << " ";
    n *= 2;
}
cout << endl;
// 1 2 4 8
\`\`\`

### Range-Based for Loop

C++11 introduced a cleaner syntax for iterating over collections:

\`\`\`cpp
#include <vector>
using namespace std;

vector<int> nums = {10, 20, 30, 40};
for (int n : nums) {
    cout << n << endl;
}
\`\`\`

Use \`const string&\` for strings to avoid copying:

\`\`\`cpp
vector<string> names = {"Alice", "Bob", "Carol"};
for (const string& name : names) {
    cout << "Hello, " << name << endl;
}
\`\`\`

### break and continue

\`break\` exits the loop; \`continue\` skips to the next iteration:

\`\`\`cpp
for (int i = 0; i < 10; i++) {
    if (i == 3) continue;  // skip 3
    if (i == 7) break;     // stop at 7
    cout << i << " ";
}
// 0 1 2 4 5 6
\`\`\`

### Nested Loops

\`\`\`cpp
for (int i = 1; i <= 3; i++) {
    for (int j = 1; j <= 3; j++) {
        cout << i * j << " ";
    }
    cout << endl;
}
\`\`\`

### Your Task

1. Use a \`for\` loop to print \`1 2 3 4 5\` on one line (space-separated, no trailing space)
2. Use a range-based for loop to print each language in \`{"C", "C++", "Go"}\` on its own line`,

	starterCode: `#include <iostream>
#include <vector>
#include <string>
using namespace std;

int main() {
	// Print 1 2 3 4 5 on one line

	// Print each language on its own line
	vector<string> langs = {"C", "C++", "Go"};

	return 0;
}
`,

	solution: `#include <iostream>
#include <vector>
#include <string>
using namespace std;

int main() {
	for (int i = 1; i <= 5; i++) {
		cout << i;
		if (i < 5) cout << " ";
	}
	cout << endl;

	vector<string> langs = {"C", "C++", "Go"};
	for (const string& lang : langs) {
		cout << lang << endl;
	}
	return 0;
}
`,

	tests: [
		{
			name: "prints numbers and languages",
			expected: "1 2 3 4 5\nC\nC++\nGo\n",
		},
	],
};
