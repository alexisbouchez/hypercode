import type { Lesson } from "../../types";

export const variables: Lesson = {
	id: "variables",
	title: "Variables and Types",
	chapterId: "basics",
	content: `## Variables and Types in C++

C++ inherits all of C's primitive types and adds more. The most commonly used types:

| Type | Description | Example |
|------|-------------|---------|
| \`int\` | Integer | \`42\` |
| \`double\` | Floating point | \`3.14\` |
| \`bool\` | True or false | \`true\` |
| \`char\` | Single character | \`'A'\` |
| \`string\` | Text | \`"hello"\` |

### Declaring Variables

\`\`\`cpp
int age = 25;
double temperature = 98.6;
bool isRaining = false;
string city = "Tokyo";
\`\`\`

### auto

C++11 introduced \`auto\`, which lets the compiler deduce the type:

\`\`\`cpp
auto x = 10;       // int
auto y = 3.14;     // double
auto name = string("Alice");  // string
\`\`\`

Use \`auto\` when the type is obvious from the initializer.

### Printing Variables with cout

Use \`<<\` to chain values:

\`\`\`cpp
int score = 95;
string grade = "A";
cout << "Score: " << score << ", Grade: " << grade << endl;
\`\`\`

### Boolean Output

In C++, \`true\` prints as \`1\` and \`false\` prints as \`0\` by default:

\`\`\`cpp
cout << true << endl;   // prints: 1
cout << false << endl;  // prints: 0
\`\`\`

### string Type

Unlike C's \`char*\`, C++'s \`std::string\` manages memory automatically:

\`\`\`cpp
#include <string>
string greeting = "Hello";
string name = "World";
string combined = greeting + ", " + name + "!";
cout << combined << endl;  // Hello, World!
\`\`\`

### Your Task

Declare the following variables and print each one on its own line:
- \`int count = 42\`
- \`double pi = 3.14\`
- \`bool active = true\`
- \`string language = "C++"\`

Print them with labels: \`Count: 42\`, \`Pi: 3.14\`, \`Active: 1\`, \`Language: C++\``,

	starterCode: `#include <iostream>
#include <string>
using namespace std;

int main() {
	// Declare count, pi, active, language and print each
	return 0;
}
`,

	solution: `#include <iostream>
#include <string>
using namespace std;

int main() {
	int count = 42;
	double pi = 3.14;
	bool active = true;
	string language = "C++";

	cout << "Count: " << count << endl;
	cout << "Pi: " << pi << endl;
	cout << "Active: " << active << endl;
	cout << "Language: " << language << endl;
	return 0;
}
`,

	tests: [
		{
			name: "prints all variables with labels",
			expected: "Count: 42\nPi: 3.14\nActive: 1\nLanguage: C++\n",
		},
	],
};
