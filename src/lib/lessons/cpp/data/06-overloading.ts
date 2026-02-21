import type { Lesson } from "../../types";

export const overloading: Lesson = {
	id: "overloading",
	title: "Function Overloading",
	chapterId: "functions",
	content: `## Function Overloading

C++ allows multiple functions with the **same name** as long as they have different parameter types or counts. The compiler picks the right version based on the arguments you pass.

### Defining Overloads

\`\`\`cpp
void print(int x) {
    cout << "int: " << x << endl;
}

void print(double x) {
    cout << "double: " << x << endl;
}

void print(string x) {
    cout << "string: " << x << endl;
}
\`\`\`

Calling \`print(42)\`, \`print(3.14)\`, or \`print("hello")\` automatically selects the matching version.

### Why Overloading?

Without overloading (C style), you'd need different names:

\`\`\`c
void print_int(int x);
void print_double(double x);
void print_string(char* x);
\`\`\`

Overloading lets you write natural, readable code with one name that works for many types.

### Different Parameter Counts

You can also overload by varying the number of parameters:

\`\`\`cpp
int add(int a, int b) {
    return a + b;
}

int add(int a, int b, int c) {
    return a + b + c;
}

cout << add(1, 2) << endl;     // 3
cout << add(1, 2, 3) << endl;  // 6
\`\`\`

### Rules

- The **return type alone** cannot distinguish overloads
- Parameters must differ in **type**, **count**, or **order**

\`\`\`cpp
// This is INVALID — same parameters, different return type:
// int foo(int x);
// double foo(int x);  // error
\`\`\`

### Your Task

Create three overloaded \`print\` functions:
- \`print(int x)\` — prints \`int: <value>\`
- \`print(double x)\` — prints \`double: <value>\`
- \`print(string x)\` — prints \`string: <value>\`

Then call them with \`42\`, \`3.14\`, and \`"hello"\`.`,

	starterCode: `#include <iostream>
#include <string>
using namespace std;

// Define three overloaded print functions here

int main() {
	print(42);
	print(3.14);
	print("hello");
	return 0;
}
`,

	solution: `#include <iostream>
#include <string>
using namespace std;

void print(int x) {
	cout << "int: " << x << endl;
}

void print(double x) {
	cout << "double: " << x << endl;
}

void print(string x) {
	cout << "string: " << x << endl;
}

int main() {
	print(42);
	print(3.14);
	print("hello");
	return 0;
}
`,

	tests: [
		{
			name: "calls correct overload for each type",
			expected: "int: 42\ndouble: 3.14\nstring: hello\n",
		},
	],
};
