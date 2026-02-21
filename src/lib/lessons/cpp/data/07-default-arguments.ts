import type { Lesson } from "../../types";

export const defaultArguments: Lesson = {
	id: "default-arguments",
	title: "Default Arguments",
	chapterId: "functions",
	content: `## Default Arguments

C++ lets you give parameters a **default value**. If the caller doesn't supply that argument, the default is used.

### Syntax

\`\`\`cpp
void greet(string name, string greeting = "Hello") {
    cout << greeting << ", " << name << "!" << endl;
}

greet("Alice");          // Hello, Alice!
greet("Bob", "Hi");      // Hi, Bob!
greet("Carol", "Hey");   // Hey, Carol!
\`\`\`

### Rules

- Defaults must be at the **end** of the parameter list
- Once a parameter has a default, all following parameters must too

\`\`\`cpp
// Valid
void f(int a, int b = 2, int c = 3);

// Invalid — non-default after default
void g(int a = 1, int b, int c = 3);  // error
\`\`\`

### Mixing Required and Optional Parameters

\`\`\`cpp
string repeat(string text, int times = 3, string sep = ", ") {
    string result = "";
    for (int i = 0; i < times; i++) {
        if (i > 0) result += sep;
        result += text;
    }
    return result;
}

cout << repeat("ha") << endl;              // ha, ha, ha
cout << repeat("go", 2) << endl;           // go, go
cout << repeat("x", 4, "-") << endl;       // x-x-x-x
\`\`\`

### When to Use Default Arguments

Use defaults when a parameter has a sensible common value that most callers would want. They reduce the need for overloads in simple cases.

### Your Task

Write a \`greet\` function that takes a \`name\` and an optional \`greeting\` (default \`"Hello"\`), and a \`multiply\` function that takes \`a\` and optional \`b\` (default \`2\`), returning \`a * b\`.

Call them as shown to produce the expected output.`,

	starterCode: `#include <iostream>
#include <string>
using namespace std;

// greet(name, greeting = "Hello") — prints "<greeting>, <name>!"
// multiply(a, b = 2) — returns a * b

int main() {
	greet("Alice");
	greet("Bob", "Hi");
	cout << multiply(5) << endl;
	cout << multiply(5, 3) << endl;
	return 0;
}
`,

	solution: `#include <iostream>
#include <string>
using namespace std;

void greet(string name, string greeting = "Hello") {
	cout << greeting << ", " << name << "!" << endl;
}

int multiply(int a, int b = 2) {
	return a * b;
}

int main() {
	greet("Alice");
	greet("Bob", "Hi");
	cout << multiply(5) << endl;
	cout << multiply(5, 3) << endl;
	return 0;
}
`,

	tests: [
		{
			name: "uses default and explicit arguments",
			expected: "Hello, Alice!\nHi, Bob!\n10\n15\n",
		},
	],
};
