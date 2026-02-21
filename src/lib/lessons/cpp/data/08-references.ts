import type { Lesson } from "../../types";

export const references: Lesson = {
	id: "references",
	title: "References",
	chapterId: "functions",
	content: `## References

A **reference** is an alias — another name for an existing variable. References are one of C++'s key improvements over C.

### Declaring a Reference

\`\`\`cpp
int x = 10;
int& ref = x;  // ref is an alias for x

ref = 20;       // changes x through the alias
cout << x;      // 20
\`\`\`

A reference must be initialized when declared and cannot be rebound to another variable.

### References as Function Parameters

This is the most common use. In C, passing by value creates a copy:

\`\`\`cpp
void increment(int x) {
    x++;  // only changes the local copy
}
\`\`\`

With a reference, the function modifies the **original**:

\`\`\`cpp
void increment(int& x) {
    x++;  // modifies the original
}

int n = 5;
increment(n);
cout << n;  // 6
\`\`\`

### const References

Use \`const T&\` to pass large objects without copying and without allowing modification. This is the idiomatic C++ way to pass strings and other large objects to functions:

\`\`\`cpp
void greet(const string& name) {
    cout << "Hello, " << name << "!" << endl;
    // name = "Bob";  // error — const
}
\`\`\`

Without \`const&\`, passing a \`string\` would copy the entire string. With \`const&\`, only a pointer-sized alias is passed.

### Return by Reference

Functions can also return references to allow chaining or direct modification:

\`\`\`cpp
string& getLonger(string& a, string& b) {
    return a.length() >= b.length() ? a : b;
}
\`\`\`

### References vs Pointers

| | References | Pointers |
|--|------------|---------|
| Syntax | \`int& r = x\` | \`int* p = &x\` |
| Null | Cannot be null | Can be null |
| Rebind | Cannot rebind | Can reassign |
| Preferred for | Function parameters | Optional/nullable |

### Your Task

Write:
1. \`printPair(const string& first, const string& second)\` — prints \`<first> and <second>\`
2. \`combine(const string& a, const string& b, const string& sep)\` — returns \`a + sep + b\`

Use them in main to produce the expected output.`,

	starterCode: `#include <iostream>
#include <string>
using namespace std;

// void printPair(const string& first, const string& second)
// string combine(const string& a, const string& b, const string& sep)

int main() {
	string x = "Hello";
	string y = "World";

	printPair(x, y);
	cout << combine(x, y, ", ") << endl;
	cout << combine(x, y, " - ") << endl;
	return 0;
}
`,

	solution: `#include <iostream>
#include <string>
using namespace std;

void printPair(const string& first, const string& second) {
	cout << first << " and " << second << endl;
}

string combine(const string& a, const string& b, const string& sep) {
	return a + sep + b;
}

int main() {
	string x = "Hello";
	string y = "World";

	printPair(x, y);
	cout << combine(x, y, ", ") << endl;
	cout << combine(x, y, " - ") << endl;
	return 0;
}
`,

	tests: [
		{
			name: "const references used for parameter passing",
			expected: "Hello and World\nHello, World\nHello - World\n",
		},
	],
};
