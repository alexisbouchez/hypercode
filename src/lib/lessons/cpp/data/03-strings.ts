import type { Lesson } from "../../types";

export const strings: Lesson = {
	id: "strings",
	title: "String Operations",
	chapterId: "basics",
	content: `## Strings in C++

C++ \`std::string\` is a major improvement over C's null-terminated \`char*\` arrays. It manages memory automatically and comes with many built-in methods.

### Creating Strings

\`\`\`cpp
#include <string>
using namespace std;

string name = "Alice";
string empty = "";
string repeated(5, 'x');  // "xxxxx"
\`\`\`

### Concatenation

Use \`+\` to combine strings:

\`\`\`cpp
string first = "Hello";
string second = "World";
string combined = first + ", " + second + "!";
// "Hello, World!"
\`\`\`

Use \`+=\` to append:

\`\`\`cpp
string s = "foo";
s += "bar";  // "foobar"
\`\`\`

### Length

\`\`\`cpp
string s = "hello";
cout << s.length() << endl;  // 5
cout << s.size() << endl;    // also 5
\`\`\`

### Substrings

\`substr(pos, length)\` extracts a portion of the string:

\`\`\`cpp
string s = "Hello, World!";
cout << s.substr(0, 5) << endl;  // Hello
cout << s.substr(7, 5) << endl;  // World
\`\`\`

### Finding Substrings

\`find\` returns the index of the first match, or \`string::npos\` if not found:

\`\`\`cpp
string s = "Hello, World!";
int pos = s.find("World");   // 7
int pos2 = s.find("xyz");    // string::npos
\`\`\`

### Comparing Strings

Use \`==\` and \`!=\` directly:

\`\`\`cpp
string a = "hello";
string b = "hello";
if (a == b) cout << "equal" << endl;
\`\`\`

### Your Task

Build a sentence and then:
1. Print the combined string
2. Print its length
3. Print the first 5 characters (using substr)`,

	starterCode: `#include <iostream>
#include <string>
using namespace std;

int main() {
	string first = "Hello";
	string second = "World";
	// Combine them into "Hello, World!" and print
	// Print the length
	// Print the first 5 characters
	return 0;
}
`,

	solution: `#include <iostream>
#include <string>
using namespace std;

int main() {
	string first = "Hello";
	string second = "World";
	string combined = first + ", " + second + "!";

	cout << combined << endl;
	cout << "Length: " << combined.length() << endl;
	cout << "Starts with: " << combined.substr(0, 5) << endl;
	return 0;
}
`,

	tests: [
		{
			name: "prints combined string, length, and substring",
			expected: "Hello, World!\nLength: 13\nStarts with: Hello\n",
		},
	],
};
