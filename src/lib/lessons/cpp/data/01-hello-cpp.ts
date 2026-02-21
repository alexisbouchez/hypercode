import type { Lesson } from "../../types";

export const helloCpp: Lesson = {
	id: "hello-cpp",
	title: "Hello, C++!",
	chapterId: "basics",
	content: `## Your First C++ Program

C++ is one of the most powerful and widely used programming languages ever created. Bjarne Stroustrup designed it at Bell Labs starting in 1979 as "C with Classes" — an extension of C that added object-oriented features. The name \`C++\` came in 1983: applying the increment operator to C.

### C++ vs C

C++ is a superset of C. Almost every valid C program is also valid C++. But C++ adds:

- **Classes and objects** — bundle data and behavior together
- **Inheritance** — share code between related classes
- **Templates** — write code that works for any type
- **The Standard Library** — \`vector\`, \`string\`, \`map\`, algorithms, and more

### Output with \`cout\`

In C, you use \`printf\` to print. C++ introduces a stream-based I/O system:

\`\`\`cpp
#include <iostream>
using namespace std;

int main() {
    cout << "Hello, World!" << endl;
    return 0;
}
\`\`\`

- \`#include <iostream>\` — includes the input/output stream library
- \`using namespace std;\` — lets you write \`cout\` instead of \`std::cout\`
- \`cout\` — the standard output stream (like \`stdout\` in C)
- \`<<\` — the insertion operator (feeds data into the stream)
- \`endl\` — inserts a newline and flushes the stream

### Chaining Output

You can chain multiple \`<<\` operators:

\`\`\`cpp
cout << "Value: " << 42 << endl;
cout << "Pi is " << 3.14 << " approximately" << endl;
\`\`\`

### Your Task

Print \`Hello, C++!\` followed by a newline.`,

	starterCode: `#include <iostream>
using namespace std;

int main() {
	// Print "Hello, C++!" followed by a newline
	return 0;
}
`,

	solution: `#include <iostream>
using namespace std;

int main() {
	cout << "Hello, C++!" << endl;
	return 0;
}
`,

	tests: [
		{
			name: "prints Hello, C++!",
			expected: "Hello, C++!\n",
		},
	],
};
