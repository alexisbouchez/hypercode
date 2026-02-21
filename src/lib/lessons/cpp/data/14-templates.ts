import type { Lesson } from "../../types";

export const templates: Lesson = {
	id: "templates",
	title: "Function Templates",
	chapterId: "templates",
	content: `## Function Templates

**Templates** let you write a single function that works with any type. The compiler generates the specific version for each type you use.

### Syntax

\`\`\`cpp
template <typename T>
T maximum(T a, T b) {
    if (a > b) return a;
    return b;
}
\`\`\`

\`T\` is a **type parameter** — a placeholder for the actual type. When you call the function, the compiler deduces \`T\`:

\`\`\`cpp
cout << maximum(10, 20) << endl;       // T = int
cout << maximum(3.14, 2.72) << endl;   // T = double
cout << maximum('a', 'z') << endl;     // T = char
\`\`\`

### Why Templates?

Without templates, you'd write one function per type:

\`\`\`cpp
int maximumInt(int a, int b) { ... }
double maximumDouble(double a, double b) { ... }
// etc.
\`\`\`

Templates eliminate that repetition. Any type that supports \`>\` works automatically.

### Multiple Type Parameters

\`\`\`cpp
template <typename A, typename B>
void printPair(A a, B b) {
    cout << a << " and " << b << endl;
}

printPair(42, "hello");    // 42 and hello
printPair(3.14, true);     // 3.14 and 1
\`\`\`

### Template Specialization

You can provide a special version for a specific type:

\`\`\`cpp
template <typename T>
T add(T a, T b) { return a + b; }

// Specialization for string (could do something different)
template <>
string add<string>(string a, string b) {
    return a + " + " + b;
}
\`\`\`

### Class Templates

Templates also work for classes (e.g., \`vector<T>\`, \`pair<A, B>\`):

\`\`\`cpp
template <typename T>
class Box {
public:
    T value;
    Box(T v) : value(v) {}
    void print() { cout << value << endl; }
};

Box<int> intBox(42);
Box<string> strBox("hello");
\`\`\`

### Your Task

Write two function templates:
- \`maximum(T a, T b)\` — returns the larger of two values
- \`minimum(T a, T b)\` — returns the smaller of two values

Test with integers.`,

	starterCode: `#include <iostream>
using namespace std;

// template <typename T>
// T maximum(T a, T b) { ... }

// template <typename T>
// T minimum(T a, T b) { ... }

int main() {
	cout << maximum(10, 20) << endl;
	cout << maximum(-5, -3) << endl;
	cout << minimum(8, 3) << endl;
	cout << minimum(-1, -4) << endl;
	return 0;
}
`,

	solution: `#include <iostream>
using namespace std;

template <typename T>
T maximum(T a, T b) {
	if (a > b) return a;
	return b;
}

template <typename T>
T minimum(T a, T b) {
	if (a < b) return a;
	return b;
}

int main() {
	cout << maximum(10, 20) << endl;
	cout << maximum(-5, -3) << endl;
	cout << minimum(8, 3) << endl;
	cout << minimum(-1, -4) << endl;
	return 0;
}
`,

	tests: [
		{
			name: "maximum and minimum work correctly",
			expected: "20\n-3\n3\n-4\n",
		},
	],
};
