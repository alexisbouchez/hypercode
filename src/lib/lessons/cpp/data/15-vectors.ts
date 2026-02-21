import type { Lesson } from "../../types";

export const vectors: Lesson = {
	id: "vectors",
	title: "Vectors",
	chapterId: "templates",
	content: `## std::vector

\`std::vector\` is a dynamic array from the C++ Standard Library. Unlike C arrays, vectors grow and shrink automatically.

### Creating Vectors

\`\`\`cpp
#include <vector>
using namespace std;

vector<int> nums;          // empty vector
vector<int> primes = {2, 3, 5, 7};  // with initial values
vector<string> words(3, "hello");   // 3 copies of "hello"
\`\`\`

### Adding Elements

\`push_back\` appends to the end:

\`\`\`cpp
vector<int> v;
v.push_back(10);
v.push_back(20);
v.push_back(30);
\`\`\`

### Accessing Elements

\`\`\`cpp
cout << v[0] << endl;       // 10 — by index
cout << v.front() << endl;  // 10 — first element
cout << v.back() << endl;   // 30 — last element
cout << v.size() << endl;   // 3 — number of elements
\`\`\`

### Iterating

Traditional index loop:
\`\`\`cpp
for (int i = 0; i < v.size(); i++) {
    cout << v[i] << endl;
}
\`\`\`

Range-based for (preferred):
\`\`\`cpp
for (int n : v) {
    cout << n << endl;
}

// For non-primitive types, use const reference:
for (const string& s : words) {
    cout << s << endl;
}
\`\`\`

### Modifying

\`\`\`cpp
v.pop_back();          // remove last element
v[1] = 99;            // change element at index 1
\`\`\`

### Passing to Functions

\`\`\`cpp
void printAll(const vector<int>& v) {
    for (int n : v) cout << n << " ";
    cout << endl;
}
\`\`\`

Use \`const vector<T>&\` to avoid copying and prevent modification.

### vector vs C Array

| | C array | vector |
|--|---------|--------|
| Size | Fixed at compile time | Dynamic |
| Bounds checking | None | None (use \`.at()\`) |
| Copy | Manual | Automatic |
| Pass to function | Decays to pointer | Passed by value/ref |

### Your Task

Create a \`vector<int>\` with five elements (10, 20, 30, 40, 50). Print:
1. The size
2. The first element
3. The last element
4. The sum of all elements`,

	starterCode: `#include <iostream>
#include <vector>
using namespace std;

int main() {
	vector<int> v;
	v.push_back(10);
	v.push_back(20);
	v.push_back(30);
	v.push_back(40);
	v.push_back(50);

	// Print size, first, last, and sum
	return 0;
}
`,

	solution: `#include <iostream>
#include <vector>
using namespace std;

int main() {
	vector<int> v;
	v.push_back(10);
	v.push_back(20);
	v.push_back(30);
	v.push_back(40);
	v.push_back(50);

	cout << "Size: " << v.size() << endl;
	cout << "First: " << v[0] << endl;
	cout << "Last: " << v.back() << endl;

	int sum = 0;
	for (int n : v) {
		sum += n;
	}
	cout << "Sum: " << sum << endl;
	return 0;
}
`,

	tests: [
		{
			name: "prints vector size, elements, and sum",
			expected: "Size: 5\nFirst: 10\nLast: 50\nSum: 150\n",
		},
	],
};
