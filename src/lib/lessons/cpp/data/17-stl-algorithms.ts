import type { Lesson } from "../../types";

export const stlAlgorithms: Lesson = {
	id: "stl-algorithms",
	title: "STL Algorithms",
	chapterId: "modern-cpp",
	content: `## STL Algorithms

The C++ Standard Library provides a rich set of **algorithms** in \`<algorithm>\` and \`<numeric>\` that operate on ranges (pairs of iterators). These algorithms replace hand-written loops with expressive, well-tested operations.

### Sorting

\`std::sort\` sorts elements in ascending order by default:

\`\`\`cpp
#include <algorithm>
#include <vector>

vector<int> v = {5, 2, 8, 1, 9};
sort(v.begin(), v.end());
// v is now {1, 2, 5, 8, 9}
\`\`\`

### Searching

\`std::find\` returns an iterator to the first matching element:

\`\`\`cpp
auto it = find(v.begin(), v.end(), 8);
if (it != v.end()) {
    cout << "Found: " << *it << endl;
}
\`\`\`

### Counting

\`std::count\` counts how many elements match a value:

\`\`\`cpp
vector<int> nums = {1, 2, 3, 2, 4, 2};
int twos = count(nums.begin(), nums.end(), 2);
// twos == 3
\`\`\`

### Accumulate (Sum / Fold)

\`std::accumulate\` (from \`<numeric>\`) computes a running total:

\`\`\`cpp
#include <numeric>

vector<int> v = {1, 2, 3, 4, 5};
int total = accumulate(v.begin(), v.end(), 0);
// total == 15
\`\`\`

### Transform

\`std::transform\` applies a function to each element and stores the result:

\`\`\`cpp
vector<int> src = {1, 2, 3};
vector<int> dst(3);
transform(src.begin(), src.end(), dst.begin(),
    [](int x) { return x * x; });
// dst == {1, 4, 9}
\`\`\`

### Predicates: any_of / all_of

These test whether elements satisfy a condition:

\`\`\`cpp
vector<int> v = {2, 4, 6, 8};
bool allEven = all_of(v.begin(), v.end(),
    [](int x) { return x % 2 == 0; });
// allEven == true

bool anyNeg = any_of(v.begin(), v.end(),
    [](int x) { return x < 0; });
// anyNeg == false
\`\`\`

### Why Use STL Algorithms?

1. **Correctness** — battle-tested implementations
2. **Readability** — \`sort(v.begin(), v.end())\` is clearer than a hand-rolled sort
3. **Performance** — optimized by compiler vendors
4. **Composability** — algorithms chain together naturally

### Your Task

Implement simplified versions of common STL algorithms as standalone functions:

1. \`bubbleSort\` — sort a vector of integers in ascending order
2. \`findValue\` — return the index of a value (-1 if not found)
3. \`countValue\` — count occurrences of a value
4. \`accumulate\` — sum all elements starting from an initial value

Print the results as shown in the tests.`,

	starterCode: `#include <iostream>
#include <vector>
using namespace std;

void bubbleSort(vector<int>& v) {
	// Sort v in ascending order using bubble sort
}

int findValue(vector<int>& v, int target) {
	// Return the index of target, or -1 if not found
}

int countValue(vector<int>& v, int target) {
	// Count how many times target appears in v
}

int accumulate(vector<int>& v, int init) {
	// Return the sum of all elements plus init
}

int main() {
	vector<int> nums = {5, 3, 8, 1, 2};
	bubbleSort(nums);
	for (int n : nums) {
		cout << n << " ";
	}
	cout << endl;

	int idx = findValue(nums, 3);
	cout << "Found 3 at index: " << idx << endl;

	vector<int> data = {1, 2, 3, 2, 4, 2};
	cout << "Count of 2: " << countValue(data, 2) << endl;

	cout << "Sum: " << accumulate(nums, 0) << endl;
	return 0;
}
`,

	solution: `#include <iostream>
#include <vector>
using namespace std;

void bubbleSort(vector<int>& v) {
	int n = v.size();
	for (int i = 0; i < n; i++) {
		for (int j = 0; j < n - 1 - i; j++) {
			if (v[j] > v[j + 1]) {
				int temp = v[j];
				v[j] = v[j + 1];
				v[j + 1] = temp;
			}
		}
	}
}

int findValue(vector<int>& v, int target) {
	for (int i = 0; i < v.size(); i++) {
		if (v[i] == target) {
			return i;
		}
	}
	return -1;
}

int countValue(vector<int>& v, int target) {
	int c = 0;
	for (int i = 0; i < v.size(); i++) {
		if (v[i] == target) {
			c = c + 1;
		}
	}
	return c;
}

int accumulate(vector<int>& v, int init) {
	int total = init;
	for (int i = 0; i < v.size(); i++) {
		total = total + v[i];
	}
	return total;
}

int main() {
	vector<int> nums = {5, 3, 8, 1, 2};
	bubbleSort(nums);
	for (int n : nums) {
		cout << n << " ";
	}
	cout << endl;

	int idx = findValue(nums, 3);
	cout << "Found 3 at index: " << idx << endl;

	vector<int> data = {1, 2, 3, 2, 4, 2};
	cout << "Count of 2: " << countValue(data, 2) << endl;

	cout << "Sum: " << accumulate(nums, 0) << endl;
	return 0;
}
`,

	tests: [
		{
			name: "sort, find, count, and accumulate",
			expected: "1 2 3 5 8 \nFound 3 at index: 2\nCount of 2: 3\nSum: 19\n",
		},
		{
			name: "bubbleSort with already sorted input",
			expected: "1 2 3 4 5 \n",
			code: `#include <iostream>
#include <vector>
using namespace std;
void bubbleSort(vector<int>& v) {
	int n = v.size();
	for (int i = 0; i < n; i++) {
		for (int j = 0; j < n - 1 - i; j++) {
			if (v[j] > v[j + 1]) {
				int temp = v[j];
				v[j] = v[j + 1];
				v[j + 1] = temp;
			}
		}
	}
}
int main() {
	vector<int> v = {1, 2, 3, 4, 5};
	bubbleSort(v);
	for (int n : v) {
		cout << n << " ";
	}
	cout << endl;
	return 0;
}
`,
		},
		{
			name: "findValue returns -1 when not found",
			expected: "-1\n",
			code: `#include <iostream>
#include <vector>
using namespace std;
int findValue(vector<int>& v, int target) {
	for (int i = 0; i < v.size(); i++) {
		if (v[i] == target) {
			return i;
		}
	}
	return -1;
}
int main() {
	vector<int> v = {10, 20, 30};
	cout << findValue(v, 99) << endl;
	return 0;
}
`,
		},
		{
			name: "accumulate with non-zero initial value",
			expected: "115\n",
			code: `#include <iostream>
#include <vector>
using namespace std;
int accumulate(vector<int>& v, int init) {
	int total = init;
	for (int i = 0; i < v.size(); i++) {
		total = total + v[i];
	}
	return total;
}
int main() {
	vector<int> v = {10, 20, 30, 40};
	cout << accumulate(v, 15) << endl;
	return 0;
}
`,
		},
	],
};
