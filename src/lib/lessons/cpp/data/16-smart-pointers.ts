import type { Lesson } from "../../types";

export const smartPointers: Lesson = {
	id: "smart-pointers",
	title: "Smart Pointers",
	chapterId: "modern-cpp",
	content: `## Smart Pointers

In modern C++, **smart pointers** manage dynamically allocated memory automatically, preventing memory leaks and dangling pointers. They are defined in the \`<memory>\` header.

### The Problem with Raw Pointers

\`\`\`cpp
void leaky() {
    int* p = new int(42);
    // if an exception occurs here, memory leaks!
    delete p;  // easy to forget
}
\`\`\`

Smart pointers solve this by automatically freeing memory when the pointer goes out of scope.

### std::unique_ptr

A \`unique_ptr\` **owns** its object exclusively — no other pointer can share ownership.

\`\`\`cpp
#include <memory>
using namespace std;

unique_ptr<int> p = make_unique<int>(42);
cout << *p << endl;  // 42

// Ownership can be transferred:
unique_ptr<int> q = move(p);
// p is now nullptr, q owns the object
\`\`\`

Key rules:
- Cannot be copied — only **moved**
- When it goes out of scope, the object is deleted
- Use \`make_unique<T>(args...)\` to create one

### std::shared_ptr

A \`shared_ptr\` allows **multiple pointers** to share ownership. The object is deleted when the **last** shared_ptr to it is destroyed.

\`\`\`cpp
shared_ptr<int> a = make_shared<int>(100);
shared_ptr<int> b = a;  // both point to same object

cout << a.use_count() << endl;  // 2
cout << *a << endl;             // 100
cout << *b << endl;             // 100
\`\`\`

Key rules:
- Can be copied freely
- Tracks a **reference count** — how many shared_ptrs point to the object
- Object is deleted when reference count reaches 0
- Use \`make_shared<T>(args...)\` to create one

### When to Use Each

| Pointer | Use when... |
|---------|-------------|
| \`unique_ptr\` | One owner. Default choice for heap objects. |
| \`shared_ptr\` | Multiple owners need the same object. |
| Raw pointer | Non-owning reference (observer). Never \`new\`/\`delete\`. |

### Best Practices

1. **Prefer \`make_unique\` / \`make_shared\`** over \`new\`
2. **Default to \`unique_ptr\`** — upgrade to \`shared_ptr\` only if needed
3. **Never use raw \`new\`/\`delete\`** in modern C++
4. Pass smart pointers by reference to avoid unnecessary copies

### Your Task

In this exercise, you will implement simplified smart pointer classes to understand how they work internally.

Create a \`UniquePtr\` class that wraps an integer value and provides:
- A constructor that takes the value
- A \`get()\` method that returns the value
- Demonstrate exclusive ownership

Create a \`SharedPtr\` class that wraps an integer value and tracks reference count:
- A constructor that takes the value
- A \`get()\` method that returns the value
- A \`refCount()\` method that returns the count

Print the results as shown in the tests.`,

	starterCode: `#include <iostream>
using namespace std;

class UniquePtr {
public:
	int value;

	UniquePtr(int v) {
		// Store the value
	}

	int get() {
		// Return the value
	}
};

class SharedPtr {
public:
	int value;
	int count;

	SharedPtr(int v) {
		// Store the value and set count to 1
	}

	int get() {
		// Return the value
	}

	int refCount() {
		// Return the reference count
	}
};

int main() {
	UniquePtr u(42);
	cout << "UniquePtr value: " << u.get() << endl;

	SharedPtr s(100);
	cout << "SharedPtr value: " << s.get() << endl;
	cout << "Ref count: " << s.refCount() << endl;

	return 0;
}
`,

	solution: `#include <iostream>
using namespace std;

class UniquePtr {
public:
	int value;

	UniquePtr(int v) {
		this->value = v;
	}

	int get() {
		return this->value;
	}
};

class SharedPtr {
public:
	int value;
	int count;

	SharedPtr(int v) {
		this->value = v;
		this->count = 1;
	}

	int get() {
		return this->value;
	}

	int refCount() {
		return this->count;
	}
};

int main() {
	UniquePtr u(42);
	cout << "UniquePtr value: " << u.get() << endl;

	SharedPtr s(100);
	cout << "SharedPtr value: " << s.get() << endl;
	cout << "Ref count: " << s.refCount() << endl;

	return 0;
}
`,

	tests: [
		{
			name: "UniquePtr and SharedPtr basic usage",
			expected: "UniquePtr value: 42\nSharedPtr value: 100\nRef count: 1\n",
		},
		{
			name: "UniquePtr with different value",
			expected: "Value: 99\n",
			code: `#include <iostream>
using namespace std;
class UniquePtr {
public:
	int value;
	UniquePtr(int v) {
		this->value = v;
	}
	int get() {
		return this->value;
	}
};
int main() {
	UniquePtr p(99);
	cout << "Value: " << p.get() << endl;
	return 0;
}
`,
		},
		{
			name: "SharedPtr tracks reference count",
			expected: "Value: 50\nCount: 1\nAfter increment: 2\nAfter decrement: 1\n",
			code: `#include <iostream>
using namespace std;
class SharedPtr {
public:
	int value;
	int count;
	SharedPtr(int v) {
		this->value = v;
		this->count = 1;
	}
	int get() {
		return this->value;
	}
	int refCount() {
		return this->count;
	}
	void addRef() {
		this->count = this->count + 1;
	}
	void release() {
		this->count = this->count - 1;
	}
};
int main() {
	SharedPtr s(50);
	cout << "Value: " << s.get() << endl;
	cout << "Count: " << s.refCount() << endl;
	s.addRef();
	cout << "After increment: " << s.refCount() << endl;
	s.release();
	cout << "After decrement: " << s.refCount() << endl;
	return 0;
}
`,
		},
		{
			name: "multiple UniquePtr instances are independent",
			expected: "A: 10\nB: 20\nSum: 30\n",
			code: `#include <iostream>
using namespace std;
class UniquePtr {
public:
	int value;
	UniquePtr(int v) {
		this->value = v;
	}
	int get() {
		return this->value;
	}
};
int main() {
	UniquePtr a(10);
	UniquePtr b(20);
	cout << "A: " << a.get() << endl;
	cout << "B: " << b.get() << endl;
	int sum = a.get() + b.get();
	cout << "Sum: " << sum << endl;
	return 0;
}
`,
		},
	],
};
