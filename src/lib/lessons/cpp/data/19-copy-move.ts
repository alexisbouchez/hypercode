import type { Lesson } from "../../types";

export const copyMove: Lesson = {
	id: "copy-move",
	title: "Copy & Move Semantics",
	chapterId: "classes",
	content: `## Copy & Move Semantics

In C++, when you assign one object to another or pass objects to functions, you need to understand **copy semantics** — how object data is duplicated — and **move semantics** — how ownership is transferred.

### Shallow vs Deep Copy

By default, copying an object duplicates each member field. For primitive types this works fine. But when a class manages a resource (dynamic memory, file handle), a **shallow copy** is dangerous — two objects end up sharing the same resource.

A **deep copy** duplicates the resource itself, so each object is fully independent.

### Implementing Copy

In C++, copy behavior is defined by the **copy constructor** and **copy assignment operator**. In our environment we use a \`copy()\` method that returns a new independent object with the same data:

\`\`\`cpp
class Buffer {
public:
    string name;
    int size;

    Buffer(string n, int s) {
        name = n;
        size = s;
    }

    Buffer copy() {
        cout << "Copied " << name << endl;
        return Buffer(name, size);
    }
};

Buffer b1("data", 100);
Buffer b2 = b1.copy();  // Deep copy
\`\`\`

### The Rule of Three

If your class needs a custom:
1. **Destructor** — to release resources
2. **Copy constructor** — to deep-copy resources
3. **Copy assignment operator** — to deep-copy on assignment

Then you almost certainly need **all three**. This is the **Rule of Three**.

### Move Semantics (C++11)

C++11 introduced **move semantics** to transfer ownership instead of copying. A move is much cheaper — it "steals" the internal data from the source and leaves it in an empty state:

\`\`\`cpp
class Buffer {
public:
    string name;
    int size;

    void moveTo(Buffer target) {
        target.name = name;
        target.size = size;
        name = "";    // Source is emptied
        size = 0;
        cout << "Moved to " << target.name << endl;
    }
};
\`\`\`

With a move, the source object is left in a valid but empty state. This avoids expensive deep copies when transferring resources.

### Rule of Five

With move semantics, the Rule of Three extends to the **Rule of Five**: destructor, copy constructor, copy assignment, **move constructor**, and **move assignment operator**.

### Your Task

Create an \`Inventory\` class that:
- Has \`public string name\` and \`public int count\`
- Constructor takes a name and count, stores them, and prints \`"Created <name>(<count>)"\`
- \`copy()\` method returns a new \`Inventory\` with the same name and count, and prints \`"Copied <name>(<count>)"\`
- \`show()\` method prints \`"<name>: <count> items"\`
- Destructor prints \`"Destroyed <name>"\`

Create an inventory, copy it, show both, then destroy both (copy first).`,

	starterCode: `#include <iostream>
using namespace std;

// Inventory class with:
// - string name, int count
// - Constructor: prints "Created <name>(<count>)"
// - copy(): returns new Inventory, prints "Copied <name>(<count>)"
// - show(): prints "<name>: <count> items"
// - Destructor: prints "Destroyed <name>"

int main() {
	Inventory a("potions", 5);
	Inventory b = a.copy();
	a.show();
	b.show();
	b.__destroy();
	a.__destroy();
	return 0;
}
`,

	solution: `#include <iostream>
using namespace std;

class Inventory {
public:
	string name;
	int count;

	Inventory(string n, int c) {
		this->name = n;
		this->count = c;
		cout << "Created " << this->name << "(" << this->count << ")" << endl;
	}

	Inventory copy() {
		cout << "Copied " << this->name << "(" << this->count << ")" << endl;
		return Inventory(this->name, this->count);
	}

	void show() {
		cout << this->name << ": " << this->count << " items" << endl;
	}

	~Inventory() {
		cout << "Destroyed " << this->name << endl;
	}
};

int main() {
	Inventory a("potions", 5);
	Inventory b = a.copy();
	a.show();
	b.show();
	b.__destroy();
	a.__destroy();
	return 0;
}
`,

	tests: [
		{
			name: "create, copy, show, and destroy inventory",
			expected:
				"Created potions(5)\nCopied potions(5)\nCreated potions(5)\npotions: 5 items\npotions: 5 items\nDestroyed potions\nDestroyed potions\n",
		},
		{
			name: "copy is independent from original",
			expected:
				"Created arrows(20)\nCopied arrows(20)\nCreated arrows(20)\narrows: 20 items\narrows: 99 items\nDestroyed arrows\nDestroyed arrows\n",
			code: `#include <iostream>
using namespace std;
class Inventory {
public:
	string name;
	int count;
	Inventory(string n, int c) {
		this->name = n;
		this->count = c;
		cout << "Created " << this->name << "(" << this->count << ")" << endl;
	}
	Inventory copy() {
		cout << "Copied " << this->name << "(" << this->count << ")" << endl;
		return Inventory(this->name, this->count);
	}
	void show() {
		cout << this->name << ": " << this->count << " items" << endl;
	}
	~Inventory() {
		cout << "Destroyed " << this->name << endl;
	}
};
int main() {
	Inventory a("arrows", 20);
	Inventory b = a.copy();
	a.show();
	b.count = 99;
	b.show();
	b.__destroy();
	a.__destroy();
	return 0;
}
`,
		},
		{
			name: "multiple copies from same source",
			expected:
				"Created gems(10)\nCopied gems(10)\nCreated gems(10)\nCopied gems(10)\nCreated gems(10)\ngems: 10 items\ngems: 10 items\ngems: 10 items\nDestroyed gems\nDestroyed gems\nDestroyed gems\n",
			code: `#include <iostream>
using namespace std;
class Inventory {
public:
	string name;
	int count;
	Inventory(string n, int c) {
		this->name = n;
		this->count = c;
		cout << "Created " << this->name << "(" << this->count << ")" << endl;
	}
	Inventory copy() {
		cout << "Copied " << this->name << "(" << this->count << ")" << endl;
		return Inventory(this->name, this->count);
	}
	void show() {
		cout << this->name << ": " << this->count << " items" << endl;
	}
	~Inventory() {
		cout << "Destroyed " << this->name << endl;
	}
};
int main() {
	Inventory a("gems", 10);
	Inventory b = a.copy();
	Inventory c = a.copy();
	a.show();
	b.show();
	c.show();
	c.__destroy();
	b.__destroy();
	a.__destroy();
	return 0;
}
`,
		},
	],
};
