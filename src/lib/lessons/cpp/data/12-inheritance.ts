import type { Lesson } from "../../types";

export const inheritance: Lesson = {
	id: "inheritance",
	title: "Inheritance",
	chapterId: "inheritance",
	content: `## Inheritance

**Inheritance** lets a class (**derived class**) reuse and extend the behavior of another class (**base class**). It models an "is-a" relationship.

### Syntax

\`\`\`cpp
class Animal {
public:
    string name;

    Animal(string n) : name(n) {}

    void breathe() {
        cout << name << " breathes" << endl;
    }
};

class Dog : public Animal {
public:
    Dog(string n) : Animal(n) {}  // call base constructor

    void bark() {
        cout << name << " barks: Woof!" << endl;
    }
};
\`\`\`

\`Dog\` inherits \`name\` and \`breathe()\` from \`Animal\`, and adds \`bark()\`.

\`\`\`cpp
Dog d("Rex");
d.breathe();  // Rex breathes
d.bark();     // Rex barks: Woof!
\`\`\`

### Calling the Base Constructor

The derived class constructor must call the base constructor in the **initializer list**:

\`\`\`cpp
class Cat : public Animal {
public:
    Cat(string n) : Animal(n) {}  // passes n to Animal's constructor
};
\`\`\`

### Overriding Methods

A derived class can provide its own version of an inherited method:

\`\`\`cpp
class Animal {
public:
    void speak() {
        cout << "..." << endl;
    }
};

class Dog : public Animal {
public:
    void speak() {  // hides Animal::speak
        cout << "Woof!" << endl;
    }
};
\`\`\`

### Multiple Levels

Inheritance can be chained:

\`\`\`cpp
class LivingThing { ... };
class Animal : public LivingThing { ... };
class Dog : public Animal { ... };
\`\`\`

\`Dog\` inherits from both \`Animal\` and \`LivingThing\`.

### Access Control in Inheritance

With \`public\` inheritance:
- \`public\` members of the base remain \`public\`
- \`private\` members of the base are inaccessible in derived
- Use \`protected\` for members that derived classes can access but outsiders cannot

### Your Task

Create:
- An \`Animal\` base class with a \`name\` and a \`breathe()\` method
- A \`Dog\` class that inherits Animal and adds \`bark()\`
- A \`Cat\` class that inherits Animal and adds \`meow()\`

Create one dog and one cat, call their methods.`,

	starterCode: `#include <iostream>
#include <string>
using namespace std;

// Base class: Animal with string name, constructor, breathe()
// class Dog : public Animal — adds bark()
// class Cat : public Animal — adds meow()

int main() {
	Dog dog("Rex");
	Cat cat("Luna");

	dog.breathe();
	dog.bark();
	cat.breathe();
	cat.meow();
	return 0;
}
`,

	solution: `#include <iostream>
#include <string>
using namespace std;

class Animal {
public:
	string name;

	Animal(string n) {
		this->name = n;
	}

	void breathe() {
		cout << this->name << " breathes" << endl;
	}
};

class Dog : public Animal {
public:
	Dog(string n) : Animal(n) {}

	void bark() {
		cout << this->name << " barks: Woof!" << endl;
	}
};

class Cat : public Animal {
public:
	Cat(string n) : Animal(n) {}

	void meow() {
		cout << this->name << " meows: Meow!" << endl;
	}
};

int main() {
	Dog dog("Rex");
	Cat cat("Luna");

	dog.breathe();
	dog.bark();
	cat.breathe();
	cat.meow();
	return 0;
}
`,

	tests: [
		{
			name: "dog and cat inherit and extend Animal",
			expected: "Rex breathes\nRex barks: Woof!\nLuna breathes\nLuna meows: Meow!\n",
		},
	],
};
