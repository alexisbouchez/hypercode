import type { Lesson } from "../../types";

export const virtualFunctions: Lesson = {
	id: "virtual-functions",
	title: "Virtual Functions",
	chapterId: "inheritance",
	content: `## Virtual Functions and Polymorphism

**Polymorphism** means "many forms" — the same call can behave differently depending on the actual type of the object. Virtual functions make this possible.

### The Problem Without virtual

\`\`\`cpp
class Shape {
public:
    string type() { return "shape"; }
};

class Circle : public Shape {
public:
    string type() { return "circle"; }
};

void describe(Shape& s) {
    cout << s.type() << endl;
}

Circle c;
describe(c);  // prints "shape" — wrong!
\`\`\`

Without \`virtual\`, the compiler picks the method at **compile time** based on the declared type (\`Shape&\`), not the actual type (\`Circle\`).

### The Fix: virtual

Mark the base class method as \`virtual\`:

\`\`\`cpp
class Shape {
public:
    virtual string type() { return "shape"; }
};

class Circle : public Shape {
public:
    string type() override { return "circle"; }
};

Circle c;
describe(c);  // prints "circle" — correct!
\`\`\`

Now the method is selected at **runtime** based on the actual object type. This is **runtime polymorphism**.

### override Keyword

\`override\` tells the compiler you intend to override a virtual function. It catches mistakes:

\`\`\`cpp
class Circle : public Shape {
    string tipe() override { ... }  // error: no virtual tipe() in Shape
};
\`\`\`

### Pure Virtual Functions and Abstract Classes

A **pure virtual** function has no implementation in the base class — derived classes must provide one:

\`\`\`cpp
class Shape {
public:
    virtual int area() = 0;  // pure virtual
};

class Square : public Shape {
public:
    int side;
    Square(int s) : side(s) {}
    int area() override { return side * side; }
};

// Shape s;  // error: cannot instantiate abstract class
\`\`\`

### Your Task

Create a \`Shape\` base class with \`virtual string type()\` and \`virtual int area()\`.
Create \`Rectangle\` (width, height) and \`Square\` (side) that override both methods.
Write a \`printInfo(Shape& s)\` function and call it with each shape.`,

	starterCode: `#include <iostream>
#include <string>
using namespace std;

// Shape base class: virtual type() and virtual area()
// Rectangle(int w, int h): type = "rectangle", area = w*h
// Square(int s): type = "square", area = s*s
// void printInfo(Shape& s): prints "type: area = N"

int main() {
	Rectangle r(4, 3);
	Square sq(5);

	printInfo(r);
	printInfo(sq);
	return 0;
}
`,

	solution: `#include <iostream>
#include <string>
using namespace std;

class Shape {
public:
	virtual string type() {
		return "shape";
	}

	virtual int area() {
		return 0;
	}
};

class Rectangle : public Shape {
public:
	int w;
	int h;

	Rectangle(int rw, int rh) {
		this->w = rw;
		this->h = rh;
	}

	string type() override {
		return "rectangle";
	}

	int area() override {
		return this->w * this->h;
	}
};

class Square : public Shape {
public:
	int side;

	Square(int s) {
		this->side = s;
	}

	string type() override {
		return "square";
	}

	int area() override {
		return this->side * this->side;
	}
};

void printInfo(Shape& s) {
	cout << s.type() << ": area = " << s.area() << endl;
}

int main() {
	Rectangle r(4, 3);
	Square sq(5);

	printInfo(r);
	printInfo(sq);
	return 0;
}
`,

	tests: [
		{
			name: "virtual dispatch calls correct overrides",
			expected: "rectangle: area = 12\nsquare: area = 25\n",
		},
	],
};
