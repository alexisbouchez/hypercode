import type { Lesson } from "../../types";

export const classes: Lesson = {
	id: "classes",
	title: "Classes and Objects",
	chapterId: "classes",
	content: `## Classes and Objects

A **class** groups related data (**member variables**) and behavior (**member functions**) into a single unit. An **object** is an instance of a class.

### Defining a Class

\`\`\`cpp
class Rectangle {
public:
    int width;
    int height;

    int area() {
        return width * height;
    }

    int perimeter() {
        return 2 * (width + height);
    }
};
\`\`\`

- \`class\` — the keyword (vs C's \`struct\`)
- \`public:\` — these members are accessible from outside the class
- \`int area()\` — a member function that can access \`width\` and \`height\` directly

### Creating Objects

\`\`\`cpp
Rectangle r;        // create an object
r.width = 10;       // set member variables
r.height = 5;
cout << r.area();   // call a member function → 50
\`\`\`

### Multiple Objects

Each object has its own copy of the member variables:

\`\`\`cpp
Rectangle a, b;
a.width = 3;  a.height = 4;
b.width = 6;  b.height = 2;

cout << a.area() << endl;  // 12
cout << b.area() << endl;  // 12
\`\`\`

### Member Functions Have Access to Members

Inside a member function, you can access any member variable or function of the same class:

\`\`\`cpp
class Circle {
public:
    double radius;

    double area() {
        return 3.14159 * radius * radius;
    }

    bool isLargerThan(Circle other) {
        return radius > other.radius;
    }
};
\`\`\`

### Class vs struct

In C++, \`class\` and \`struct\` are nearly identical. The only difference:
- \`struct\` members are \`public\` by default
- \`class\` members are \`private\` by default

### Your Task

Define a \`Rectangle\` class with \`width\` and \`height\` (both \`int\`), plus \`area()\` and \`perimeter()\` methods. Create a rectangle with width 4 and height 3 and print its area and perimeter.`,

	starterCode: `#include <iostream>
using namespace std;

// Define a Rectangle class with:
// - int width, int height
// - int area() — returns width * height
// - int perimeter() — returns 2 * (width + height)

int main() {
	Rectangle r;
	r.width = 4;
	r.height = 3;
	cout << "Area: " << r.area() << endl;
	cout << "Perimeter: " << r.perimeter() << endl;
	return 0;
}
`,

	solution: `#include <iostream>
using namespace std;

class Rectangle {
public:
	int width;
	int height;

	int area() {
		return this->width * this->height;
	}

	int perimeter() {
		return 2 * (this->width + this->height);
	}
};

int main() {
	Rectangle r;
	r.width = 4;
	r.height = 3;
	cout << "Area: " << r.area() << endl;
	cout << "Perimeter: " << r.perimeter() << endl;
	return 0;
}
`,

	tests: [
		{
			name: "computes area and perimeter",
			expected: "Area: 12\nPerimeter: 14\n",
		},
	],
};
