import type { Lesson } from "../../types";

export const constructors: Lesson = {
	id: "constructors",
	title: "Constructors",
	chapterId: "classes",
	content: `## Constructors

A **constructor** is a special member function that runs automatically when an object is created. It has the same name as the class and no return type.

### Default Constructor

A constructor with no parameters:

\`\`\`cpp
class Counter {
public:
    int count;

    Counter() {
        count = 0;
        cout << "Counter created" << endl;
    }
};

Counter c;  // "Counter created" is printed
\`\`\`

### Parameterized Constructor

A constructor that takes arguments lets you initialize the object with specific values:

\`\`\`cpp
class Point {
public:
    int x;
    int y;

    Point(int x, int y) {
        this->x = x;  // this-> distinguishes parameter from member
        this->y = y;
    }
};

Point p(3, 4);
cout << p.x << " " << p.y << endl;  // 3 4
\`\`\`

### this Pointer

Inside a member function, \`this\` is a pointer to the current object. It's useful when parameter names shadow member names.

### Multiple Constructors

You can overload constructors like any other function:

\`\`\`cpp
class Point {
public:
    int x, y;

    Point() {              // default
        x = 0; y = 0;
        cout << "Point at origin" << endl;
    }

    Point(int x, int y) { // parameterized
        this->x = x;
        this->y = y;
        cout << "Point at (" << x << ", " << y << ")" << endl;
    }

    void print() {
        cout << "(" << x << ", " << y << ")" << endl;
    }
};

Point p1;       // Point at origin
Point p2(3, 4); // Point at (3, 4)
p1.print();     // (0, 0)
p2.print();     // (3, 4)
\`\`\`

### Member Initializer Lists

A cleaner way to initialize members — runs before the constructor body:

\`\`\`cpp
class Point {
public:
    int x, y;
    Point(int x, int y) : x(x), y(y) {}
};
\`\`\`

### Your Task

Create a \`Point\` class with two constructors (default and parameterized) and a \`print()\` method. Create both types of points and print them.`,

	starterCode: `#include <iostream>
using namespace std;

// Point class with:
// - int x, int y
// - Default constructor: sets x=0, y=0, prints "Point at origin"
// - Point(int x, int y): sets members, prints "Point at (x, y)"
// - void print(): prints "(x, y)"

int main() {
	Point p1;
	Point p2(3, 4);
	p1.print();
	p2.print();
	return 0;
}
`,

	solution: `#include <iostream>
using namespace std;

class Point {
public:
	int x;
	int y;

	Point(int px = 0, int py = 0) {
		this->x = px;
		this->y = py;
		if (px == 0 && py == 0) {
			cout << "Point at origin" << endl;
		} else {
			cout << "Point at (" << px << ", " << py << ")" << endl;
		}
	}

	void print() {
		cout << "(" << this->x << ", " << this->y << ")" << endl;
	}
};

int main() {
	Point p1;
	Point p2(3, 4);
	p1.print();
	p2.print();
	return 0;
}
`,

	tests: [
		{
			name: "constructs and prints points",
			expected: "Point at origin\nPoint at (3, 4)\n(0, 0)\n(3, 4)\n",
		},
	],
};
