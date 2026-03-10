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

### Default Parameters

Instead of writing multiple constructors, you can use **default parameter values** to handle both cases with a single constructor:

\`\`\`cpp
class Point {
public:
    int x, y;

    Point(int px = 0, int py = 0) {
        x = px;
        y = py;
        if (px == 0 && py == 0) {
            cout << "Point at origin" << endl;
        } else {
            cout << "Point at (" << px << ", " << py << ")" << endl;
        }
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

Create a \`Point\` class with a constructor that uses **default parameters** (defaulting to 0). If both values are 0, print \`"Point at origin"\`; otherwise print \`"Point at (x, y)"\`. Add a \`print()\` method that prints \`"(x, y)"\`.`,

	starterCode: `#include <iostream>
using namespace std;

// Point class with:
// - int x, int y
// - Point(int px = 0, int py = 0): uses default parameters
//   If both 0: prints "Point at origin", else prints "Point at (x, y)"
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
		{
			name: "point with negative coordinates",
			expected: "Point at (-2, -5)\n(-2, -5)\n",
			code: `#include <iostream>
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
	Point p(-2, -5);
	p.print();
	return 0;
}
`,
		},
		{
			name: "multiple points created independently",
			expected: "Point at (1, 2)\nPoint at (10, 20)\n(1, 2)\n(10, 20)\n",
			code: `#include <iostream>
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
	Point a(1, 2);
	Point b(10, 20);
	a.print();
	b.print();
	return 0;
}
`,
		},
	],
};
