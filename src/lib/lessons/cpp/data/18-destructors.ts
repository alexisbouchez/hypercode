import type { Lesson } from "../../types";

export const destructors: Lesson = {
	id: "destructors",
	title: "Destructors",
	chapterId: "classes",
	content: `## Destructors

A **destructor** is a special member function that runs when an object is destroyed — when it goes out of scope or is explicitly deleted. Its name is the class name prefixed with \`~\`.

### Syntax

\`\`\`cpp
class File {
private:
    string name;

public:
    File(string n) {
        name = n;
        cout << "Opened " << name << endl;
    }

    ~File() {
        cout << "Closed " << name << endl;
    }
};
\`\`\`

When a \`File\` object goes out of scope, the destructor runs automatically:
\`\`\`cpp
{
    File f("data.txt");
}  // "Closed data.txt" printed here
\`\`\`

### RAII — Resource Acquisition Is Initialization

C++ uses the **RAII** pattern: acquire resources in the constructor, release them in the destructor. This guarantees cleanup even if an exception occurs.

\`\`\`cpp
class Connection {
public:
    Connection()  { cout << "Connected" << endl; }
    ~Connection() { cout << "Disconnected" << endl; }
};
\`\`\`

### Destruction Order

Objects are destroyed in **reverse order** of their construction:

\`\`\`cpp
File a("first");
File b("second");
// destructor order: b, then a
\`\`\`

### Rules

- A destructor takes **no parameters** and has **no return type**
- Each class has exactly **one destructor**
- If you don't write one, the compiler generates a default that does nothing special
- Always write a destructor if your class manages a resource (memory, file handle, network socket)

### Your Task

Create a \`Logger\` class that:
- Has a \`private string name\`
- Constructor takes a name, stores it, and prints \`"Logger <name> created"\`
- Destructor prints \`"Logger <name> destroyed"\`
- Has a \`log(string msg)\` method that prints \`"[<name>] <msg>"\`

Create two loggers, log a message with each, then call their destructors in reverse order.`,

	starterCode: `#include <iostream>
using namespace std;

// Logger class with constructor, destructor, and log method

int main() {
	Logger app("app");
	Logger db("db");

	app.log("started");
	db.log("connected");

	// Destructors run in reverse order of construction
	// (In real C++, this happens automatically at end of scope)
	db.__destroy();
	app.__destroy();
	return 0;
}
`,

	solution: `#include <iostream>
using namespace std;

class Logger {
private:
	string name;

public:
	Logger(string n) {
		this->name = n;
		cout << "Logger " << this->name << " created" << endl;
	}

	~Logger() {
		cout << "Logger " << this->name << " destroyed" << endl;
	}

	void log(string msg) {
		cout << "[" << this->name << "] " << msg << endl;
	}
};

int main() {
	Logger app("app");
	Logger db("db");

	app.log("started");
	db.log("connected");

	db.__destroy();
	app.__destroy();
	return 0;
}
`,

	tests: [
		{
			name: "loggers create, log, and destroy in order",
			expected:
				"Logger app created\nLogger db created\n[app] started\n[db] connected\nLogger db destroyed\nLogger app destroyed\n",
		},
		{
			name: "single logger lifecycle",
			expected: "Logger test created\n[test] hello\nLogger test destroyed\n",
			code: `#include <iostream>
using namespace std;
class Logger {
private:
	string name;
public:
	Logger(string n) {
		this->name = n;
		cout << "Logger " << this->name << " created" << endl;
	}
	~Logger() {
		cout << "Logger " << this->name << " destroyed" << endl;
	}
	void log(string msg) {
		cout << "[" << this->name << "] " << msg << endl;
	}
};
int main() {
	Logger l("test");
	l.log("hello");
	l.__destroy();
	return 0;
}
`,
		},
		{
			name: "three loggers destroyed in reverse order",
			expected:
				"Logger a created\nLogger b created\nLogger c created\nLogger c destroyed\nLogger b destroyed\nLogger a destroyed\n",
			code: `#include <iostream>
using namespace std;
class Logger {
private:
	string name;
public:
	Logger(string n) {
		this->name = n;
		cout << "Logger " << this->name << " created" << endl;
	}
	~Logger() {
		cout << "Logger " << this->name << " destroyed" << endl;
	}
	void log(string msg) {
		cout << "[" << this->name << "] " << msg << endl;
	}
};
int main() {
	Logger a("a");
	Logger b("b");
	Logger c("c");
	c.__destroy();
	b.__destroy();
	a.__destroy();
	return 0;
}
`,
		},
	],
};
