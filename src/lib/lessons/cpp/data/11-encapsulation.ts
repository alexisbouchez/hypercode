import type { Lesson } from "../../types";

export const encapsulation: Lesson = {
	id: "encapsulation",
	title: "Encapsulation",
	chapterId: "classes",
	content: `## Encapsulation

**Encapsulation** means hiding internal data behind a public interface. You control what the outside world can see and how it can interact with your class.

### public and private

\`\`\`cpp
class BankAccount {
private:
    double balance;   // hidden — only the class can access this

public:
    BankAccount(double initial) {
        balance = initial;
    }

    void deposit(double amount) {
        if (amount > 0) balance += amount;
    }

    double getBalance() {
        return balance;
    }
};
\`\`\`

From outside:
\`\`\`cpp
BankAccount account(1000);
account.deposit(500);
cout << account.getBalance() << endl;  // 1500

// account.balance = -1000;  // error: private
\`\`\`

### Why Encapsulation?

- **Safety** — invalid states are impossible (negative balance can't be set directly)
- **Flexibility** — you can change the internal implementation without breaking callers
- **Clarity** — the public interface documents what the class can do

### Getters and Setters

**Getters** return private data; **setters** validate and set it:

\`\`\`cpp
class Temperature {
private:
    double celsius;

public:
    Temperature(double c) : celsius(c) {}

    double getCelsius() { return celsius; }

    double getFahrenheit() { return celsius * 9.0 / 5.0 + 32; }

    void setCelsius(double c) {
        if (c >= -273.15) celsius = c;  // validate before setting
    }
};
\`\`\`

### Default Access

In a \`class\`, members are \`private\` by default. In a \`struct\`, they are \`public\` by default.

\`\`\`cpp
class Foo {
    int x;  // private — inaccessible from outside
public:
    int y;  // public — accessible
};
\`\`\`

### Your Task

Create a \`Counter\` class with:
- \`private int count\` and \`private int step\`
- Constructor that takes a step (default 1)
- \`increment()\` — adds step to count
- \`reset()\` — sets count to 0
- \`getCount()\` — returns count

Create two counters, increment them, and print their values.`,

	starterCode: `#include <iostream>
using namespace std;

// Counter class — private count and step, public methods

int main() {
	Counter c1;      // step = 1
	Counter c2(5);   // step = 5

	c1.increment();
	c1.increment();
	c1.increment();

	c2.increment();
	c2.increment();

	cout << "c1: " << c1.getCount() << endl;
	cout << "c2: " << c2.getCount() << endl;

	c1.reset();
	cout << "c1 after reset: " << c1.getCount() << endl;
	return 0;
}
`,

	solution: `#include <iostream>
using namespace std;

class Counter {
private:
	int count;
	int step;

public:
	Counter(int s = 1) {
		this->count = 0;
		this->step = s;
	}

	void increment() {
		this->count += this->step;
	}

	void reset() {
		this->count = 0;
	}

	int getCount() {
		return this->count;
	}
};

int main() {
	Counter c1;
	Counter c2(5);

	c1.increment();
	c1.increment();
	c1.increment();

	c2.increment();
	c2.increment();

	cout << "c1: " << c1.getCount() << endl;
	cout << "c2: " << c2.getCount() << endl;

	c1.reset();
	cout << "c1 after reset: " << c1.getCount() << endl;
	return 0;
}
`,

	tests: [
		{
			name: "counter increments with step and resets",
			expected: "c1: 3\nc2: 10\nc1 after reset: 0\n",
		},
	],
};
