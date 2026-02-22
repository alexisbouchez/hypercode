import type { Lesson } from "../../types";

export const classes: Lesson = {
	id: "classes",
	title: "Classes & Objects",
	chapterId: "oop",
	content: `## Classes and Objects

A **class** is a blueprint; an **object** is an instance of that blueprint.

\`\`\`java
class Dog {
    String name;      // field
    int age;

    Dog(String name, int age) {  // constructor
        this.name = name;
        this.age = age;
    }

    String bark() {
        return name + " says: Woof!";
    }

    @Override
    public String toString() {
        return "Dog(" + name + ", age=" + age + ")";
    }
}

Dog d = new Dog("Rex", 3);
System.out.println(d);        // Dog(Rex, age=3)
System.out.println(d.bark()); // Rex says: Woof!
\`\`\`

### Encapsulation

Use \`private\` to hide fields and expose them through methods:

\`\`\`java
private double balance;
void deposit(double amount) { balance += amount; }
double getBalance() { return balance; }
\`\`\`

### Your Task

Create a \`BankAccount\` class (as a static inner class) with:
- \`private String owner\`, \`private double balance\`
- Constructor \`BankAccount(String owner, double initialBalance)\`
- \`void deposit(double amount)\`
- \`boolean withdraw(double amount)\` — returns \`false\` if insufficient funds, otherwise deducts and returns \`true\`
- \`toString()\` returning \`"<owner>: $<balance>"\`

Then run the sequence shown in the expected output.`,

	starterCode: `public class Main {
    static class BankAccount {
        private String owner;
        private double balance;

        BankAccount(String owner, double initialBalance) {
            this.owner = owner;
            this.balance = initialBalance;
        }

        void deposit(double amount) {
            // add amount to balance
        }

        boolean withdraw(double amount) {
            // return false if insufficient, else deduct and return true
            return false;
        }

        @Override
        public String toString() {
            return owner + ": $" + balance;
        }
    }

    public static void main(String[] args) {
        BankAccount acc = new BankAccount("Alice", 1000.0);
        System.out.println(acc);
        acc.deposit(500.0);
        System.out.println(acc);
        System.out.println(acc.withdraw(200.0));
        System.out.println(acc);
    }
}
`,

	solution: `public class Main {
    static class BankAccount {
        private String owner;
        private double balance;

        BankAccount(String owner, double initialBalance) {
            this.owner = owner;
            this.balance = initialBalance;
        }

        void deposit(double amount) {
            balance += amount;
        }

        boolean withdraw(double amount) {
            if (amount > balance) return false;
            balance -= amount;
            return true;
        }

        @Override
        public String toString() {
            return owner + ": $" + balance;
        }
    }

    public static void main(String[] args) {
        BankAccount acc = new BankAccount("Alice", 1000.0);
        System.out.println(acc);
        acc.deposit(500.0);
        System.out.println(acc);
        System.out.println(acc.withdraw(200.0));
        System.out.println(acc);
    }
}
`,

	tests: [
		{
			name: "deposit, withdraw, toString",
			expected:
				"Alice: $1000.0\nAlice: $1500.0\ntrue\nAlice: $1300.0\n",
		},
	],
};
