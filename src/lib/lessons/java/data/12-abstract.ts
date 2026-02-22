import type { Lesson } from "../../types";

export const abstractClasses: Lesson = {
	id: "abstract",
	title: "Abstract Classes",
	chapterId: "oop",
	content: `## Abstract Classes

An **abstract class** can have both concrete and abstract methods. You cannot instantiate it directly — only subclasses:

\`\`\`java
abstract class Shape {
    abstract double area();   // subclasses must implement this

    void describe() {         // concrete method shared by all
        System.out.println("Area: " + area());
    }
}

class Circle extends Shape {
    double radius;
    Circle(double r) { this.radius = r; }

    @Override
    double area() { return Math.PI * radius * radius; }
}
\`\`\`

### When to Use Abstract Classes vs Interfaces

- Use an **interface** when you only need a contract (method signatures)
- Use an **abstract class** when you want shared state (fields) and partial implementation

\`\`\`java
Shape s = new Circle(5);
s.describe();  // Area: 78.53981633974483
\`\`\`

### Your Task

Create an abstract class \`Employee\` with:
- \`protected String name\`, \`protected double hourlyRate\`
- Constructor \`Employee(String name, double hourlyRate)\`
- Abstract \`double hoursWorked()\`
- Concrete \`double pay()\` returning \`hourlyRate * hoursWorked()\`
- \`toString()\` returning \`"<name> earns $<pay>"\`

Subclasses:
- \`FullTime\`: \`hoursWorked()\` returns \`40\`
- \`PartTime(double hours)\`: \`hoursWorked()\` returns the given hours

Print Alice (FullTime, $25/hr) and Bob (PartTime, $20/hr, 20 hrs).`,

	starterCode: `public class Main {
    static abstract class Employee {
        protected String name;
        protected double hourlyRate;

        Employee(String name, double hourlyRate) {
            this.name = name;
            this.hourlyRate = hourlyRate;
        }

        abstract double hoursWorked();

        double pay() {
            return hourlyRate * hoursWorked();
        }

        @Override
        public String toString() {
            return name + " earns $" + pay();
        }
    }

    static class FullTime extends Employee {
        FullTime(String name, double hourlyRate) {
            super(name, hourlyRate);
        }

        @Override
        double hoursWorked() {
            // return 40
            return 0;
        }
    }

    static class PartTime extends Employee {
        private double hours;

        PartTime(String name, double hourlyRate, double hours) {
            super(name, hourlyRate);
            this.hours = hours;
        }

        @Override
        double hoursWorked() {
            // return hours
            return 0;
        }
    }

    public static void main(String[] args) {
        Employee[] staff = {
            new FullTime("Alice", 25.0),
            new PartTime("Bob", 20.0, 20.0)
        };
        for (Employee e : staff) {
            System.out.println(e);
        }
    }
}
`,

	solution: `public class Main {
    static abstract class Employee {
        protected String name;
        protected double hourlyRate;

        Employee(String name, double hourlyRate) {
            this.name = name;
            this.hourlyRate = hourlyRate;
        }

        abstract double hoursWorked();

        double pay() {
            return hourlyRate * hoursWorked();
        }

        @Override
        public String toString() {
            return name + " earns $" + pay();
        }
    }

    static class FullTime extends Employee {
        FullTime(String name, double hourlyRate) {
            super(name, hourlyRate);
        }

        @Override
        double hoursWorked() {
            return 40;
        }
    }

    static class PartTime extends Employee {
        private double hours;

        PartTime(String name, double hourlyRate, double hours) {
            super(name, hourlyRate);
            this.hours = hours;
        }

        @Override
        double hoursWorked() {
            return hours;
        }
    }

    public static void main(String[] args) {
        Employee[] staff = {
            new FullTime("Alice", 25.0),
            new PartTime("Bob", 20.0, 20.0)
        };
        for (Employee e : staff) {
            System.out.println(e);
        }
    }
}
`,

	tests: [
		{
			name: "Alice $1000, Bob $400",
			expected: "Alice earns $1000.0\nBob earns $400.0\n",
		},
	],
};
