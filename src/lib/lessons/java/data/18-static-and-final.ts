import type { Lesson } from "../../types";

export const staticAndFinal: Lesson = {
	id: "static-and-final",
	title: "Static & Final",
	chapterId: "oop",
	content: `## Static & Final Keywords

### Static Fields and Methods

A \`static\` member belongs to the **class** rather than any instance. You access it via the class name:

\`\`\`java
class Counter {
    static int count = 0;

    Counter() { count++; }

    static int getCount() { return count; }
}

new Counter();
new Counter();
System.out.println(Counter.getCount()); // 2
\`\`\`

Static methods cannot access instance fields or use \`this\`.

### Final Variables

A \`final\` variable can only be assigned **once**. It acts as a constant:

\`\`\`java
final int MAX = 100;
// MAX = 200;  // compile error!

final String name;
name = "Alice";   // OK — first assignment
// name = "Bob";  // compile error — already assigned
\`\`\`

### Final Methods and Classes

- A \`final\` method **cannot be overridden** in a subclass.
- A \`final\` class **cannot be extended** at all (e.g. \`String\` is final).

\`\`\`java
class Base {
    final void greet() {
        System.out.println("Hello");
    }
}
\`\`\`

### Static Factory Pattern

Instead of public constructors, use static factory methods for more readable, controlled object creation:

\`\`\`java
class Color {
    private final int r, g, b;

    private Color(int r, int g, int b) {
        this.r = r; this.g = g; this.b = b;
    }

    static Color of(int r, int g, int b) {
        return new Color(r, g, b);
    }

    static Color red()   { return new Color(255, 0, 0); }
    static Color green() { return new Color(0, 255, 0); }
}
\`\`\`

### Your Task

1. Create a class \`IdGenerator\` with:
   - A \`private static int nextId\` starting at 0
   - A \`static int generate()\` method that increments and returns \`nextId\`
   - A \`static void reset()\` method that sets \`nextId\` back to 0

2. Create a class \`Temperature\` with:
   - A \`private final double value\` and \`private final String unit\` field
   - A private constructor
   - Static factory methods \`celsius(double v)\` and \`fahrenheit(double v)\`
   - A \`toString()\` method returning e.g. \`"36.6 C"\` or \`"98.6 F"\`
   - A \`toFahrenheit()\` method that returns a new \`Temperature\` in Fahrenheit (if already F, return itself). Formula: \`value * 9 / 5 + 32\`

3. In \`main\`, generate 3 IDs (print each), reset, generate 1 more (print it). Then create a Celsius temperature of 100.0 and a Fahrenheit temperature of 98.6, print both, and convert the Celsius one to Fahrenheit and print it.`,

	starterCode: `public class Main {
    static class IdGenerator {
        // private static int nextId = 0;
        // static int generate() — increment and return
        // static void reset() — set back to 0
    }

    static class Temperature {
        // private final double value;
        // private final String unit;
        // private constructor
        // static Temperature celsius(double v)
        // static Temperature fahrenheit(double v)
        // String toString()
        // Temperature toFahrenheit()

        public String toString() { return ""; }
    }

    public static void main(String[] args) {
        // Generate 3 IDs, print each
        // Reset, generate 1 more, print it
        // Create Temperature.celsius(100.0) and Temperature.fahrenheit(98.6)
        // Print both, then print celsius.toFahrenheit()
    }
}
`,

	solution: `public class Main {
    static class IdGenerator {
        private static int nextId = 0;

        static int generate() {
            return ++nextId;
        }

        static void reset() {
            nextId = 0;
        }
    }

    static class Temperature {
        private final double value;
        private final String unit;

        private Temperature(double value, String unit) {
            this.value = value;
            this.unit = unit;
        }

        static Temperature celsius(double v) {
            return new Temperature(v, "C");
        }

        static Temperature fahrenheit(double v) {
            return new Temperature(v, "F");
        }

        Temperature toFahrenheit() {
            if (unit.equals("F")) return this;
            return new Temperature(value * 9 / 5 + 32, "F");
        }

        public String toString() {
            return value + " " + unit;
        }
    }

    public static void main(String[] args) {
        System.out.println(IdGenerator.generate());
        System.out.println(IdGenerator.generate());
        System.out.println(IdGenerator.generate());
        IdGenerator.reset();
        System.out.println(IdGenerator.generate());

        Temperature c = Temperature.celsius(100.0);
        Temperature f = Temperature.fahrenheit(98.6);
        System.out.println(c);
        System.out.println(f);
        System.out.println(c.toFahrenheit());
    }
}
`,

	tests: [
		{
			name: "id generator and temperature",
			expected:
				"1\n2\n3\n1\n100.0 C\n98.6 F\n212.0 F\n",
		},
		{
			name: "id generator reset behavior",
			expected: "1\n2\n1\n2\n3\n",
			code: `public class Main {
    static class IdGenerator {
        private static int nextId = 0;
        static int generate() { return ++nextId; }
        static void reset() { nextId = 0; }
    }
    public static void main(String[] args) {
        System.out.println(IdGenerator.generate());
        System.out.println(IdGenerator.generate());
        IdGenerator.reset();
        System.out.println(IdGenerator.generate());
        System.out.println(IdGenerator.generate());
        System.out.println(IdGenerator.generate());
    }
}
`,
		},
		{
			name: "temperature conversions",
			expected: "212.0 F\n32.0 F\n98.6 F\n",
			code: `public class Main {
    static class Temperature {
        private final double value;
        private final String unit;
        private Temperature(double value, String unit) { this.value = value; this.unit = unit; }
        static Temperature celsius(double v) { return new Temperature(v, "C"); }
        static Temperature fahrenheit(double v) { return new Temperature(v, "F"); }
        Temperature toFahrenheit() {
            if (unit.equals("F")) return this;
            return new Temperature(value * 9 / 5 + 32, "F");
        }
        public String toString() { return value + " " + unit; }
    }
    public static void main(String[] args) {
        System.out.println(Temperature.celsius(100).toFahrenheit());
        System.out.println(Temperature.celsius(0).toFahrenheit());
        Temperature f = Temperature.fahrenheit(98.6);
        System.out.println(f.toFahrenheit());
    }
}
`,
		},
	],
};
