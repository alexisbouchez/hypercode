import type { Lesson } from "../../types";

export const enums: Lesson = {
	id: "enums",
	title: "Enums",
	chapterId: "oop",
	content: `## Enums

An **enum** (short for enumeration) is a special class that represents a fixed set of constants. Use enums when a variable can only take one of a small set of possible values.

### Basic Enum Declaration

\`\`\`java
enum Direction {
    NORTH, SOUTH, EAST, WEST
}

Direction d = Direction.NORTH;
System.out.println(d);           // NORTH
System.out.println(d.name());    // NORTH
System.out.println(d.ordinal()); // 0
\`\`\`

### Enum Methods

Every enum gets useful methods automatically:
- \`name()\` — returns the constant name as a string
- \`ordinal()\` — returns the position (0-based)
- \`values()\` — returns an array of all constants
- \`valueOf(String)\` — converts a string to the enum constant

### Enums with Fields and Constructors

Enums can have fields, constructors, and methods:

\`\`\`java
enum Planet {
    MERCURY(3.303e+23, 2.4397e6),
    EARTH(5.976e+24, 6.37814e6);

    private final double mass;
    private final double radius;

    Planet(double mass, double radius) {
        this.mass = mass;
        this.radius = radius;
    }

    double surfaceGravity() {
        final double G = 6.67300E-11;
        return G * mass / (radius * radius);
    }
}
\`\`\`

### Enums with Abstract Methods

Each constant can override an abstract method:

\`\`\`java
enum Operation {
    ADD { double apply(double a, double b) { return a + b; } },
    SUB { double apply(double a, double b) { return a - b; } };

    abstract double apply(double a, double b);
}
\`\`\`

### Your Task

1. Create an enum \`Season\` with constants \`SPRING\`, \`SUMMER\`, \`AUTUMN\`, \`WINTER\`, each having a \`label\` field (e.g. "Warm", "Hot", "Cool", "Cold") and a \`getLabel()\` method.

2. Create an enum \`MathOp\` with constants \`ADD\`, \`SUB\`, \`MUL\`, each overriding an abstract method \`int apply(int a, int b)\`.

3. In \`main\`, iterate over all seasons printing \`name: label\`, then use each \`MathOp\` on 10 and 3.`,

	starterCode: `public class Main {
    enum Season {
        // SPRING("Warm"), SUMMER("Hot"), AUTUMN("Cool"), WINTER("Cold");
        // Add a label field, constructor, and getLabel() method
        SPRING, SUMMER, AUTUMN, WINTER;
    }

    enum MathOp {
        // ADD, SUB, MUL — each overrides abstract int apply(int a, int b)
        ADD, SUB, MUL;

        int apply(int a, int b) { return 0; }
    }

    public static void main(String[] args) {
        for (Season s : Season.values()) {
            // Print: NAME: label
        }
        int a = 10, b = 3;
        for (MathOp op : MathOp.values()) {
            // Print: NAME(10, 3) = result
        }
    }
}
`,

	solution: `public class Main {
    enum Season {
        SPRING("Warm"), SUMMER("Hot"), AUTUMN("Cool"), WINTER("Cold");

        private final String label;

        Season(String label) {
            this.label = label;
        }

        String getLabel() {
            return label;
        }
    }

    enum MathOp {
        ADD {
            int apply(int a, int b) { return a + b; }
        },
        SUB {
            int apply(int a, int b) { return a - b; }
        },
        MUL {
            int apply(int a, int b) { return a * b; }
        };

        abstract int apply(int a, int b);
    }

    public static void main(String[] args) {
        for (Season s : Season.values()) {
            System.out.println(s.name() + ": " + s.getLabel());
        }
        int a = 10, b = 3;
        for (MathOp op : MathOp.values()) {
            System.out.println(op.name() + "(" + a + ", " + b + ") = " + op.apply(a, b));
        }
    }
}
`,

	tests: [
		{
			name: "seasons and math ops",
			expected:
				"SPRING: Warm\nSUMMER: Hot\nAUTUMN: Cool\nWINTER: Cold\nADD(10, 3) = 13\nSUB(10, 3) = 7\nMUL(10, 3) = 30\n",
		},
		{
			name: "season ordinals and valueOf",
			expected: "0\n3\nSUMMER: Hot\n",
			code: `public class Main {
    enum Season {
        SPRING("Warm"), SUMMER("Hot"), AUTUMN("Cool"), WINTER("Cold");
        private final String label;
        Season(String label) { this.label = label; }
        String getLabel() { return label; }
    }
    public static void main(String[] args) {
        System.out.println(Season.SPRING.ordinal());
        System.out.println(Season.WINTER.ordinal());
        Season s = Season.valueOf("SUMMER");
        System.out.println(s.name() + ": " + s.getLabel());
    }
}
`,
		},
		{
			name: "math ops with different values",
			expected: "25\n-5\n150\n",
			code: `public class Main {
    enum MathOp {
        ADD { int apply(int a, int b) { return a + b; } },
        SUB { int apply(int a, int b) { return a - b; } },
        MUL { int apply(int a, int b) { return a * b; } };
        abstract int apply(int a, int b);
    }
    public static void main(String[] args) {
        System.out.println(MathOp.ADD.apply(10, 15));
        System.out.println(MathOp.SUB.apply(10, 15));
        System.out.println(MathOp.MUL.apply(10, 15));
    }
}
`,
		},
	],
};
