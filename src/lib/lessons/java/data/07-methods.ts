import type { Lesson } from "../../types";

export const methods: Lesson = {
	id: "methods",
	title: "Methods",
	chapterId: "methods",
	content: `## Methods

Methods are named blocks of code that can accept parameters and return values.

\`\`\`java
static int add(int a, int b) {
    return a + b;
}
\`\`\`

- \`static\` — can be called without creating an object
- \`int\` — return type (use \`void\` for no return value)
- Parameters are typed and comma-separated

### Method Overloading

Java allows multiple methods with the same name if their parameter types differ:

\`\`\`java
static int max(int a, int b) { return a > b ? a : b; }
static double max(double a, double b) { return a > b ? a : b; }

System.out.println(max(3, 7));      // 7
System.out.println(max(3.5, 2.1));  // 3.5
\`\`\`

### Return Types

\`\`\`java
static String greet(String name) {
    return "Hello, " + name + "!";
}
static void printLine() {
    System.out.println("---");
}
\`\`\`

### Your Task

Implement and call three static methods:
- \`multiply(int a, int b)\` → returns \`a * b\`
- \`average(double a, double b, double c)\` → returns the average
- \`repeat(String s, int n)\` → returns \`s\` repeated \`n\` times

Print \`multiply(6, 7)\`, \`average(1.0, 2.0, 3.0)\`, and \`repeat("ha", 3)\`.`,

	starterCode: `public class Main {
    static int multiply(int a, int b) {
        // return a * b
        return 0;
    }

    static double average(double a, double b, double c) {
        // return the average of three numbers
        return 0;
    }

    static String repeat(String s, int n) {
        // return s repeated n times
        return "";
    }

    public static void main(String[] args) {
        System.out.println(multiply(6, 7));
        System.out.println(average(1.0, 2.0, 3.0));
        System.out.println(repeat("ha", 3));
    }
}
`,

	solution: `public class Main {
    static int multiply(int a, int b) {
        return a * b;
    }

    static double average(double a, double b, double c) {
        return (a + b + c) / 3.0;
    }

    static String repeat(String s, int n) {
        String result = "";
        for (int i = 0; i < n; i++) {
            result += s;
        }
        return result;
    }

    public static void main(String[] args) {
        System.out.println(multiply(6, 7));
        System.out.println(average(1.0, 2.0, 3.0));
        System.out.println(repeat("ha", 3));
    }
}
`,

	tests: [
		{
			name: "42, 2.0, hahaha",
			expected: "42\n2.0\nhahaha\n",
		},
	],
};
