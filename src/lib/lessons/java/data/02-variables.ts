import type { Lesson } from "../../types";

export const variables: Lesson = {
	id: "variables",
	title: "Variables & Types",
	chapterId: "basics",
	content: `## Variables and Primitive Types

Java is statically typed — every variable has a declared type:

\`\`\`java
int age = 25;           // 32-bit integer
long population = 8_000_000_000L;  // 64-bit integer
double pi = 3.14159;    // 64-bit floating point
boolean active = true;  // true or false
char grade = 'A';       // single character
\`\`\`

### String

\`String\` is a class (not a primitive), but behaves like one for most purposes:

\`\`\`java
String name = "Alice";
String greeting = "Hello, " + name + "!";  // concatenation
\`\`\`

### Type Inference with \`var\`

Since Java 10, you can use \`var\` to let the compiler infer the type:

\`\`\`java
var score = 100;        // inferred as int
var message = "Hi";    // inferred as String
\`\`\`

### Constants

Use \`final\` to declare a constant:

\`\`\`java
final double TAX_RATE = 0.08;
\`\`\`

### Your Task

Declare the following variables and print the output exactly as shown:
- \`name\` = \`"Alice"\`, \`age\` = \`25\`, \`height\` = \`1.75\`, \`student\` = \`true\`
- Print: \`Alice is 25 years old\`
- Print: \`Height: 1.75\`
- Print: \`Student: true\``,

	starterCode: `public class Main {
    public static void main(String[] args) {
        String name = "Alice";
        int age = 25;
        double height = 1.75;
        boolean student = true;
        // Print the three lines
    }
}
`,

	solution: `public class Main {
    public static void main(String[] args) {
        String name = "Alice";
        int age = 25;
        double height = 1.75;
        boolean student = true;
        System.out.println(name + " is " + age + " years old");
        System.out.println("Height: " + height);
        System.out.println("Student: " + student);
    }
}
`,

	tests: [
		{
			name: "correct output",
			expected: "Alice is 25 years old\nHeight: 1.75\nStudent: true\n",
		},
	],
};
