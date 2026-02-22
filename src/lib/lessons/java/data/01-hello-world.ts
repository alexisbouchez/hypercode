import type { Lesson } from "../../types";

export const helloWorld: Lesson = {
	id: "hello-world",
	title: "Hello, World!",
	chapterId: "basics",
	content: `## Your First Java Program

Java programs live inside a **class**. The entry point is always a method called \`main\`:

\`\`\`java
public class Main {
    public static void main(String[] args) {
        System.out.println("Hello, World!");
    }
}
\`\`\`

- \`public class Main\` — declares a class named \`Main\` (must match the filename \`Main.java\`)
- \`public static void main(String[] args)\` — the JVM calls this method to start your program
- \`System.out.println\` — prints a line to standard output (adds a newline)

You can also use \`System.out.print\` (no newline) for inline output:

\`\`\`java
System.out.print("Hello");
System.out.print(", ");
System.out.println("World!");  // Hello, World!
\`\`\`

### Your Task

Print exactly \`Hello, World!\` using \`System.out.println\`.`,

	starterCode: `public class Main {
    public static void main(String[] args) {
        // Print "Hello, World!"
    }
}
`,

	solution: `public class Main {
    public static void main(String[] args) {
        System.out.println("Hello, World!");
    }
}
`,

	tests: [
		{
			name: "prints Hello, World!",
			expected: "Hello, World!\n",
		},
	],
};
