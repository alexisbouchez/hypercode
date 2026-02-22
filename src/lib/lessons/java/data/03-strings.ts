import type { Lesson } from "../../types";

export const strings: Lesson = {
	id: "strings",
	title: "Strings",
	chapterId: "basics",
	content: `## Working with Strings

Java's \`String\` class has many useful methods:

\`\`\`java
String s = "Hello, Java!";

s.length()          // 12
s.toUpperCase()     // "HELLO, JAVA!"
s.toLowerCase()     // "hello, java!"
s.substring(0, 5)   // "Hello"
s.charAt(7)         // 'J'
s.indexOf("Java")   // 7
s.contains("Java")  // true
s.replace("Java", "World")  // "Hello, World!"
s.trim()            // removes leading/trailing whitespace
s.startsWith("Hello")  // true
s.endsWith("!")        // true
\`\`\`

### String Comparison

Always use \`.equals()\` for content comparison (not \`==\`):

\`\`\`java
String a = "hello";
String b = "hello";
System.out.println(a.equals(b));   // true
System.out.println(a.equalsIgnoreCase("HELLO"));  // true
\`\`\`

### String Formatting

\`String.format\` works like printf:

\`\`\`java
String msg = String.format("Pi is %.2f", 3.14159);
// "Pi is 3.14"
\`\`\`

### Your Task

Given \`s = "Hello, Java!"\`, print:
1. Its length
2. It in uppercase
3. The substring from index 0 to 5 (exclusive)
4. The string with "Java" replaced by "World"
5. Whether it contains "Java"`,

	starterCode: `public class Main {
    public static void main(String[] args) {
        String s = "Hello, Java!";
        // Print length, uppercase, substring(0,5), replaced, contains
    }
}
`,

	solution: `public class Main {
    public static void main(String[] args) {
        String s = "Hello, Java!";
        System.out.println(s.length());
        System.out.println(s.toUpperCase());
        System.out.println(s.substring(0, 5));
        System.out.println(s.replace("Java", "World"));
        System.out.println(s.contains("Java"));
    }
}
`,

	tests: [
		{
			name: "length, uppercase, substring, replace, contains",
			expected: "12\nHELLO, JAVA!\nHello\nHello, World!\ntrue\n",
		},
	],
};
