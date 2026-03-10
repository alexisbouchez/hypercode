import type { Lesson } from "../../types";

export const conditionals: Lesson = {
	id: "conditionals",
	title: "Conditionals",
	chapterId: "control-flow",
	content: `## Conditionals

### if / else if / else

\`\`\`java
int score = 85;
if (score >= 90) {
    System.out.println("A");
} else if (score >= 80) {
    System.out.println("B");
} else if (score >= 70) {
    System.out.println("C");
} else {
    System.out.println("F");
}
// prints: B
\`\`\`

### Ternary Operator

A compact one-liner for simple conditions:

\`\`\`java
int x = 7;
String result = (x % 2 == 0) ? "even" : "odd";
System.out.println(result);  // odd
\`\`\`

### Logical Operators

- \`&&\` — and
- \`||\` — or
- \`!\` — not

\`\`\`java
int age = 20;
boolean hasId = true;
if (age >= 18 && hasId) {
    System.out.println("entry allowed");
}
\`\`\`

### Your Task

Given \`score = 85\` and \`n = 7\`:
1. Print the letter grade (A ≥ 90, B ≥ 80, C ≥ 70, F otherwise)
2. Print whether \`n\` is \`"even"\` or \`"odd"\` using the ternary operator`,

	starterCode: `public class Main {
    public static void main(String[] args) {
        int score = 85;
        int n = 7;
        // Print grade and even/odd
    }
}
`,

	solution: `public class Main {
    public static void main(String[] args) {
        int score = 85;
        int n = 7;
        String grade;
        if (score >= 90) {
            grade = "A";
        } else if (score >= 80) {
            grade = "B";
        } else if (score >= 70) {
            grade = "C";
        } else {
            grade = "F";
        }
        System.out.println(grade);
        String parity = (n % 2 == 0) ? "even" : "odd";
        System.out.println(parity);
    }
}
`,

	tests: [
		{
			name: "grade B and odd",
			expected: "B\nodd\n",
		},
		{
			name: "grade A for score >= 90",
			expected: "A\n",
			code: `public class Main {
    public static void main(String[] args) {
        int score = 95;
        String grade;
        if (score >= 90) {
            grade = "A";
        } else if (score >= 80) {
            grade = "B";
        } else if (score >= 70) {
            grade = "C";
        } else {
            grade = "F";
        }
        System.out.println(grade);
    }
}
`,
		},
		{
			name: "grade C for score 75",
			expected: "C\n",
			code: `public class Main {
    public static void main(String[] args) {
        int score = 75;
        String grade;
        if (score >= 90) {
            grade = "A";
        } else if (score >= 80) {
            grade = "B";
        } else if (score >= 70) {
            grade = "C";
        } else {
            grade = "F";
        }
        System.out.println(grade);
    }
}
`,
		},
		{
			name: "grade F for score below 70 and even number",
			expected: "F\neven\n",
			code: `public class Main {
    public static void main(String[] args) {
        int score = 50;
        String grade;
        if (score >= 90) {
            grade = "A";
        } else if (score >= 80) {
            grade = "B";
        } else if (score >= 70) {
            grade = "C";
        } else {
            grade = "F";
        }
        System.out.println(grade);
        int n = 4;
        String parity = (n % 2 == 0) ? "even" : "odd";
        System.out.println(parity);
    }
}
`,
		},
	],
};
