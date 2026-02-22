import type { Lesson } from "../../types";

export const recursion: Lesson = {
	id: "recursion",
	title: "Recursion",
	chapterId: "methods",
	content: `## Recursion

A recursive method calls itself. Every recursion needs a **base case** to stop:

\`\`\`java
static int factorial(int n) {
    if (n <= 1) return 1;          // base case
    return n * factorial(n - 1);   // recursive case
}

System.out.println(factorial(5));  // 120
\`\`\`

### How It Works

\`factorial(4)\` expands as:
\`\`\`
factorial(4)
  4 * factorial(3)
       3 * factorial(2)
            2 * factorial(1)
                 1           (base case)
= 4 * 3 * 2 * 1 = 24
\`\`\`

### Fibonacci

The Fibonacci sequence: 0, 1, 1, 2, 3, 5, 8, 13, 21, 34, 55, ...

\`\`\`java
static int fibonacci(int n) {
    if (n <= 1) return n;
    return fibonacci(n - 1) + fibonacci(n - 2);
}

System.out.println(fibonacci(7));  // 13
\`\`\`

### Your Task

Implement:
- \`factorial(int n)\` returning a \`long\` (for large values) — \`factorial(10)\` = \`3628800\`
- \`fibonacci(int n)\` — \`fibonacci(10)\` = \`55\`

Print \`factorial(10)\` then \`fibonacci(10)\`.`,

	starterCode: `public class Main {
    static long factorial(int n) {
        // base case: n <= 1 → return 1
        // recursive case: n * factorial(n-1)
        return 0;
    }

    static int fibonacci(int n) {
        // base case: n <= 1 → return n
        // recursive case: fibonacci(n-1) + fibonacci(n-2)
        return 0;
    }

    public static void main(String[] args) {
        System.out.println(factorial(10));
        System.out.println(fibonacci(10));
    }
}
`,

	solution: `public class Main {
    static long factorial(int n) {
        if (n <= 1) return 1;
        return n * factorial(n - 1);
    }

    static int fibonacci(int n) {
        if (n <= 1) return n;
        return fibonacci(n - 1) + fibonacci(n - 2);
    }

    public static void main(String[] args) {
        System.out.println(factorial(10));
        System.out.println(fibonacci(10));
    }
}
`,

	tests: [
		{
			name: "factorial(10)=3628800, fibonacci(10)=55",
			expected: "3628800\n55\n",
		},
	],
};
