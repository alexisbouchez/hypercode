import type { Lesson } from "../../types";

export const loops: Lesson = {
	id: "loops",
	title: "Loops",
	chapterId: "control-flow",
	content: `## Loops

### for Loop

\`\`\`java
for (int i = 0; i < 5; i++) {
    System.out.println(i);  // 0, 1, 2, 3, 4
}
\`\`\`

### while Loop

\`\`\`java
int n = 3;
while (n > 0) {
    System.out.print(n + " ");
    n--;
}
// prints: 3 2 1
\`\`\`

### for-each Loop

Iterate over arrays or collections:

\`\`\`java
String[] fruits = {"apple", "banana", "cherry"};
for (String fruit : fruits) {
    System.out.println(fruit);
}
\`\`\`

### break and continue

\`\`\`java
for (int i = 0; i < 10; i++) {
    if (i == 3) continue;  // skip 3
    if (i == 6) break;     // stop at 6
    System.out.print(i + " ");
}
// prints: 0 1 2 4 5
\`\`\`

### Your Task

1. Use a \`for\` loop to sum integers 1 through 5, print the sum
2. Use a \`while\` loop to print \`3 2 1 \` (note the trailing space)
3. Use a \`for-each\` loop over \`{"apple", "banana", "cherry"}\`, printing each on its own line`,

	starterCode: `public class Main {
    public static void main(String[] args) {
        // 1. Sum 1..5
        // 2. Countdown 3 2 1
        // 3. For-each over fruits
    }
}
`,

	solution: `public class Main {
    public static void main(String[] args) {
        int sum = 0;
        for (int i = 1; i <= 5; i++) {
            sum += i;
        }
        System.out.println(sum);

        int n = 3;
        while (n > 0) {
            System.out.print(n + " ");
            n--;
        }
        System.out.println();

        String[] fruits = {"apple", "banana", "cherry"};
        for (String fruit : fruits) {
            System.out.println(fruit);
        }
    }
}
`,

	tests: [
		{
			name: "sum=15, countdown, fruits",
			expected: "15\n3 2 1 \napple\nbanana\ncherry\n",
		},
	],
};
