import type { Lesson } from "../../types";

export const arrays: Lesson = {
	id: "arrays",
	title: "Arrays",
	chapterId: "basics",
	content: `## Arrays

An array stores a fixed-size sequence of elements of the same type:

\`\`\`java
int[] nums = {5, 2, 8, 1, 9};
String[] names = {"Alice", "Bob", "Charlie"};
double[] prices = new double[3];  // filled with 0.0
\`\`\`

Access elements by index (0-based):

\`\`\`java
nums[0]   // 5
nums[4]   // 9
nums.length  // 5 (not a method call — it's a field)
\`\`\`

### java.util.Arrays

The \`Arrays\` utility class provides sorting and printing:

\`\`\`java
import java.util.Arrays;

int[] arr = {3, 1, 4, 1, 5};
Arrays.sort(arr);                  // sorts in place
System.out.println(Arrays.toString(arr));  // [1, 1, 3, 4, 5]
\`\`\`

### 2D Arrays

\`\`\`java
int[][] grid = {{1, 2}, {3, 4}, {5, 6}};
System.out.println(grid[1][0]);  // 3
\`\`\`

### Your Task

Given \`nums = {5, 2, 8, 1, 9, 3}\`:
1. Print its length
2. Print the first element
3. Sort it, then print \`Arrays.toString\` result
4. Print the first element after sorting`,

	starterCode: `import java.util.Arrays;

public class Main {
    public static void main(String[] args) {
        int[] nums = {5, 2, 8, 1, 9, 3};
        // Print length, first element, sorted array, first element after sort
    }
}
`,

	solution: `import java.util.Arrays;

public class Main {
    public static void main(String[] args) {
        int[] nums = {5, 2, 8, 1, 9, 3};
        System.out.println(nums.length);
        System.out.println(nums[0]);
        Arrays.sort(nums);
        System.out.println(Arrays.toString(nums));
        System.out.println(nums[0]);
    }
}
`,

	tests: [
		{
			name: "length, first, sorted, min",
			expected: "6\n5\n[1, 2, 3, 5, 8, 9]\n1\n",
		},
	],
};
