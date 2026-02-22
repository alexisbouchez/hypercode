import type { Lesson } from "../../types";

export const arrayList: Lesson = {
	id: "arraylist",
	title: "ArrayList",
	chapterId: "advanced",
	content: `## ArrayList

\`ArrayList\` is a resizable array from \`java.util\`. Unlike a plain array, it grows automatically:

\`\`\`java
import java.util.ArrayList;

ArrayList<String> list = new ArrayList<>();
list.add("apple");
list.add("banana");
list.add("cherry");

System.out.println(list.size());       // 3
System.out.println(list.get(1));       // banana
System.out.println(list.contains("apple"));  // true
list.remove("banana");
System.out.println(list.size());       // 2
System.out.println(list);             // [apple, cherry]
\`\`\`

### Sorting

\`\`\`java
import java.util.Collections;

ArrayList<Integer> nums = new ArrayList<>();
nums.add(3); nums.add(1); nums.add(2);
Collections.sort(nums);
System.out.println(nums);  // [1, 2, 3]
\`\`\`

### Iterating

\`\`\`java
for (String item : list) {
    System.out.println(item);
}
\`\`\`

### Your Task

Create an \`ArrayList<String>\` with "banana", "apple", "cherry", "date". Then:
1. Print its size
2. Print the first element (\`get(0)\`)
3. Print whether it contains "apple"
4. Sort it with \`Collections.sort\`, then print the whole list
5. Remove "cherry", print the new size, print the first element`,

	starterCode: `import java.util.ArrayList;
import java.util.Collections;

public class Main {
    public static void main(String[] args) {
        ArrayList<String> list = new ArrayList<>();
        list.add("banana");
        list.add("apple");
        list.add("cherry");
        list.add("date");
        // Print size, get(0), contains("apple")
        // Sort, print list
        // Remove "cherry", print size and get(0)
    }
}
`,

	solution: `import java.util.ArrayList;
import java.util.Collections;

public class Main {
    public static void main(String[] args) {
        ArrayList<String> list = new ArrayList<>();
        list.add("banana");
        list.add("apple");
        list.add("cherry");
        list.add("date");
        System.out.println(list.size());
        System.out.println(list.get(0));
        System.out.println(list.contains("apple"));
        Collections.sort(list);
        System.out.println(list);
        list.remove("cherry");
        System.out.println(list.size());
        System.out.println(list.get(0));
    }
}
`,

	tests: [
		{
			name: "size, get, contains, sort, remove",
			expected: "4\nbanana\ntrue\n[apple, banana, cherry, date]\n3\napple\n",
		},
	],
};
