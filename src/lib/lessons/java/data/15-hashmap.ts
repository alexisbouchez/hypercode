import type { Lesson } from "../../types";

export const hashMap: Lesson = {
	id: "hashmap",
	title: "HashMap",
	chapterId: "advanced",
	content: `## HashMap

\`HashMap<K, V>\` stores key-value pairs with O(1) average lookup:

\`\`\`java
import java.util.HashMap;

HashMap<String, Integer> ages = new HashMap<>();
ages.put("Alice", 30);
ages.put("Bob", 25);
ages.put("Charlie", 35);

System.out.println(ages.size());           // 3
System.out.println(ages.get("Alice"));     // 30
System.out.println(ages.containsKey("Bob"));  // true
System.out.println(ages.getOrDefault("Dave", 0));  // 0

ages.put("Alice", 31);  // update existing key
System.out.println(ages.get("Alice"));  // 31
ages.remove("Bob");
System.out.println(ages.size());  // 2
\`\`\`

### Common Methods

| Method | Description |
|---|---|
| \`put(k, v)\` | Insert or update |
| \`get(k)\` | Retrieve value (null if missing) |
| \`containsKey(k)\` | Check existence |
| \`getOrDefault(k, def)\` | Get or fallback |
| \`remove(k)\` | Delete entry |
| \`size()\` | Number of entries |
| \`keySet()\` | Set of all keys |

### Your Task

Create \`HashMap<String, Integer>\` of scores: Alice→95, Bob→87, Charlie→92.

1. Print its size
2. Print Alice's score
3. Print whether Bob is in the map
4. Print Dave's score with default 0
5. Update Alice's score to 98, print it`,

	starterCode: `import java.util.HashMap;

public class Main {
    public static void main(String[] args) {
        HashMap<String, Integer> scores = new HashMap<>();
        scores.put("Alice", 95);
        scores.put("Bob", 87);
        scores.put("Charlie", 92);
        // Print size, get Alice, containsKey Bob,
        // getOrDefault Dave, update Alice to 98 and print
    }
}
`,

	solution: `import java.util.HashMap;

public class Main {
    public static void main(String[] args) {
        HashMap<String, Integer> scores = new HashMap<>();
        scores.put("Alice", 95);
        scores.put("Bob", 87);
        scores.put("Charlie", 92);
        System.out.println(scores.size());
        System.out.println(scores.get("Alice"));
        System.out.println(scores.containsKey("Bob"));
        System.out.println(scores.getOrDefault("Dave", 0));
        scores.put("Alice", 98);
        System.out.println(scores.get("Alice"));
    }
}
`,

	tests: [
		{
			name: "size, get, containsKey, default, update",
			expected: "3\n95\ntrue\n0\n98\n",
		},
	],
};
