import type { Lesson } from "../../types";

export const lambdaExpressions: Lesson = {
	id: "lambda-expressions",
	title: "Lambda Expressions",
	chapterId: "functional",
	content: `## Lambda Expressions

Java 8 introduced **lambda expressions** — concise anonymous functions that implement a **functional interface** (an interface with exactly one abstract method).

### Functional Interfaces

A functional interface has a single abstract method. The \`@FunctionalInterface\` annotation is optional but recommended:

\`\`\`java
@FunctionalInterface
interface Greeting {
    String greet(String name);
}
\`\`\`

Java provides built-in functional interfaces in \`java.util.function\`:
- \`Function<T, R>\` — takes T, returns R
- \`Predicate<T>\` — takes T, returns boolean
- \`Consumer<T>\` — takes T, returns void
- \`Supplier<T>\` — takes nothing, returns T

### Lambda Syntax

A lambda replaces the boilerplate of an anonymous class:

\`\`\`java
// Full syntax
Greeting g1 = (String name) -> { return "Hello, " + name; };

// Simplified — type inference, no braces for single expression
Greeting g2 = name -> "Hello, " + name;

System.out.println(g2.greet("Alice")); // Hello, Alice
\`\`\`

### Method References

When a lambda simply delegates to an existing method, use a **method reference**:

\`\`\`java
// Static method reference
Function<String, Integer> parse = Integer::parseInt;

// Instance method reference
Function<String, String> upper = String::toUpperCase;

System.out.println(parse.apply("42"));      // 42
System.out.println(upper.apply("hello"));   // HELLO
\`\`\`

### Streams: map, filter, reduce

The Streams API lets you process collections in a functional pipeline:

\`\`\`java
import java.util.*;
import java.util.stream.*;

List<Integer> nums = List.of(1, 2, 3, 4, 5);

// filter: keep only elements matching a predicate
List<Integer> evens = nums.stream()
    .filter(n -> n % 2 == 0)
    .collect(Collectors.toList());
// [2, 4]

// map: transform each element
List<String> strs = nums.stream()
    .map(n -> "num:" + n)
    .collect(Collectors.toList());

// reduce: combine all elements into one value
int sum = nums.stream()
    .reduce(0, (a, b) -> a + b);
// 15
\`\`\`

### Your Task

1. Use \`stream().filter()\` to keep only strings with length > 3 from a list, then print each on its own line in order.

2. Use \`stream().map()\` to convert a list of integers to their squares, then print each on its own line.

3. Use \`stream().reduce()\` to compute the product of a list of integers and print the result.

4. Combine \`filter\`, \`map\`, and \`reduce\`: given a list of integers, keep only the even numbers, square them, and sum the results. Print the final sum.`,

	starterCode: `import java.util.*;
import java.util.stream.*;

public class Main {
    public static void main(String[] args) {
        List<String> words = List.of("hi", "hello", "hey", "howdy", "yo");
        // 1. Filter words with length > 3, print each on its own line

        List<Integer> nums = List.of(1, 2, 3, 4, 5);
        // 2. Map to squares, print each on its own line

        List<Integer> factors = List.of(2, 3, 4, 5);
        // 3. Reduce to product, print result

        List<Integer> mixed = List.of(1, 2, 3, 4, 5, 6);
        // 4. Filter even, square them, sum — print result
    }
}
`,

	solution: `import java.util.*;
import java.util.stream.*;

public class Main {
    public static void main(String[] args) {
        List<String> words = List.of("hi", "hello", "hey", "howdy", "yo");
        words.stream()
            .filter(w -> w.length() > 3)
            .forEach(System.out::println);

        List<Integer> nums = List.of(1, 2, 3, 4, 5);
        nums.stream()
            .map(n -> n * n)
            .forEach(System.out::println);

        List<Integer> factors = List.of(2, 3, 4, 5);
        int product = factors.stream()
            .reduce(1, (a, b) -> a * b);
        System.out.println(product);

        List<Integer> mixed = List.of(1, 2, 3, 4, 5, 6);
        int sum = mixed.stream()
            .filter(n -> n % 2 == 0)
            .map(n -> n * n)
            .reduce(0, Integer::sum);
        System.out.println(sum);
    }
}
`,

	tests: [
		{
			name: "filter, map, reduce, and combined pipeline",
			expected: "hello\nhowdy\n1\n4\n9\n16\n25\n120\n56\n",
		},
		{
			name: "filter with different data",
			expected: "lambda\nstreams\n",
			code: `import java.util.*;
import java.util.stream.*;

public class Main {
    public static void main(String[] args) {
        List<String> words = List.of("map", "lambda", "go", "streams");
        words.stream()
            .filter(w -> w.length() > 3)
            .forEach(System.out::println);
    }
}
`,
		},
		{
			name: "reduce sum with method reference",
			expected: "21\n",
			code: `import java.util.*;
import java.util.stream.*;

public class Main {
    public static void main(String[] args) {
        List<Integer> nums = List.of(1, 2, 3, 4, 5, 6);
        int sum = nums.stream().reduce(0, Integer::sum);
        System.out.println(sum);
    }
}
`,
		},
		{
			name: "chained filter-map-collect",
			expected: "[4, 16, 36]\n",
			code: `import java.util.*;
import java.util.stream.*;

public class Main {
    public static void main(String[] args) {
        List<Integer> nums = List.of(1, 2, 3, 4, 5, 6);
        List<Integer> result = nums.stream()
            .filter(n -> n % 2 == 0)
            .map(n -> n * n)
            .collect(Collectors.toList());
        System.out.println(result);
    }
}
`,
		},
	],
};
