import type { Lesson } from "../../types";

export const generics: Lesson = {
	id: "generics",
	title: "Generics",
	chapterId: "advanced",
	content: `## Generics

Generics let you write type-safe classes and methods that work with any type:

\`\`\`java
class Pair<A, B> {
    A first;
    B second;

    Pair(A first, B second) {
        this.first = first;
        this.second = second;
    }

    @Override
    public String toString() {
        return "(" + first + ", " + second + ")";
    }
}

Pair<String, Integer> p = new Pair<>("hello", 42);
System.out.println(p);         // (hello, 42)
System.out.println(p.first);   // hello
\`\`\`

### Generic Methods

\`\`\`java
static <T extends Comparable<T>> T min(T a, T b) {
    return a.compareTo(b) <= 0 ? a : b;
}

System.out.println(min(3, 7));          // 3
System.out.println(min("apple", "banana"));  // apple
\`\`\`

### Type Bounds

- \`<T extends Comparable<T>>\` — T must implement Comparable
- \`<T extends Number>\` — T must be a Number subtype

### Your Task

Implement:
- Generic class \`Stack<T>\` with \`push(T)\`, \`T pop()\`, \`int size()\`
- Generic method \`<T extends Comparable<T>> T min(T a, T b)\`

Push 10, 20, 30 onto the stack; print size, pop twice, print size again.
Then print \`min(3, 7)\` and \`min("apple", "banana")\`.`,

	starterCode: `public class Main {
    static class Stack<T> {
        private Object[] data;
        private int size;

        Stack(int capacity) {
            data = new Object[capacity];
            size = 0;
        }

        void push(T item) {
            data[size++] = item;
        }

        @SuppressWarnings("unchecked")
        T pop() {
            return (T) data[--size];
        }

        int size() { return size; }
    }

    static <T extends Comparable<T>> T min(T a, T b) {
        // return the smaller of a and b
        return a;
    }

    public static void main(String[] args) {
        Stack<Integer> stack = new Stack<>(10);
        stack.push(10);
        stack.push(20);
        stack.push(30);
        System.out.println(stack.size());
        System.out.println(stack.pop());
        System.out.println(stack.pop());
        System.out.println(stack.size());
        System.out.println(min(3, 7));
        System.out.println(min("apple", "banana"));
    }
}
`,

	solution: `public class Main {
    static class Stack<T> {
        private Object[] data;
        private int size;

        Stack(int capacity) {
            data = new Object[capacity];
            size = 0;
        }

        void push(T item) {
            data[size++] = item;
        }

        @SuppressWarnings("unchecked")
        T pop() {
            return (T) data[--size];
        }

        int size() { return size; }
    }

    static <T extends Comparable<T>> T min(T a, T b) {
        return a.compareTo(b) <= 0 ? a : b;
    }

    public static void main(String[] args) {
        Stack<Integer> stack = new Stack<>(10);
        stack.push(10);
        stack.push(20);
        stack.push(30);
        System.out.println(stack.size());
        System.out.println(stack.pop());
        System.out.println(stack.pop());
        System.out.println(stack.size());
        System.out.println(min(3, 7));
        System.out.println(min("apple", "banana"));
    }
}
`,

	tests: [
		{
			name: "stack ops and min",
			expected: "3\n30\n20\n1\n3\napple\n",
		},
	],
};
