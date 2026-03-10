import type { Lesson } from "../../types";

export const exceptionHandling: Lesson = {
	id: "exception-handling",
	title: "Exception Handling",
	chapterId: "error-handling",
	content: `## Exception Handling

Java uses **exceptions** to handle errors at runtime. When something goes wrong, an exception is **thrown**, and you can **catch** it to handle it gracefully.

### try / catch / finally

\`\`\`java
try {
    int result = 10 / 0;
} catch (ArithmeticException e) {
    System.out.println("Error: " + e.getMessage());
} finally {
    System.out.println("This always runs");
}
\`\`\`

- \`try\` — wraps code that might throw an exception
- \`catch\` — handles a specific exception type
- \`finally\` — always executes, whether an exception occurred or not (useful for cleanup)

### Checked vs Unchecked Exceptions

- **Unchecked** (\`RuntimeException\` and its subclasses): \`NullPointerException\`, \`ArithmeticException\`, \`ArrayIndexOutOfBoundsException\`. The compiler does **not** force you to handle these.
- **Checked** (\`Exception\` but not \`RuntimeException\`): \`IOException\`, \`SQLException\`. The compiler **forces** you to either catch them or declare them with \`throws\`.

### Throwing Exceptions

Use \`throw\` to raise an exception yourself:

\`\`\`java
void setAge(int age) {
    if (age < 0) {
        throw new IllegalArgumentException("Age cannot be negative");
    }
}
\`\`\`

### Custom Exceptions

You can define your own exception classes by extending \`Exception\` (checked) or \`RuntimeException\` (unchecked):

\`\`\`java
class InsufficientFundsException extends RuntimeException {
    private double deficit;

    InsufficientFundsException(double deficit) {
        super("Insufficient funds: need " + deficit + " more");
        this.deficit = deficit;
    }

    double getDeficit() { return deficit; }
}
\`\`\`

### Your Task

Create a static inner class \`SafeCalculator\` with:
- \`static String divide(int a, int b)\` — returns the result as a string, or \`"Error: division by zero"\` if \`b\` is 0 (use try/catch on \`ArithmeticException\`)
- \`static String parseAndAdd(String x, String y)\` — parses both strings as integers and returns their sum as a string. If either string is not a valid integer, return \`"Error: invalid number"\`. Use a \`finally\` block to print \`"parse complete"\` regardless of outcome.

Also create a custom exception class \`NegativeNumberException\` (extending \`RuntimeException\`) with a constructor that takes a \`String\` message, and a method:
- \`static int squareRoot(int n)\` — if \`n < 0\`, throw \`NegativeNumberException\` with message \`"negative input: "\` followed by \`n\`. Otherwise return \`(int) Math.sqrt(n)\`.

Then run the test sequence shown in the expected output.`,

	starterCode: `public class Main {
    static class NegativeNumberException extends RuntimeException {
        NegativeNumberException(String message) {
            super(message);
        }
    }

    static class SafeCalculator {
        static String divide(int a, int b) {
            // Use try/catch to handle ArithmeticException
            return "";
        }

        static String parseAndAdd(String x, String y) {
            // Use try/catch/finally
            // finally block should print "parse complete"
            return "";
        }

        static int squareRoot(int n) {
            // Throw NegativeNumberException if n < 0
            return 0;
        }
    }

    public static void main(String[] args) {
        System.out.println(SafeCalculator.divide(10, 3));
        System.out.println(SafeCalculator.divide(10, 0));
        System.out.println(SafeCalculator.parseAndAdd("5", "3"));
        System.out.println(SafeCalculator.parseAndAdd("five", "3"));
        System.out.println(SafeCalculator.squareRoot(16));
        try {
            SafeCalculator.squareRoot(-4);
        } catch (NegativeNumberException e) {
            System.out.println("Caught: " + e.getMessage());
        }
    }
}
`,

	solution: `public class Main {
    static class NegativeNumberException extends RuntimeException {
        NegativeNumberException(String message) {
            super(message);
        }
    }

    static class SafeCalculator {
        static String divide(int a, int b) {
            try {
                return String.valueOf(a / b);
            } catch (ArithmeticException e) {
                return "Error: division by zero";
            }
        }

        static String parseAndAdd(String x, String y) {
            try {
                int a = Integer.parseInt(x);
                int b = Integer.parseInt(y);
                return String.valueOf(a + b);
            } catch (NumberFormatException e) {
                return "Error: invalid number";
            } finally {
                System.out.println("parse complete");
            }
        }

        static int squareRoot(int n) {
            if (n < 0) {
                throw new NegativeNumberException("negative input: " + n);
            }
            return (int) Math.sqrt(n);
        }
    }

    public static void main(String[] args) {
        System.out.println(SafeCalculator.divide(10, 3));
        System.out.println(SafeCalculator.divide(10, 0));
        System.out.println(SafeCalculator.parseAndAdd("5", "3"));
        System.out.println(SafeCalculator.parseAndAdd("five", "3"));
        System.out.println(SafeCalculator.squareRoot(16));
        try {
            SafeCalculator.squareRoot(-4);
        } catch (NegativeNumberException e) {
            System.out.println("Caught: " + e.getMessage());
        }
    }
}
`,

	tests: [
		{
			name: "try/catch division and parse",
			expected:
				"3\nError: division by zero\nparse complete\n8\nparse complete\nError: invalid number\n4\nCaught: negative input: -4\n",
		},
		{
			name: "finally block always executes",
			expected: "parse complete\n15\nparse complete\nError: invalid number\n",
			code: `public class Main {
    static class SafeCalculator {
        static String parseAndAdd(String x, String y) {
            try {
                int a = Integer.parseInt(x);
                int b = Integer.parseInt(y);
                return String.valueOf(a + b);
            } catch (NumberFormatException e) {
                return "Error: invalid number";
            } finally {
                System.out.println("parse complete");
            }
        }
    }
    public static void main(String[] args) {
        System.out.println(SafeCalculator.parseAndAdd("7", "8"));
        System.out.println(SafeCalculator.parseAndAdd("abc", "2"));
    }
}
`,
		},
		{
			name: "custom exception with message",
			expected: "5\nCaught: negative input: -9\nCaught: negative input: -1\n",
			code: `public class Main {
    static class NegativeNumberException extends RuntimeException {
        NegativeNumberException(String message) {
            super(message);
        }
    }
    static class SafeCalculator {
        static int squareRoot(int n) {
            if (n < 0) {
                throw new NegativeNumberException("negative input: " + n);
            }
            return (int) Math.sqrt(n);
        }
    }
    public static void main(String[] args) {
        System.out.println(SafeCalculator.squareRoot(25));
        try {
            SafeCalculator.squareRoot(-9);
        } catch (NegativeNumberException e) {
            System.out.println("Caught: " + e.getMessage());
        }
        try {
            SafeCalculator.squareRoot(-1);
        } catch (NegativeNumberException e) {
            System.out.println("Caught: " + e.getMessage());
        }
    }
}
`,
		},
		{
			name: "divide edge cases",
			expected: "0\n-5\nError: division by zero\n",
			code: `public class Main {
    static class SafeCalculator {
        static String divide(int a, int b) {
            try {
                return String.valueOf(a / b);
            } catch (ArithmeticException e) {
                return "Error: division by zero";
            }
        }
    }
    public static void main(String[] args) {
        System.out.println(SafeCalculator.divide(0, 5));
        System.out.println(SafeCalculator.divide(-10, 2));
        System.out.println(SafeCalculator.divide(7, 0));
    }
}
`,
		},
	],
};
