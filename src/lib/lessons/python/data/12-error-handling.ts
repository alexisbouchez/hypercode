import type { Lesson } from "../../types";

export const errorHandling: Lesson = {
	id: "error-handling",
	title: "Error Handling",
	chapterId: "object-oriented",
	content: `## Error Handling

Python uses exceptions for error handling. Exceptions are classes that inherit from \`BaseException\`.

### try / except

\`\`\`python
try:
    result = 10 / 0
except ZeroDivisionError:
    print("Cannot divide by zero")
\`\`\`

### Multiple except Clauses

\`\`\`python
try:
    value = int("abc")
except ValueError as e:
    print(f"ValueError: {e}")
except TypeError:
    print("Wrong type")
\`\`\`

### else and finally

\`\`\`python
try:
    result = 10 / 2
except ZeroDivisionError:
    print("error")
else:
    print(f"ok: {result}")  # runs only if no exception
finally:
    print("always runs")    # cleanup code
\`\`\`

### Raising Exceptions

\`\`\`python
def set_age(age):
    if age < 0:
        raise ValueError(f"Age cannot be negative: {age}")
    return age
\`\`\`

### Custom Exceptions

\`\`\`python
class InsufficientFundsError(Exception):
    def __init__(self, amount, balance):
        super().__init__(f"Need {amount}, have {balance}")
        self.amount = amount
        self.balance = balance
\`\`\`

### Your Task

Implement \`safe_divide(a, b)\` that:
- Returns \`a / b\` if \`b != 0\`
- Raises \`ValueError("Cannot divide by zero")\` if \`b == 0\``,

	starterCode: `def safe_divide(a, b):
    # Divide a by b, or raise ValueError if b is 0
    pass

print(safe_divide(10, 2))
print(safe_divide(7, 2))

try:
    safe_divide(5, 0)
except ValueError as e:
    print(e)
`,

	solution: `def safe_divide(a, b):
    if b == 0:
        raise ValueError("Cannot divide by zero")
    return a / b

print(safe_divide(10, 2))
print(safe_divide(7, 2))

try:
    safe_divide(5, 0)
except ValueError as e:
    print(e)
`,

	tests: [
		{
			name: "safe_divide(10, 2) = 5.0",
			code: `{{FUNC}}
print(safe_divide(10, 2))`,
			expected: "5.0\n",
		},
		{
			name: "safe_divide(7, 2) = 3.5",
			code: `{{FUNC}}
print(safe_divide(7, 2))`,
			expected: "3.5\n",
		},
		{
			name: "raises ValueError on division by zero",
			code: `{{FUNC}}
try:
    safe_divide(5, 0)
except ValueError as e:
    print(e)`,
			expected: "Cannot divide by zero\n",
		},
		{
			name: "safe_divide(0, 5) = 0.0",
			code: `{{FUNC}}
print(safe_divide(0, 5))`,
			expected: "0.0\n",
		},
	],
};
