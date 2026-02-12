import type { Lesson } from "../../types";

export const filteringWithWhere: Lesson = {
  id: "filtering-with-where",
  title: "Filtering with WHERE",
  chapterId: "sql-basics",
  content: `## The WHERE Clause

The \`WHERE\` clause filters rows based on a condition. Only rows that satisfy the condition are included in the result:

\`\`\`sql
SELECT * FROM products WHERE category = 'Electronics';
\`\`\`

This returns only the products whose \`category\` column equals \`'Electronics'\`.

> **Warning:** SQL uses single quotes for string literals. Double quotes are reserved for identifiers (like column or table names). Writing \`WHERE category = "Electronics"\` will cause an error in PostgreSQL because it looks for a column named \`Electronics\`.

### Comparison Operators

SQL provides the standard set of comparison operators:

| Operator | Meaning |
|----------|---------|
| \`=\` | Equal to |
| \`<>\` or \`!=\` | Not equal to |
| \`<\` | Less than |
| \`>\` | Greater than |
| \`<=\` | Less than or equal to |
| \`>=\` | Greater than or equal to |

\`\`\`sql
SELECT * FROM products WHERE price > 50;
SELECT * FROM products WHERE price <= 20;
\`\`\`

### Combining Conditions with AND / OR

Use \`AND\` when both conditions must be true. Use \`OR\` when at least one must be true:

\`\`\`sql
-- Both conditions must be true
SELECT * FROM products WHERE category = 'Electronics' AND price < 500;

-- At least one condition must be true
SELECT * FROM products WHERE category = 'Kitchen' OR category = 'Office';
\`\`\`

When mixing \`AND\` and \`OR\`, use parentheses to make the logic explicit:

\`\`\`sql
SELECT * FROM products
WHERE (category = 'Electronics' OR category = 'Kitchen')
  AND price < 100;
\`\`\`

### The IN Operator

\`IN\` checks whether a value matches any value in a list. It is a cleaner alternative to multiple \`OR\` conditions:

\`\`\`sql
SELECT * FROM products WHERE category IN ('Electronics', 'Kitchen', 'Office');
\`\`\`

This is equivalent to:

\`\`\`sql
SELECT * FROM products
WHERE category = 'Electronics'
   OR category = 'Kitchen'
   OR category = 'Office';
\`\`\`

### The BETWEEN Operator

\`BETWEEN\` checks whether a value falls within a range (inclusive on both ends):

\`\`\`sql
SELECT * FROM products WHERE price BETWEEN 10 AND 100;
\`\`\`

This is equivalent to \`price >= 10 AND price <= 100\`.

### Pattern Matching with LIKE

\`LIKE\` matches text against a pattern using two wildcards:

| Wildcard | Meaning |
|----------|---------|
| \`%\` | Any sequence of zero or more characters |
| \`_\` | Exactly one character |

\`\`\`sql
-- Products starting with 'C'
SELECT * FROM products WHERE name LIKE 'C%';

-- Products ending with 'er'
SELECT * FROM products WHERE name LIKE '%er';

-- Products with exactly 3 characters
SELECT * FROM products WHERE name LIKE '___';
\`\`\`

> **Tip:** \`LIKE\` is case-sensitive in PostgreSQL. Use \`ILIKE\` for case-insensitive matching: \`WHERE name ILIKE '%coffee%'\`.

### NULL Checks

\`NULL\` represents the absence of a value. You cannot use \`=\` to check for \`NULL\`. Use \`IS NULL\` or \`IS NOT NULL\` instead:

\`\`\`sql
SELECT * FROM products WHERE category IS NOT NULL;
\`\`\`

### Your Task

Find all products in the \`'Electronics'\` category with a price between 50 and 1000.`,

  starterCode: `-- Find Electronics products priced between 50 and 1000
SELECT * FROM products
WHERE`,

  solution: `SELECT * FROM products
WHERE category = 'Electronics'
  AND price BETWEEN 50 AND 1000;`,

  tests: [
    {
      name: "returns exactly 2 products (Electronics between 50 and 1000)",
      expected: '{"type":"rowCount","value":2}',
    },
  ],
};
