import type { Lesson } from "../../types";

export const filtering: Lesson = {
  id: "filtering",
  title: "Filtering with WHERE",
  chapterId: "getting-started",
  content: `## The WHERE Clause

The \`WHERE\` clause filters rows based on one or more conditions. Only rows where the condition evaluates to true are included in the result.

\`\`\`sql
SELECT * FROM products WHERE category = 'Electronics';
\`\`\`

### Comparison Operators

| Operator | Meaning |
|----------|---------|
| \`=\` | Equal |
| \`<>\` or \`!=\` | Not equal |
| \`<\` | Less than |
| \`>\` | Greater than |
| \`<=\` | Less than or equal |
| \`>=\` | Greater than or equal |

\`\`\`sql
SELECT name, price FROM products WHERE price < 20;
SELECT name, price FROM products WHERE price >= 50;
\`\`\`

### AND, OR, NOT

Combine conditions with logical operators:

\`\`\`sql
-- Both conditions must be true
SELECT * FROM products WHERE category = 'Office' AND price < 20;

-- Either condition must be true
SELECT * FROM products WHERE category = 'Kitchen' OR category = 'Accessories';

-- Negates the condition
SELECT * FROM products WHERE NOT category = 'Electronics';
\`\`\`

Use parentheses to control precedence:

\`\`\`sql
SELECT * FROM products
WHERE (category = 'Office' OR category = 'Kitchen') AND price < 40;
\`\`\`

### LIKE — Pattern Matching

\`LIKE\` matches text patterns using wildcards:
- \`%\` — matches any sequence of characters (including none)
- \`_\` — matches exactly one character

\`\`\`sql
-- Products whose name starts with 'C'
SELECT * FROM products WHERE name LIKE 'C%';

-- Products with 'Maker' anywhere in the name
SELECT * FROM products WHERE name LIKE '%Maker%';
\`\`\`

SQLite's \`LIKE\` is **case-insensitive** for ASCII characters by default.

### IN — Match a List

\`IN\` checks if a value matches any value in a list:

\`\`\`sql
SELECT * FROM products WHERE category IN ('Electronics', 'Kitchen');
\`\`\`

This is equivalent to:
\`\`\`sql
SELECT * FROM products WHERE category = 'Electronics' OR category = 'Kitchen';
\`\`\`

### BETWEEN — Range Check

\`BETWEEN\` checks if a value falls within an inclusive range:

\`\`\`sql
SELECT * FROM products WHERE price BETWEEN 10 AND 60;
\`\`\`

### IS NULL / IS NOT NULL

Check for missing values:

\`\`\`sql
SELECT * FROM orders WHERE customer_id IS NULL;
SELECT * FROM products WHERE category IS NOT NULL;
\`\`\`

Never use \`= NULL\` — it always returns false. Always use \`IS NULL\`.

### Your Task

Select the \`name\` and \`price\` of all products in the \`'Office'\` category with a price less than \`20\`.`,

  starterCode: `-- Select name and price of Office products under $20
SELECT name, price FROM products
WHERE`,

  solution: `SELECT name, price FROM products
WHERE category = 'Office' AND price < 20;`,

  tests: [
    {
      name: "returns name and price columns",
      expected: '{"type":"contains","columns":["name","price"]}',
    },
    {
      name: "returns 2 matching products",
      expected: '{"type":"rowCount","value":2}',
    },
  ],
};
