import type { Lesson } from "../../types";

export const selfAndCrossJoins: Lesson = {
  id: "self-and-cross-joins",
  title: "Self & Cross Joins",
  chapterId: "joins",
  content: `## Joining a Table with Itself

Not all joins are between different tables. Sometimes the relationship you need to express exists within a single table. A **self join** is a regular join where both sides reference the same table.

### Self Joins

A self join requires table aliases to distinguish the two "copies" of the same table:

\`\`\`sql
SELECT a.name AS product_a, b.name AS product_b, a.category
FROM products a
INNER JOIN products b ON a.category = b.category AND a.id < b.id;
\`\`\`

This finds all pairs of products in the same category. The condition \`a.id < b.id\` ensures each pair appears only once (not both "Laptop, Headphones" and "Headphones, Laptop") and excludes a product being paired with itself.

### Finding Duplicates

Self joins are useful for finding duplicate or near-duplicate data:

\`\`\`sql
SELECT a.name, b.name, a.price
FROM products a
INNER JOIN products b ON a.price = b.price AND a.id < b.id;
\`\`\`

This finds products with the same price. If no products share a price, the result is empty.

### Comparing Rows

You can use self joins to compare rows against each other, such as finding products that are more expensive than another product in the same category:

\`\`\`sql
SELECT expensive.name AS expensive_product,
       cheap.name AS cheaper_product,
       expensive.price - cheap.price AS price_difference
FROM products expensive
INNER JOIN products cheap
  ON expensive.category = cheap.category
  AND expensive.price > cheap.price;
\`\`\`

> **Tip:** Use meaningful aliases in self joins. Names like \`a\` and \`b\` work for simple cases, but \`expensive\` and \`cheap\` or \`parent\` and \`child\` make the query's intent immediately clear.

### Hierarchical Data

Self joins are the classic way to query hierarchical data stored in a single table. Consider an \`employees\` table with a \`manager_id\` column that references \`id\` in the same table:

\`\`\`sql
SELECT e.name AS employee, m.name AS manager
FROM employees e
LEFT JOIN employees m ON e.manager_id = m.id;
\`\`\`

The \`LEFT JOIN\` ensures top-level employees (whose \`manager_id\` is \`NULL\`) still appear in the result.

### The CROSS JOIN

A \`CROSS JOIN\` produces the **cartesian product** of two tables: every row from the first table paired with every row from the second. No \`ON\` condition is used:

\`\`\`sql
SELECT p.name, c.name AS customer
FROM products p
CROSS JOIN customers c;
\`\`\`

If \`products\` has 8 rows and \`customers\` has 3 rows, the result has 24 rows (8 * 3).

> **Warning:** Cross joins can produce enormous result sets. A cross join of two tables with 10,000 rows each produces 100,000,000 rows. Use them deliberately and only when the cartesian product is what you actually need.

### CROSS JOIN Use Cases

Despite the size warning, cross joins have legitimate uses:

- **Generating combinations**: pairing every size with every color for a product catalog
- **Creating date scaffolds**: crossing a list of dates with a list of categories for reporting
- **Testing and data generation**: producing test data from smaller seed tables

\`\`\`sql
-- Generate all category-customer combinations
SELECT DISTINCT p.category, c.name
FROM products p
CROSS JOIN customers c;
\`\`\`

### Implicit Cross Join Syntax

An older SQL syntax produces the same result by listing tables separated by commas:

\`\`\`sql
-- Implicit cross join (equivalent to CROSS JOIN)
SELECT p.name, c.name
FROM products p, customers c;
\`\`\`

This syntax is still valid but less explicit. Prefer the \`CROSS JOIN\` keyword to make your intent clear.

### Your Task

Find all pairs of products that are in the same category but are different products. Select the names of both products and the category they share.`,

  starterCode: `-- Find pairs of products in the same category
SELECT`,

  solution: `SELECT a.name AS product_a, b.name AS product_b, a.category
FROM products a
INNER JOIN products b ON a.category = b.category AND a.id < b.id;`,

  tests: [
    {
      name: "returns product pairs with shared category (5 pairs)",
      expected: '{"type":"rowCount","value":5}',
    },
    {
      name: "returns product_a, product_b, and category columns",
      expected: '{"type":"contains","columns":["category"]}',
    },
  ],
};
