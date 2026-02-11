import type { Lesson } from "../../types";

export const indexesAndOptimization: Lesson = {
  id: "indexes-and-optimization",
  title: "Indexes & Query Optimization",
  chapterId: "performance",
  content: `## What is an Index?

An index is a data structure that speeds up data retrieval at the cost of additional storage and slower writes. Without an index, PostgreSQL must scan every row in a table to find matches (a **sequential scan**). With an index, it can jump directly to the relevant rows.

Think of an index like the index at the back of a book. Instead of reading every page to find a topic, you look it up in the index and go straight to the right page.

### Index Types in PostgreSQL

PostgreSQL supports several index types, each optimized for different query patterns:

| Type | Best For | Example Use |
|------|----------|-------------|
| **B-tree** | Equality and range queries (\`=\`, \`<\`, \`>\`, \`BETWEEN\`) | Most columns; the default index type |
| **Hash** | Equality comparisons only (\`=\`) | Exact lookups on large values |
| **GIN** | Full-text search, JSONB, arrays | \`@>\`, \`?\`, \`@@\` operators |
| **GiST** | Geometric data, range types, full-text | Spatial queries, nearest-neighbor |
| **BRIN** | Very large tables with naturally ordered data | Timestamp columns on append-only tables |

### Creating an Index

The basic syntax for creating an index is:

\`\`\`sql
CREATE INDEX index_name ON table_name (column_name);
\`\`\`

For example, to create an index on the \`category\` column of the \`products\` table:

\`\`\`sql
CREATE INDEX idx_products_category ON products (category);
\`\`\`

You can also create a **unique index** that enforces uniqueness:

\`\`\`sql
CREATE UNIQUE INDEX idx_users_email ON users (email);
\`\`\`

Multi-column indexes cover queries that filter on multiple columns:

\`\`\`sql
CREATE INDEX idx_products_category_price ON products (category, price);
\`\`\`

The column order matters. This index is most effective for queries that filter on \`category\` or on both \`category\` and \`price\`. It is less useful for queries that filter only on \`price\`.

> **Tip:** Use a consistent naming convention for indexes. A common pattern is \`idx_tablename_columnname\`. This makes it easy to identify what each index covers.

### EXPLAIN and EXPLAIN ANALYZE

\`EXPLAIN\` shows the query plan that PostgreSQL will use **without** running the query:

\`\`\`sql
EXPLAIN SELECT * FROM products WHERE category = 'Electronics';
\`\`\`

This shows whether PostgreSQL plans to use a sequential scan, an index scan, or another strategy.

\`EXPLAIN ANALYZE\` runs the query and shows the **actual** execution statistics:

\`\`\`sql
EXPLAIN ANALYZE SELECT * FROM products WHERE category = 'Electronics';
\`\`\`

The output includes the estimated and actual row counts, execution time, and the type of scan used.

### Reading Execution Plans

A typical plan looks like this:

\`\`\`
Seq Scan on products  (cost=0.00..1.10 rows=2 width=52) (actual time=0.012..0.014 rows=2 loops=1)
  Filter: (category = 'Electronics')
  Rows Removed by Filter: 6
Planning Time: 0.082 ms
Execution Time: 0.031 ms
\`\`\`

Key elements to look for:

- **Scan type**: \`Seq Scan\` (full table scan) vs \`Index Scan\` or \`Index Only Scan\` (uses an index)
- **cost**: Estimated startup cost and total cost (lower is better)
- **rows**: Estimated vs actual number of rows
- **Filter / Rows Removed**: How many rows were scanned but discarded

After creating an index, the plan might change to:

\`\`\`
Index Scan using idx_products_category on products  (cost=0.14..8.16 rows=2 width=52)
  Index Cond: (category = 'Electronics')
\`\`\`

This is typically faster because only matching rows are read from disk.

### Query Optimization Tips

**Select only the columns you need.** Avoid \`SELECT *\` in production queries. Fetching fewer columns means less data transferred and potentially allows index-only scans:

\`\`\`sql
-- Instead of this
SELECT * FROM products WHERE category = 'Electronics';

-- Prefer this
SELECT name, price FROM products WHERE category = 'Electronics';
\`\`\`

**Avoid functions on indexed columns.** Applying a function to an indexed column prevents the index from being used:

\`\`\`sql
-- This cannot use an index on the name column
SELECT * FROM products WHERE LOWER(name) = 'laptop';

-- Create a functional index instead
CREATE INDEX idx_products_name_lower ON products (LOWER(name));
\`\`\`

**Use EXISTS over IN for subqueries.** \`EXISTS\` short-circuits as soon as a match is found:

\`\`\`sql
-- Less efficient
SELECT * FROM customers WHERE id IN (SELECT customer_id FROM orders);

-- More efficient
SELECT * FROM customers c
WHERE EXISTS (SELECT 1 FROM orders o WHERE o.customer_id = c.id);
\`\`\`

**Batch operations.** When inserting or updating many rows, batch the operations instead of running them one at a time:

\`\`\`sql
-- Instead of multiple single inserts
INSERT INTO products (name, price, category) VALUES ('Item A', 10, 'Office');
INSERT INTO products (name, price, category) VALUES ('Item B', 20, 'Office');

-- Use a single multi-row insert
INSERT INTO products (name, price, category) VALUES
  ('Item A', 10, 'Office'),
  ('Item B', 20, 'Office');
\`\`\`

> **Warning:** Do not blindly add indexes to every column. Each index adds storage overhead and slows down \`INSERT\`, \`UPDATE\`, and \`DELETE\` operations. Profile your queries with \`EXPLAIN ANALYZE\` first, and only add indexes where they provide a measurable benefit.

### Your Task

Create an index on the \`products\` table for the \`category\` column.`,

  starterCode: `-- Create an index on products for the category column
CREATE INDEX`,

  solution: `CREATE INDEX idx_products_category ON products (category);`,

  tests: [
    {
      name: "creates an index on the category column",
      expected: '{"type":"custom"}',
      code: `{{USER_SQL}}\n---VALIDATE---\nSELECT indexname FROM pg_indexes WHERE tablename = 'products' AND indexdef LIKE '%category%';`,
    },
  ],
};
