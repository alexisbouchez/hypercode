import type { Lesson } from "../../types";

export const databaseDesign: Lesson = {
  id: "database-design",
  title: "Database Design",
  chapterId: "performance",
  content: `## Database Normalization

Normalization is the process of organizing a database to reduce redundancy and improve data integrity. It involves splitting data across multiple related tables so that each fact is stored in exactly one place.

> Designing a starship requires careful planning of how all systems --- life support, propulsion, weapons --- connect and depend on each other. Good database design demands the same discipline.

### First Normal Form (1NF)

A table is in 1NF when:

- Each column contains **atomic** (indivisible) values.
- There are no repeating groups or arrays stored in a single column.
- Each row is unique (usually enforced by a primary key).

**Violates 1NF:**

| id | name | phone_numbers |
|----|------|---------------|
| 1 | Alice | 555-0100, 555-0101 |
| 2 | Bob | 555-0200 |

The \`phone_numbers\` column contains multiple values. To fix this, move phone numbers to a separate table:

**Satisfies 1NF:**

| id | name |
|----|------|
| 1 | Alice |
| 2 | Bob |

| id | customer_id | phone_number |
|----|-------------|--------------|
| 1 | 1 | 555-0100 |
| 2 | 1 | 555-0101 |
| 3 | 2 | 555-0200 |

### Second Normal Form (2NF)

A table is in 2NF when:

- It satisfies 1NF.
- Every non-key column depends on the **entire** primary key, not just part of it.

This mainly applies to tables with composite primary keys. If a column depends on only one part of the key, it should be moved to its own table.

**Violates 2NF (composite key: order_id + product_id):**

| order_id | product_id | product_name | quantity |
|----------|------------|--------------|----------|
| 1 | 101 | Laptop | 1 |
| 1 | 102 | Headphones | 2 |

\`product_name\` depends only on \`product_id\`, not the full key. It should live in a separate \`products\` table.

### Third Normal Form (3NF)

A table is in 3NF when:

- It satisfies 2NF.
- No non-key column depends on another non-key column (no **transitive dependencies**).

**Violates 3NF:**

| id | name | department_id | department_name |
|----|------|---------------|-----------------|
| 1 | Alice | 10 | Engineering |
| 2 | Bob | 10 | Engineering |

\`department_name\` depends on \`department_id\`, not on the primary key \`id\`. The fix is a separate \`departments\` table:

| id | name | department_id |
|----|------|---------------|
| 1 | Alice | 10 |
| 2 | Bob | 10 |

| id | name |
|----|------|
| 10 | Engineering |

### Denormalized vs Normalized

Consider an e-commerce order stored in a single denormalized table:

**Denormalized (single table):**

| order_id | customer_name | customer_email | product_name | product_price | quantity | total |
|----------|---------------|----------------|--------------|---------------|----------|-------|
| 1 | Alice | alice@example.com | Laptop | 999.99 | 1 | 999.99 |
| 2 | Alice | alice@example.com | Headphones | 79.99 | 2 | 159.98 |

Problems with this design:
- **Update anomaly**: If Alice changes her email, every row must be updated.
- **Insert anomaly**: You cannot add a new customer until they place an order.
- **Delete anomaly**: If you delete Alice's orders, you lose her customer record.

**Normalized (multiple tables):**

\`customers\` table:

| id | name | email |
|----|------|-------|
| 1 | Alice | alice@example.com |

\`products\` table:

| id | name | price |
|----|------|-------|
| 1 | Laptop | 999.99 |
| 2 | Headphones | 79.99 |

\`orders\` table:

| id | customer_id | product_id | quantity | total |
|----|-------------|------------|----------|-------|
| 1 | 1 | 1 | 1 | 999.99 |
| 2 | 1 | 2 | 2 | 159.98 |

Each fact is stored once. Changes propagate correctly. Referential integrity is maintained through foreign keys.

### When to Denormalize

Normalization is not always the right choice. In some cases, controlled denormalization improves performance:

- **Read-heavy workloads**: If a query constantly joins five tables, storing precomputed data in one table avoids the join overhead.
- **Reporting and analytics**: Aggregated or materialized views are often denormalized for speed.
- **Caching columns**: Storing a derived value (like \`order_total\`) avoids recalculating it on every read.

> **Tip:** Start with a normalized design. Denormalize only when you have measured a performance problem that normalization causes. Premature denormalization trades correctness for speed you may not need.

### Naming Conventions

Consistent naming makes a schema easier to navigate:

| Convention | Example | Notes |
|------------|---------|-------|
| Table names: plural, snake_case | \`products\`, \`order_items\` | Represents a collection of entities |
| Column names: singular, snake_case | \`first_name\`, \`created_at\` | Represents a single attribute |
| Primary key: \`id\` | \`id SERIAL PRIMARY KEY\` | Simple and universally understood |
| Foreign key: \`table_id\` | \`customer_id\`, \`product_id\` | References \`id\` in the related table |
| Indexes: \`idx_table_column\` | \`idx_products_category\` | Describes what is indexed |
| Boolean columns: \`is_\` or \`has_\` prefix | \`is_active\`, \`has_shipped\` | Clearly indicates a boolean |
| Timestamps: \`_at\` suffix | \`created_at\`, \`updated_at\` | Standard temporal column naming |

### Schema Design Tips

1. **Always define primary keys.** Every table should have a primary key. Use \`SERIAL\` or \`BIGSERIAL\` for auto-incrementing integers, or \`UUID\` for distributed systems.

2. **Use foreign keys.** Foreign key constraints enforce referential integrity and prevent orphaned records:

\`\`\`sql
CREATE TABLE orders (
  id SERIAL PRIMARY KEY,
  customer_id INTEGER NOT NULL REFERENCES customers(id),
  product_id INTEGER NOT NULL REFERENCES products(id),
  quantity INTEGER NOT NULL CHECK (quantity > 0),
  total DECIMAL(10,2) NOT NULL
);
\`\`\`

3. **Add NOT NULL where appropriate.** Columns that must always have a value should be \`NOT NULL\`. This prevents accidental gaps in your data.

4. **Use CHECK constraints.** Enforce data rules at the database level:

\`\`\`sql
ALTER TABLE products ADD CONSTRAINT price_positive CHECK (price > 0);
\`\`\`

5. **Plan for time.** Include \`created_at\` and \`updated_at\` columns in tables that track records over time. Use \`TIMESTAMPTZ\` (timestamp with time zone) to avoid timezone issues.

6. **Think about access patterns.** Design tables around how data will be queried, not just how it is conceptually organized.

> **Tip:** A well-designed schema is the foundation of a reliable application. Spending time on design upfront saves significant effort debugging data issues later.

### Your Task

Write a query that shows the normalized structure by joining customers with their orders and the product names.`,

  starterCode: `-- Join customers, orders, and products to show the normalized structure
SELECT
  c.name,
`,

  solution: `SELECT
  c.name,
  p.name AS product_name,
  o.quantity,
  o.total
FROM customers c
JOIN orders o ON o.customer_id = c.id
JOIN products p ON o.product_id = p.id;`,

  tests: [
    {
      name: "returns name and total columns",
      expected: '{"type":"contains","columns":["name","total"]}',
    },
    {
      name: "returns all 3 orders",
      expected: '{"type":"rowCount","value":3}',
    },
  ],
};
