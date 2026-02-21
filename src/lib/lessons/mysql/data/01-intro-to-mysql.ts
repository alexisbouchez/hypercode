import type { Lesson } from "../../types";

export const introToMysql: Lesson = {
  id: "intro-to-mysql",
  title: "Introduction to MySQL",
  chapterId: "getting-started",
  content: `## What is MySQL?

MySQL is the world's most popular open-source relational database management system (RDBMS). It was created in 1995 by Michael Widenius and David Axmark, acquired by Sun Microsystems in 2008, and then by Oracle in 2010. Despite corporate ownership, MySQL remains open-source under the GPL license, and a community fork called **MariaDB** was created by the original authors to keep it fully open.

### Where is MySQL Used?

MySQL powers a significant portion of the internet:

- **WordPress** — The most popular CMS in the world uses MySQL by default.
- **Facebook** — Started on MySQL and still uses it for some workloads at massive scale.
- **Twitter** — Used MySQL for core data storage in its early years.
- **Wikipedia** — The encyclopedia runs on MySQL (via MariaDB).
- **Airbnb, GitHub, Netflix** — All use MySQL or MariaDB in production.

The classic **LAMP stack** (Linux, Apache, MySQL, PHP) built the early web and remains in widespread use today.

### Relational Databases

MySQL is a **relational** database. Data is organized into **tables**, which are like spreadsheets with rows and columns. Tables relate to each other through **foreign keys**.

For example, a store database might have:

| Table | What it stores |
|-------|---------------|
| \`products\` | Item catalog with prices |
| \`customers\` | People who buy things |
| \`orders\` | Purchase transactions |
| \`order_items\` | Line items within each order |

This structure avoids duplicating data. Instead of storing a customer's name on every order, you store a \`customer_id\` that points back to the \`customers\` table.

### SQL — Structured Query Language

You interact with MySQL using **SQL** (Structured Query Language). SQL is a declarative language: you describe *what* you want, and the database figures out *how* to get it.

The most fundamental SQL statement is \`SELECT\`:

\`\`\`sql
SELECT * FROM products;
\`\`\`

This reads as: "Give me all columns (\`*\`) from the \`products\` table."

### MySQL vs. Other Databases

| Feature | MySQL | PostgreSQL | SQLite |
|---------|-------|-----------|--------|
| License | GPL / Commercial | PostgreSQL (BSD-like) | Public domain |
| Server | Yes | Yes | No (embedded) |
| Speed | Very fast reads | Balanced | Fastest for local use |
| JSON | Supported | Native (JSONB) | Limited |
| Full-text search | Built-in | Built-in (tsvector) | FTS extension |
| Use case | Web apps, OLTP | Analytics, complex queries | Local/embedded |

### The Exercise Database

Throughout this course, you will work with a store database containing four tables:

| Table | Columns |
|-------|---------|
| \`products\` | id, name, price, category, stock |
| \`customers\` | id, name, email, city |
| \`orders\` | id, customer_id, total, status, order_date |
| \`order_items\` | id, order_id, product_id, quantity, unit_price |

### Your Task

Select all columns from the \`products\` table to see what data you are working with.`,

  starterCode: `-- Select all columns from the products table
SELECT`,

  solution: `SELECT * FROM products;`,

  tests: [
    {
      name: "returns all 10 products",
      expected: '{"type":"rowCount","value":10}',
    },
    {
      name: "returns name, price, and category columns",
      expected: '{"type":"contains","columns":["name","price","category"]}',
    },
    {
      name: "includes Electronics category",
      expected: '{"type":"contains","value":"Electronics"}',
    },
    {
      name: "includes Laptop product",
      expected: '{"type":"contains","value":"Laptop"}',
    },
  ],
};
