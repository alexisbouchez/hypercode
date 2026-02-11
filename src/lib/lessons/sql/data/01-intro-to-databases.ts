import type { Lesson } from "../../types";

export const introToDatabases: Lesson = {
  id: "intro-to-databases",
  title: "Introduction to Databases",
  chapterId: "sql-basics",
  content: `## What is a Database?

A database is an organized collection of data stored and accessed electronically. At its core, a database lets you **store**, **retrieve**, **update**, and **delete** data efficiently and reliably.

You interact with databases every day without realizing it. When you log in to a website, browse products in an online store, or check your bank balance, a database is doing the heavy lifting behind the scenes.

### Why PostgreSQL?

PostgreSQL (often called "Postgres") is one of the most powerful open-source relational database systems in the world. It has over 35 years of active development and a reputation for reliability, correctness, and extensibility.

Key strengths of PostgreSQL:

| Feature | Description |
|---------|-------------|
| **ACID Compliance** | Guarantees data integrity even during crashes |
| **SQL Standard** | Closely follows the SQL specification |
| **Extensibility** | Custom types, functions, operators, and extensions |
| **JSON Support** | First-class support for JSON and JSONB data |
| **Concurrency** | MVCC allows reads and writes without blocking each other |
| **Open Source** | Free to use, modify, and distribute |

### Tables, Rows, and Columns

Relational databases organize data into **tables**. A table is a structured collection of related data, much like a spreadsheet.

- **Table**: A named collection of data about a specific entity (e.g., \`products\`, \`users\`, \`orders\`)
- **Column**: A single field within a table, with a name and a data type (e.g., \`name TEXT\`, \`price DECIMAL\`)
- **Row**: A single record in the table, containing one value for each column

Here is an example of what a \`products\` table might look like:

| id | name | price | category |
|----|------|-------|----------|
| 1 | Laptop | 999.99 | Electronics |
| 2 | Headphones | 79.99 | Electronics |
| 3 | Coffee Maker | 49.99 | Kitchen |

Each row represents one product. Each column represents an attribute of that product.

### SQL: The Language of Databases

SQL (Structured Query Language) is the standard language for interacting with relational databases. You use SQL to:

- **Query** data with \`SELECT\`
- **Insert** new records with \`INSERT\`
- **Update** existing records with \`UPDATE\`
- **Delete** records with \`DELETE\`
- **Define** table structures with \`CREATE TABLE\`, \`ALTER TABLE\`, and \`DROP TABLE\`

> **Tip:** SQL keywords like \`SELECT\` and \`FROM\` are case-insensitive. \`SELECT\`, \`select\`, and \`Select\` all work the same way. By convention, most developers write SQL keywords in uppercase for readability.

### Your Task

Run the query below to see all the products in the database. This is the sandbox you will use throughout the SQL course.`,

  starterCode: `SELECT * FROM products;`,

  solution: `SELECT * FROM products;`,

  tests: [
    {
      name: "returns all products",
      expected: '{"type":"rowCount","value":8}',
    },
  ],
};
