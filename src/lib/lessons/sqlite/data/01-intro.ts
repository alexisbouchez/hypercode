import type { Lesson } from "../../types";

export const intro: Lesson = {
  id: "intro",
  title: "What is SQLite?",
  chapterId: "getting-started",
  content: `## What is SQLite?

SQLite is the most widely deployed database engine in the world. Unlike client-server databases such as PostgreSQL or MySQL, SQLite is a **library** embedded directly into your application. There is no separate server process, no configuration, and no installation.

### Where is SQLite Used?

SQLite is literally everywhere:

- **iOS and Android** — Every app that stores local data uses SQLite. Your contacts, messages, and photos are stored in SQLite databases.
- **Firefox** — Bookmarks, history, cookies, and extensions are SQLite databases.
- **Python** — The \`sqlite3\` module is part of the standard library. No installation needed.
- **Bun** — Built-in \`bun:sqlite\` with a fast native API.
- **Browsers** — Web SQL (deprecated but widely used), IndexedDB internals.
- **Embedded systems** — Airplanes, cars, medical devices.

According to its author D. Richard Hipp, there are likely more than **one trillion** SQLite databases currently in active use.

### SQLite vs. Traditional Databases

| Feature | SQLite | PostgreSQL/MySQL |
|---------|--------|-----------------|
| Server | None (embedded) | Separate process |
| Setup | Zero | Installation + config |
| File | Single \`.db\` file | Multiple data files |
| Concurrency | Single writer | Multiple writers |
| Scale | Millions of rows | Billions of rows |
| Use case | Local/embedded | Server/multi-user |

SQLite is not trying to compete with server databases. It excels at a different set of problems: local storage, embedded applications, testing, prototyping, and small-to-medium datasets.

### SQL Compatibility

SQLite supports most of standard SQL. The syntax you learn here — \`SELECT\`, \`WHERE\`, \`JOIN\`, \`GROUP BY\`, window functions, CTEs — works in PostgreSQL and MySQL too, with minor variations.

The key SQLite-specific things to know:
- Type system is **flexible** (type affinity rather than strict types)
- Uses \`INTEGER PRIMARY KEY\` instead of \`SERIAL\`
- Has \`json_extract()\` for JSON, \`pragma_table_info()\` for schema inspection

### The Exercise Database

Throughout this course, you will work with a sample store database:

| Table | Columns |
|-------|---------|
| \`products\` | id, name, price, category |
| \`users\` | id, name, email |
| \`customers\` | id, name, email |
| \`orders\` | id, customer_id, product_id, quantity, total |

### Your Task

Select all columns from the \`products\` table to see what data we are working with.`,

  starterCode: `-- Select everything from products
SELECT`,

  solution: `SELECT * FROM products;`,

  tests: [
    {
      name: "returns all 8 products",
      expected: '{"type":"rowCount","value":8}',
    },
    {
      name: "returns name and price columns",
      expected: '{"type":"contains","columns":["name","price"]}',
    },
  ],
};
