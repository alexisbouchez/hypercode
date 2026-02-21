import type { Lesson } from "../../types";

export const intro: Lesson = {
  id: "intro",
  title: "What is Redis?",
  chapterId: "strings",
  content: `## What is Redis?

Redis (**Re**mote **Di**ctionary **S**erver) is an in-memory data structure store. Unlike traditional databases that store data on disk, Redis keeps all data in RAM — making it orders of magnitude faster than disk-based databases for read and write operations.

### Why Redis?

- **Blazing fast** — sub-millisecond response times, hundreds of thousands of operations per second
- **Versatile** — stores strings, lists, sets, hashes, sorted sets, streams, and more
- **Persistent** — optional disk persistence via RDB snapshots and AOF logging
- **Atomic** — all operations are atomic; no race conditions for single commands
- **Pub/Sub** — built-in messaging between clients

### Where Redis is Used

Redis is most commonly used for:

- **Caching** — store expensive query results in memory; retrieve them in microseconds
- **Session storage** — web sessions that expire automatically
- **Rate limiting** — count requests per user per time window
- **Leaderboards** — sorted sets make ranking trivial
- **Message queues** — lists as FIFO queues; streams as durable logs
- **Real-time analytics** — HyperLogLog for approximate counts

Companies using Redis in production: Twitter, GitHub, Stack Overflow, Pinterest, Snapchat, Craigslist, and many others.

### The Redis CLI Format

In this course, you write Redis commands one per line — the same syntax as the \`redis-cli\` tool:

\`\`\`
SET name "Alice"
GET name
\`\`\`

Commands are case-insensitive (\`SET\` = \`set\` = \`Set\`). Keys and values are case-sensitive.

### Core Commands: SET, GET, DEL, EXISTS

\`\`\`
SET key value        -- store a string value
GET key              -- retrieve it ("value" or (nil))
DEL key [key ...]   -- delete one or more keys, returns count deleted
EXISTS key [key ...] -- 1 if exists, 0 if not (sums for multiple keys)
\`\`\`

Example:
\`\`\`
SET greeting "Hello, Redis!"
GET greeting
DEL greeting
GET greeting
\`\`\`

Output:
\`\`\`
OK
"Hello, Redis!"
(integer) 1
(nil)
\`\`\`

### Your Task

Set a key \`city\` to \`"Tokyo"\`, then retrieve it with GET.`,

  starterCode: `# Set city to "Tokyo" and retrieve it
SET city "Tokyo"
GET city`,

  solution: `SET city "Tokyo"
GET city`,

  tests: [
    {
      name: "GET returns Tokyo",
      expected: '{"type":"lastLine","value":"\\"Tokyo\\""}',
    },
    {
      name: "output contains OK",
      expected: '{"type":"contains","value":"OK"}',
    },
  ],
};
