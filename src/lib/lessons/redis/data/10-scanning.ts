import type { Lesson } from "../../types";

export const scanning: Lesson = {
  id: "scanning",
  title: "SCAN — Iterating Keys",
  chapterId: "key-ops",
  content: `## SCAN vs KEYS

\`KEYS pattern\` is simple but dangerous in production: it blocks the entire Redis server while it scans all keys. For a database with millions of keys, this can take seconds and freeze your application.

\`SCAN\` solves this by iterating keys in **batches** without blocking:

### SCAN — Cursor-Based Iteration

\`\`\`
SCAN cursor [MATCH pattern] [COUNT count]
\`\`\`

- \`cursor\` — start with \`0\`, use the returned cursor for the next call
- Returns \`0\` as the cursor when the full iteration is complete
- \`MATCH\` — filter keys by pattern (applied after fetching, not before)
- \`COUNT\` — hint for how many keys to check per call (not a guarantee)

**Example — iterate all keys:**
\`\`\`
SCAN 0
\`\`\`

Output:
\`\`\`
1) "0"        -- next cursor (0 means scan complete)
2) 1) "key1"
   2) "key2"
   3) "key3"
\`\`\`

**Example — iterate with pattern:**
\`\`\`
SET user:1 "Alice"
SET user:2 "Bob"
SET product:1 "Laptop"

SCAN 0 MATCH user:*
\`\`\`

Output:
\`\`\`
1) "0"
2) 1) "user:1"
   2) "user:2"
\`\`\`

### How Cursor Iteration Works

In real Redis, SCAN returns a cursor that you use in subsequent calls:

\`\`\`bash
cursor = 0
do:
  cursor, keys = SCAN cursor MATCH pattern COUNT 100
  process(keys)
while cursor != 0
\`\`\`

This emulator always returns cursor \`0\` (scan complete in one call), matching real Redis behavior for small datasets.

### HSCAN, SSCAN, ZSCAN

These work similarly but iterate over hash fields, set members, and sorted set members:

\`\`\`
HSET user:1 name "Alice" email "a@b.com" age "30"
HSCAN user:1 0

SADD tags "redis" "cache" "db"
SSCAN tags 0

ZADD scores 100 "Alice" 90 "Bob"
ZSCAN scores 0
\`\`\`

### Production Pattern

In application code with ioredis or redis-py:
\`\`\`python
# Python: iterate all keys matching pattern
for key in redis.scan_iter("user:*"):
    print(key)
\`\`\`

### Your Task

Set three keys: \`session:abc\` → \`"user1"\`, \`session:def\` → \`"user2"\`, \`cache:page1\` → \`"html"\`. Use SCAN with MATCH to find only session keys.`,

  starterCode: `# Set some keys
SET session:abc "user1"
SET session:def "user2"
SET cache:page1 "html"

# Scan for session keys only
SCAN 0 MATCH session:*`,

  solution: `SET session:abc "user1"
SET session:def "user2"
SET cache:page1 "html"
SCAN 0 MATCH session:*`,

  tests: [
    {
      name: "output contains session:abc",
      expected: '{"type":"contains","value":"\\"session:abc\\""}',
    },
    {
      name: "output contains session:def",
      expected: '{"type":"contains","value":"\\"session:def\\""}',
    },
    {
      name: "cursor returned is 0",
      expected: '{"type":"contains","value":"\\"0\\""}',
    },
  ],
};
