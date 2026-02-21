import type { Lesson } from "../../types";

export const sets: Lesson = {
  id: "sets",
  title: "Sets",
  chapterId: "collections",
  content: `## Redis Sets

A Redis set is an unordered collection of unique strings. Duplicate members are automatically ignored. Sets are ideal for tracking unique items, managing relationships, and performing set operations.

### SADD — Add Members

\`\`\`
SADD online "Alice" "Bob" "Charlie"
SADD online "Alice"    -- duplicate, ignored
SCARD online            -- count = 3
\`\`\`

Output:
\`\`\`
(integer) 3
(integer) 0
(integer) 3
\`\`\`

### SMEMBERS — Get All Members

Returns all members in **sorted order** (alphabetically in this emulator):

\`\`\`
SMEMBERS online
\`\`\`

Output:
\`\`\`
1) "Alice"
2) "Bob"
3) "Charlie"
\`\`\`

### SISMEMBER — Check Membership

\`\`\`
SISMEMBER online "Alice"    -- 1 (true)
SISMEMBER online "Dave"     -- 0 (false)
\`\`\`

### SCARD — Set Cardinality (Size)

\`\`\`
SCARD online
\`\`\`

### SREM — Remove Members

\`\`\`
SREM online "Charlie"
SCARD online    -- now 2
\`\`\`

### Set Operations — SUNION, SINTER, SDIFF

These are some of Redis's most powerful features:

\`\`\`
SADD team_a "Alice" "Bob" "Charlie"
SADD team_b "Bob" "Charlie" "Dave"

SUNION team_a team_b      -- all unique members across both sets
SINTER team_a team_b      -- members in BOTH sets
SDIFF team_a team_b       -- members in team_a but NOT team_b
\`\`\`

Output:
\`\`\`
1) "Alice"
2) "Bob"
3) "Charlie"
4) "Dave"
---
1) "Bob"
2) "Charlie"
---
1) "Alice"
\`\`\`

### Real-World Patterns

**Who follows both Alice and Bob?**
\`\`\`
SADD followers:alice "u1" "u2" "u3"
SADD followers:bob "u2" "u3" "u4"
SINTER followers:alice followers:bob    -- u2, u3
\`\`\`

**Unique visitors today:**
\`\`\`
SADD visitors:2024-01-15 "user1" "user2" "user1"
SCARD visitors:2024-01-15    -- 2 unique
\`\`\`

### Your Task

Create two sets: \`python_devs\` with members \`"Alice"\`, \`"Bob"\`, \`"Charlie"\` and \`"js_devs"\` with members \`"Bob"\`, \`"Charlie"\`, \`"Dave"\`. Use SINTER to find developers who know both languages.`,

  starterCode: `# Add Python developers
SADD python_devs "Alice" "Bob" "Charlie"

# Add JS developers
SADD js_devs "Bob" "Charlie" "Dave"

# Find developers who know both
SINTER python_devs js_devs`,

  solution: `SADD python_devs "Alice" "Bob" "Charlie"
SADD js_devs "Bob" "Charlie" "Dave"
SINTER python_devs js_devs`,

  tests: [
    {
      name: "SINTER result contains Bob",
      expected: '{"type":"contains","value":"\\"Bob\\""}',
    },
    {
      name: "SINTER result contains Charlie",
      expected: '{"type":"contains","value":"\\"Charlie\\""}',
    },
    {
      name: "output has 4 lines (2 SADD + 2 SINTER results)",
      expected: '{"type":"lineCount","value":4}',
    },
  ],
};
