import type { Lesson } from "../../types";

export const counters: Lesson = {
  id: "counters",
  title: "Counters",
  chapterId: "strings",
  content: `## Atomic Counters

Redis string commands include atomic integer operations. Because they are atomic, multiple clients can increment a counter simultaneously without race conditions — no transactions needed.

### INCR — Increment by 1

\`\`\`
SET pageviews 0
INCR pageviews
INCR pageviews
INCR pageviews
GET pageviews
\`\`\`

Output:
\`\`\`
OK
(integer) 1
(integer) 2
(integer) 3
"3"
\`\`\`

If the key doesn't exist, \`INCR\` starts from 0 and increments to 1.

### INCRBY — Increment by N

\`\`\`
INCRBY pageviews 10
\`\`\`

### DECR and DECRBY — Decrement

\`\`\`
DECR pageviews
DECRBY pageviews 5
\`\`\`

### INCRBYFLOAT — Floating-Point Increment

\`\`\`
SET price 10.50
INCRBYFLOAT price 2.25
GET price
\`\`\`

Output:
\`\`\`
OK
"12.75"
"12.75"
\`\`\`

### Pattern: Rate Limiting

The INCR pattern is ideal for rate limiting:

\`\`\`
# Allow 100 requests per minute per user
INCR rate:user123
EXPIRE rate:user123 60

# On each request:
# If GET rate:user123 > 100, reject the request
\`\`\`

The key expires automatically after 60 seconds, resetting the counter.

### Pattern: Page View Counters

\`\`\`
# Increment on each page load
INCR views:article:42

# Get current count
GET views:article:42
\`\`\`

### Pattern: Inventory Management

\`\`\`
SET inventory:item:5 100
DECRBY inventory:item:5 3
GET inventory:item:5
\`\`\`

### Your Task

Set a key \`score\` to \`0\`. Increment it 3 times with \`INCR\`. Then add 10 more with \`INCRBY\`. The final value should be 13.`,

  starterCode: `# Initialize score
SET score 0

# Increment 3 times
INCR score
INCR score
INCR score

# Add 10 more
INCRBY score 10`,

  solution: `SET score 0
INCR score
INCR score
INCR score
INCRBY score 10`,

  tests: [
    {
      name: "final value is 13",
      expected: '{"type":"lastLine","value":"(integer) 13"}',
    },
  ],
};
