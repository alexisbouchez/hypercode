import type { Lesson } from "../../types";

export const hyperloglog: Lesson = {
  id: "hyperloglog",
  title: "HyperLogLog",
  chapterId: "advanced",
  content: `## HyperLogLog — Approximate Counting

HyperLogLog (HLL) is a probabilistic data structure that estimates the cardinality (unique count) of a set using a fixed amount of memory — typically 12 KB — regardless of how many unique elements you add.

This makes it ideal for counting unique visitors, unique search queries, unique events, or any other "how many distinct things?" problem at scale.

### The Trade-Off

HyperLogLog is **approximate**: it estimates cardinality with a standard error of about **0.81%**. For most analytics use cases, this is perfectly acceptable.

Exact counting with a Set would require memory proportional to the number of unique items. HyperLogLog uses a fixed 12 KB no matter how many items you add.

> **Note:** In this emulator, HyperLogLog uses an exact set internally, so counts are always precise. In real Redis, results are approximate.

### PFADD — Add Elements

\`\`\`
PFADD visitors "user1" "user2" "user3"
PFADD visitors "user1" "user2"    -- duplicates ignored in counting
\`\`\`

\`PFADD\` returns \`1\` if the HLL was modified (new unique element added), \`0\` if not.

### PFCOUNT — Estimate Cardinality

\`\`\`
PFADD visitors "user1" "user2" "user3" "user1"
PFCOUNT visitors    -- (integer) 3 (only 3 unique)
\`\`\`

### PFMERGE — Merge Multiple HLLs

Combine multiple HyperLogLogs into one:

\`\`\`
PFADD page:home "u1" "u2" "u3"
PFADD page:about "u2" "u3" "u4" "u5"

PFMERGE site:total page:home page:about
PFCOUNT site:total    -- unique visitors across both pages
\`\`\`

Output:
\`\`\`
(integer) 1
(integer) 1
OK
(integer) 5
\`\`\`

### Real-World Use Cases

**Daily unique visitors:**
\`\`\`
PFADD visitors:2024-01-15 "user1" "user2" "user3"
PFCOUNT visitors:2024-01-15

# Weekly total (merge 7 days)
PFMERGE visitors:week:3 visitors:2024-01-15 visitors:2024-01-16 ...
PFCOUNT visitors:week:3
\`\`\`

**Unique search queries:**
\`\`\`
PFADD searches:today "redis tutorial" "python redis" "redis tutorial"
PFCOUNT searches:today    -- 2 unique queries
\`\`\`

**Unique API callers:**
\`\`\`
PFADD api:callers:hourly "client-123" "client-456" "client-123"
PFCOUNT api:callers:hourly
\`\`\`

### Why "PF"?

The "PF" prefix honors **Philippe Flajolet**, the French mathematician who invented the HyperLogLog algorithm in 2007.

### Your Task

Track unique visitors across two pages:
1. PFADD \`page:home\` with visitors: \`"u1"\`, \`"u2"\`, \`"u3"\`, \`"u4"\`
2. PFADD \`page:docs\` with visitors: \`"u3"\`, \`"u4"\`, \`"u5"\`
3. PFMERGE \`site\` from both pages
4. PFCOUNT \`site\` to get total unique visitors`,

  starterCode: `# Home page visitors
PFADD page:home "u1" "u2" "u3" "u4"

# Docs page visitors
PFADD page:docs "u3" "u4" "u5"

# Merge and count unique visitors across site
PFMERGE site page:home page:docs
PFCOUNT site`,

  solution: `PFADD page:home "u1" "u2" "u3" "u4"
PFADD page:docs "u3" "u4" "u5"
PFMERGE site page:home page:docs
PFCOUNT site`,

  tests: [
    {
      name: "total unique visitors is 5",
      expected: '{"type":"lastLine","value":"(integer) 5"}',
    },
    {
      name: "PFMERGE returns OK",
      expected: '{"type":"contains","value":"OK"}',
    },
  ],
};
