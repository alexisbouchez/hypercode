import type { Lesson } from "../../types";

export const sortedSets: Lesson = {
  id: "sorted-sets",
  title: "Sorted Sets",
  chapterId: "collections",
  content: `## Redis Sorted Sets

A sorted set is like a set (unique members) but each member has an associated floating-point **score**. Members are always kept in score order. Sorted sets are perfect for leaderboards, rankings, and priority queues.

### ZADD — Add Members with Scores

\`\`\`
ZADD leaderboard 100 "Alice" 85 "Bob" 92 "Charlie"
\`\`\`

Returns the number of **new** members added (not updated).

### ZRANGE — Get Members in Score Order (Ascending)

\`\`\`
ZRANGE leaderboard 0 -1
ZRANGE leaderboard 0 -1 WITHSCORES
\`\`\`

Output:
\`\`\`
1) "Bob"
2) "Charlie"
3) "Alice"
---
1) "Bob"
2) "85"
3) "Charlie"
4) "92"
5) "Alice"
6) "100"
\`\`\`

### ZREVRANGE — Get Members in Reverse Order (Descending)

\`\`\`
ZREVRANGE leaderboard 0 2 WITHSCORES
\`\`\`

Returns top 3, highest score first.

### ZRANK and ZREVRANK — Get Rank

\`\`\`
ZRANK leaderboard "Alice"       -- 2 (0-indexed, ascending)
ZREVRANK leaderboard "Alice"    -- 0 (0-indexed, descending = rank 1)
\`\`\`

### ZSCORE — Get Score

\`\`\`
ZSCORE leaderboard "Alice"    -- "100"
\`\`\`

### ZINCRBY — Change Score

\`\`\`
ZINCRBY leaderboard 15 "Bob"
ZSCORE leaderboard "Bob"    -- "100"
\`\`\`

### ZCARD — Number of Members

\`\`\`
ZCARD leaderboard    -- 3
\`\`\`

### ZRANGEBYSCORE — Range by Score

\`\`\`
ZRANGEBYSCORE leaderboard 90 100    -- members scoring 90-100
ZRANGEBYSCORE leaderboard -inf +inf -- all members
ZRANGEBYSCORE leaderboard (85 100   -- exclusive lower bound (>85)
\`\`\`

### Real-World Uses

**Gaming leaderboard:**
\`\`\`
ZADD game:scores 1200 "Charlie" 1000 "Alice" 750 "Bob"
ZREVRANGE game:scores 0 2 WITHSCORES    -- top 3
\`\`\`

**Priority queue:**
\`\`\`
ZADD tasks 1 "critical-bug" 5 "feature" 3 "refactor"
ZPOPMIN tasks    -- get highest priority (lowest score)
\`\`\`

### Your Task

Build a leaderboard \`scores\` with: \`"Alice"\` at 100, \`"Bob"\` at 75, \`"Charlie"\` at 92, \`"Dave"\` at 88. Then use ZREVRANGE to get all members with scores, highest first.`,

  starterCode: `# Add players to leaderboard
ZADD scores 100 "Alice" 75 "Bob" 92 "Charlie" 88 "Dave"

# Get all players, highest score first
ZREVRANGE scores 0 -1 WITHSCORES`,

  solution: `ZADD scores 100 "Alice" 75 "Bob" 92 "Charlie" 88 "Dave"
ZREVRANGE scores 0 -1 WITHSCORES`,

  tests: [
    {
      name: "ZADD added 4 members",
      expected: '{"type":"contains","value":"(integer) 4"}',
    },
    {
      name: "Alice appears in output",
      expected: '{"type":"contains","value":"\\"Alice\\""}',
    },
    {
      name: "scores appear in output",
      expected: '{"type":"contains","value":"\\"100\\""}',
    },
  ],
};
