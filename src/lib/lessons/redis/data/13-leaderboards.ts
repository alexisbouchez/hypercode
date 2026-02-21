import type { Lesson } from "../../types";

export const leaderboards: Lesson = {
  id: "leaderboards",
  title: "Leaderboards",
  chapterId: "patterns",
  content: `## Leaderboards with Sorted Sets

Leaderboards are one of the most popular Redis use cases. Sorted sets make ranking operations trivially efficient — O(log N) for most operations, regardless of the number of players.

### Building a Leaderboard

\`\`\`
ZADD game:scores 1200 "Charlie" 1000 "Alice" 750 "Bob" 500 "Dave"
\`\`\`

### Get Top N Players

\`ZREVRANGE\` returns members in descending score order:

\`\`\`
ZREVRANGE game:scores 0 2 WITHSCORES    -- top 3
\`\`\`

Output:
\`\`\`
1) "Charlie"
2) "1200"
3) "Alice"
4) "1000"
5) "Bob"
6) "750"
\`\`\`

### Get a Player's Rank

\`ZREVRANK\` gives 0-based rank (0 = first place):

\`\`\`
ZREVRANK game:scores "Alice"    -- 1 (second place)
ZREVRANK game:scores "Dave"     -- 3 (last place)
\`\`\`

### Get a Player's Score

\`\`\`
ZSCORE game:scores "Alice"    -- "1000"
\`\`\`

### Update a Score

\`ZINCRBY\` atomically adds to a score:

\`\`\`
ZINCRBY game:scores 300 "Bob"    -- Bob's score becomes 1050
ZREVRANK game:scores "Bob"       -- now rank 2
\`\`\`

### Players in a Score Range

\`\`\`
ZRANGEBYSCORE game:scores 800 1100 WITHSCORES    -- scores 800-1100
ZRANGEBYSCORE game:scores -inf +inf              -- everyone
\`\`\`

### Real-Time Leaderboard Pattern

\`\`\`
# Player completes a level — add to their score
ZINCRBY leaderboard:daily 500 "user:42"

# Refresh player's rank display
ZREVRANK leaderboard:daily "user:42"
ZSCORE leaderboard:daily "user:42"

# Get top 10
ZREVRANGE leaderboard:daily 0 9 WITHSCORES
\`\`\`

### Multiple Leaderboards

Redis makes it trivial to maintain leaderboards at different scopes:
\`\`\`
ZINCRBY leaderboard:daily 100 "alice"
ZINCRBY leaderboard:weekly 100 "alice"
ZINCRBY leaderboard:alltime 100 "alice"
\`\`\`

### Your Task

Build a game leaderboard with four players: Alice at 950, Bob at 1100, Charlie at 800, Dave at 1050. Get the **top 3** players with their scores using ZREVRANGE.`,

  starterCode: `# Build the leaderboard
ZADD game 950 "Alice" 1100 "Bob" 800 "Charlie" 1050 "Dave"

# Get top 3 with scores
ZREVRANGE game 0 2 WITHSCORES`,

  solution: `ZADD game 950 "Alice" 1100 "Bob" 800 "Charlie" 1050 "Dave"
ZREVRANGE game 0 2 WITHSCORES`,

  tests: [
    {
      name: "Bob is first (highest score 1100)",
      expected: '{"type":"contains","value":"\\"Bob\\""}',
    },
    {
      name: "score 1100 appears",
      expected: '{"type":"contains","value":"\\"1100\\""}',
    },
    {
      name: "ZADD added 4 members",
      expected: '{"type":"contains","value":"(integer) 4"}',
    },
  ],
};
