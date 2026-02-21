import type { Lesson } from "../../types";

export const lists: Lesson = {
  id: "lists",
  title: "Lists",
  chapterId: "collections",
  content: `## Redis Lists

A Redis list is a linked list of strings. Elements can be added to or removed from either end efficiently — making lists ideal for queues, stacks, and activity feeds.

### LPUSH and RPUSH — Add Elements

- \`LPUSH key elem [elem ...]\` — push to the **left** (head) of the list
- \`RPUSH key elem [elem ...]\` — push to the **right** (tail) of the list

Both return the new length of the list.

\`\`\`
RPUSH tasks "email" "report" "backup"
LLEN tasks
\`\`\`

Output:
\`\`\`
(integer) 3
(integer) 3
\`\`\`

When pushing multiple elements, they are added in the order given (for RPUSH) or in reverse (for LPUSH).

### LRANGE — Get a Range

\`LRANGE key start stop\` returns elements from index start to stop (inclusive). Indices are 0-based; negative indices count from the end:

\`\`\`
RPUSH fruits "apple" "banana" "cherry"
LRANGE fruits 0 -1    -- all elements
LRANGE fruits 0 1     -- first two
LRANGE fruits -2 -1   -- last two
\`\`\`

Output:
\`\`\`
(integer) 3
1) "apple"
2) "banana"
3) "cherry"
1) "apple"
2) "banana"
1) "banana"
2) "cherry"
\`\`\`

### LPOP and RPOP — Remove Elements

Pop (remove and return) from either end:

\`\`\`
LPOP tasks         -- remove and return first element
RPOP tasks         -- remove and return last element
\`\`\`

\`\`\`
LPOP tasks 2       -- remove and return first 2 elements
\`\`\`

### LLEN — List Length

\`\`\`
LLEN tasks
\`\`\`

### Patterns

**Queue (FIFO):** RPUSH to enqueue, LPOP to dequeue.

\`\`\`
RPUSH queue "job1" "job2" "job3"
LPOP queue    -- "job1" (first in, first out)
\`\`\`

**Stack (LIFO):** RPUSH to push, RPOP to pop.

\`\`\`
RPUSH stack "a" "b" "c"
RPOP stack    -- "c" (last in, first out)
\`\`\`

**Activity Feed:** LPUSH new events, LRANGE to get recent events.

\`\`\`
LPUSH feed:user1 "liked post 42" "commented on 17"
LRANGE feed:user1 0 9    -- latest 10 events
\`\`\`

### Your Task

Create a queue called \`jobs\`. RPUSH three jobs: \`"build"\`, \`"test"\`, \`"deploy"\`. Then use LRANGE to see all jobs.`,

  starterCode: `# Add 3 jobs to the queue
RPUSH jobs "build" "test" "deploy"

# See all jobs
LRANGE jobs 0 -1`,

  solution: `RPUSH jobs "build" "test" "deploy"
LRANGE jobs 0 -1`,

  tests: [
    {
      name: "RPUSH returns 3",
      expected: '{"type":"contains","value":"(integer) 3"}',
    },
    {
      name: "LRANGE contains build",
      expected: '{"type":"contains","value":"\\"build\\""}',
    },
    {
      name: "LRANGE contains deploy",
      expected: '{"type":"contains","value":"\\"deploy\\""}',
    },
  ],
};
