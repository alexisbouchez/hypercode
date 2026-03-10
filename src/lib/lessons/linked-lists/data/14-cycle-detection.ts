import type { Lesson } from "../../types";

export const cycleDetection: Lesson = {
	id: "cycle-detection",
	title: "Cycle Detection",
	chapterId: "classic",
	content: `## Floyd's Cycle Detection

A **cycle** in a linked list means some node's \`next\` pointer points back to an earlier node, creating an infinite loop. Traversing such a list with a simple \`while (cur != NULL)\` would never terminate.

\`\`\`
[1] -> [2] -> [3] -> [4] -> [5]
              ^                |
              |________________|
\`\`\`

### The Tortoise and Hare Algorithm

Floyd's algorithm uses two pointers moving at different speeds:

- **Slow** (tortoise): moves one step at a time
- **Fast** (hare): moves two steps at a time

If there is a cycle, the fast pointer will eventually lap the slow pointer and they will meet inside the cycle. If there is no cycle, the fast pointer reaches \`NULL\`.

\`\`\`c
int has_cycle(struct Node *head) {
    struct Node *slow = head;
    struct Node *fast = head;
    while (fast != NULL && fast->next != NULL) {
        slow = slow->next;
        fast = fast->next->next;
        if (slow == fast) return 1;
    }
    return 0;
}
\`\`\`

### Why Does It Work?

Once both pointers are inside the cycle, the distance between them shrinks by 1 on every iteration (fast gains one step, but they are in a loop). So they are guaranteed to collide.

### Time & Space Complexity

- **Time**: O(n) — the pointers meet within at most one full traversal of the cycle
- **Space**: O(1) — only two pointers, no extra data structures

### Your Task

Implement \`int has_cycle(struct Node *head)\` that returns \`1\` if the list contains a cycle, and \`0\` otherwise.`,

	starterCode: `#include <stdio.h>
#include <stdlib.h>

struct Node {
\tint val;
\tstruct Node *next;
};

struct Node *new_node(int val) {
\tstruct Node *n = (struct Node *)malloc(sizeof(struct Node));
\tn->val = val;
\tn->next = NULL;
\treturn n;
}

int has_cycle(struct Node *head) {
\t// Use Floyd's tortoise and hare algorithm
\treturn 0;
}

int main() {
\tstruct Node *a = new_node(1);
\tstruct Node *b = new_node(2);
\tstruct Node *c = new_node(3);
\tstruct Node *d = new_node(4);
\ta->next = b;
\tb->next = c;
\tc->next = d;
\td->next = b;
\tprintf("%d\\n", has_cycle(a));
\treturn 0;
}
`,

	solution: `#include <stdio.h>
#include <stdlib.h>

struct Node {
\tint val;
\tstruct Node *next;
};

struct Node *new_node(int val) {
\tstruct Node *n = (struct Node *)malloc(sizeof(struct Node));
\tn->val = val;
\tn->next = NULL;
\treturn n;
}

int has_cycle(struct Node *head) {
\tstruct Node *slow = head;
\tstruct Node *fast = head;
\twhile (fast != NULL && fast->next != NULL) {
\t\tslow = slow->next;
\t\tfast = fast->next->next;
\t\tif (slow == fast) return 1;
\t}
\treturn 0;
}

int main() {
\tstruct Node *a = new_node(1);
\tstruct Node *b = new_node(2);
\tstruct Node *c = new_node(3);
\tstruct Node *d = new_node(4);
\ta->next = b;
\tb->next = c;
\tc->next = d;
\td->next = b;
\tprintf("%d\\n", has_cycle(a));
\treturn 0;
}
`,

	tests: [
		{
			name: "cycle detected (loop at node 2)",
			expected: "1\n",
		},
		{
			name: "no cycle in linear list",
			code: `#include <stdio.h>
#include <stdlib.h>
{{FUNC}}
int main() {
\tstruct Node *a = new_node(1);
\tstruct Node *b = new_node(2);
\tstruct Node *c = new_node(3);
\ta->next = b;
\tb->next = c;
\tprintf("%d\\n", has_cycle(a));
\treturn 0;
}`,
			expected: "0\n",
		},
		{
			name: "cycle at head (single node points to itself)",
			code: `#include <stdio.h>
#include <stdlib.h>
{{FUNC}}
int main() {
\tstruct Node *a = new_node(42);
\ta->next = a;
\tprintf("%d\\n", has_cycle(a));
\treturn 0;
}`,
			expected: "1\n",
		},
		{
			name: "empty list returns 0",
			code: `#include <stdio.h>
#include <stdlib.h>
{{FUNC}}
int main() {
\tprintf("%d\\n", has_cycle(NULL));
\treturn 0;
}`,
			expected: "0\n",
		},
	],
};
