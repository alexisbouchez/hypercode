import type { Lesson } from "../../types";

export const listNode: Lesson = {
	id: "list-node",
	title: "The Node",
	chapterId: "the-node",
	content: `## Linked Lists

A **linked list** is a sequence of nodes where each node holds a value and a pointer to the next node:

\`\`\`
[1] -> [2] -> [3] -> [4] -> NULL
\`\`\`

Unlike arrays, linked lists don't store elements in contiguous memory. Each node lives wherever \`malloc\` puts it — they're connected only by pointers.

This makes inserting and deleting at the front O(1), but random access O(n).

### The Node Struct

\`\`\`c
struct Node {
    int val;
    struct Node *next;
};
\`\`\`

\`next\` points to the following node. The last node's \`next\` is \`NULL\`.

### Creating Nodes

Allocate each node with \`malloc\` and initialize its fields:

\`\`\`c
struct Node *new_node(int val) {
    struct Node *n = (struct Node *)malloc(sizeof(struct Node));
    n->val = val;
    n->next = NULL;
    return n;
}
\`\`\`

### Building a List Manually

\`\`\`c
struct Node *head = new_node(1);
head->next = new_node(2);
head->next->next = new_node(3);
// [1] -> [2] -> [3] -> NULL
\`\`\`

### Your Task

Implement \`struct Node *new_node(int val)\` that allocates a node with the given value and \`next = NULL\`. Create a node with value \`10\` and print its value.`,

	starterCode: `#include <stdio.h>
#include <stdlib.h>

struct Node {
\tint val;
\tstruct Node *next;
};

struct Node *new_node(int val) {
\t// Allocate a Node, set val and next = NULL
\treturn NULL;
}

int main() {
\tstruct Node *n = new_node(10);
\tprintf("%d\\n", n->val);
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

int main() {
\tstruct Node *n = new_node(10);
\tprintf("%d\\n", n->val);
\treturn 0;
}
`,

	tests: [
		{
			name: "new_node(10) prints 10",
			expected: "10\n",
		},
		{
			name: "new_node(42)",
			code: `#include <stdio.h>
#include <stdlib.h>
{{FUNC}}
int main() {
\tstruct Node *n = new_node(42);
\tprintf("%d\\n", n->val);
\treturn 0;
}`,
			expected: "42\n",
		},
		{
			name: "next is NULL",
			code: `#include <stdio.h>
#include <stdlib.h>
{{FUNC}}
int main() {
\tstruct Node *n = new_node(7);
\tprintf("%d\\n", n->val);
\tprintf("%d\\n", n->next == NULL ? 1 : 0);
\treturn 0;
}`,
			expected: "7\n1\n",
		},
		{
			name: "link two nodes",
			code: `#include <stdio.h>
#include <stdlib.h>
{{FUNC}}
int main() {
\tstruct Node *a = new_node(1);
\tstruct Node *b = new_node(2);
\ta->next = b;
\tprintf("%d\\n", a->val);
\tprintf("%d\\n", a->next->val);
\tprintf("%d\\n", a->next->next == NULL ? 1 : 0);
\treturn 0;
}`,
			expected: "1\n2\n1\n",
		},
	],
};
