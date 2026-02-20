import type { Lesson } from "../../types";

export const pushFront: Lesson = {
	id: "push-front",
	title: "Push Front",
	chapterId: "insertions",
	content: `## Inserting at the Front

Adding a node to the front of a list is O(1) — the cheapest insertion in a linked list.

Create the new node, point its \`next\` to the current head, and return it as the new head:

\`\`\`c
struct Node *push_front(struct Node *head, int val) {
    struct Node *n = new_node(val);
    n->next = head;
    return n;
}
\`\`\`

\`\`\`
Before: [2] -> [3] -> NULL
After:  [1] -> [2] -> [3] -> NULL   (push_front(head, 1))
\`\`\`

### Why Return the New Head?

In C, if we just modify \`n->next\`, the caller's \`head\` variable still points to the old first node. We must return the new node and let the caller update their pointer:

\`\`\`c
head = push_front(head, 10);
\`\`\`

### Push Order

Calling \`push_front\` repeatedly builds the list in **reverse** order:

\`\`\`c
head = push_front(NULL, 1);  // [1]
head = push_front(head, 2);  // [2] -> [1]
head = push_front(head, 3);  // [3] -> [2] -> [1]
\`\`\`

### Your Task

Implement \`struct Node *push_front(struct Node *head, int val)\` that prepends a node and returns the new head.`,

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

void print_list(struct Node *head) {
\tstruct Node *cur = head;
\twhile (cur != NULL) { printf("%d\\n", cur->val); cur = cur->next; }
}

struct Node *push_front(struct Node *head, int val) {
\t// Create node, link it before head, return it
\treturn head;
}

int main() {
\tstruct Node *head = NULL;
\thead = push_front(head, 1);
\thead = push_front(head, 2);
\thead = push_front(head, 3);
\tprint_list(head);
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

void print_list(struct Node *head) {
\tstruct Node *cur = head;
\twhile (cur != NULL) { printf("%d\\n", cur->val); cur = cur->next; }
}

struct Node *push_front(struct Node *head, int val) {
\tstruct Node *n = new_node(val);
\tn->next = head;
\treturn n;
}

int main() {
\tstruct Node *head = NULL;
\thead = push_front(head, 1);
\thead = push_front(head, 2);
\thead = push_front(head, 3);
\tprint_list(head);
\treturn 0;
}
`,

	tests: [
		{
			name: "push 1,2,3 front → [3,2,1]",
			expected: "3\n2\n1\n",
		},
		{
			name: "push onto empty list",
			code: `#include <stdio.h>
#include <stdlib.h>
{{FUNC}}
int main() {
\tstruct Node *head = NULL;
\thead = push_front(head, 42);
\tprint_list(head);
\treturn 0;
}`,
			expected: "42\n",
		},
		{
			name: "new node becomes head",
			code: `#include <stdio.h>
#include <stdlib.h>
{{FUNC}}
int main() {
\tstruct Node *head = new_node(2);
\thead = push_front(head, 1);
\tprintf("%d\\n", head->val);
\tprintf("%d\\n", head->next->val);
\treturn 0;
}`,
			expected: "1\n2\n",
		},
	],
};
