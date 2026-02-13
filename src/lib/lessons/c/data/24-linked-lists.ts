import type { Lesson } from "../../types";

export const linkedLists: Lesson = {
	id: "linked-lists",
	title: "Linked Lists",
	chapterId: "dynamic-memory",
	content: `## Linked Lists

A **linked list** is a dynamic data structure where each element (node) points to the next. Unlike arrays, linked lists can grow and shrink without reallocation.

### Node Structure

\`\`\`c
struct Node {
    int data;
    struct Node *next;
};
\`\`\`

Each node holds a value and a pointer to the next node. The last node points to \`NULL\`.

> The chain of command: each officer points to the next in line, from Captain down to Ensign. The last one points to \`NULL\` -- no one left to delegate to.

### Creating Nodes

\`\`\`c
struct Node *new_node(int data) {
    struct Node *n = (struct Node *)malloc(sizeof(struct Node));
    n->data = data;
    n->next = NULL;
    return n;
}
\`\`\`

### Building a List

\`\`\`c
struct Node *head = new_node(10);
head->next = new_node(20);
head->next->next = new_node(30);
// List: 10 -> 20 -> 30 -> NULL
\`\`\`

### Traversing a List

\`\`\`c
struct Node *curr = head;
while (curr != NULL) {
    printf("%d\\n", curr->data);
    curr = curr->next;
}
\`\`\`

### Prepending (Adding to Front)

\`\`\`c
struct Node *prepend(struct Node *head, int data) {
    struct Node *n = new_node(data);
    n->next = head;
    return n;  // new head
}
\`\`\`

### Your Task

Implement \`struct Node *prepend(struct Node *head, int data)\` that adds a new node to the front of the list, and \`void print_list(struct Node *head)\` that prints each value on its own line. Build a list by prepending 3, 2, 1 (in that order) and print it.`,

	starterCode: `#include <stdio.h>
#include <stdlib.h>

struct Node {
\tint data;
\tstruct Node *next;
};

struct Node *prepend(struct Node *head, int data) {
\t// Create a new node and link it before head
\treturn NULL;
}

void print_list(struct Node *head) {
\t// Print each node's data on its own line
}

int main() {
\tstruct Node *list = NULL;
\tlist = prepend(list, 3);
\tlist = prepend(list, 2);
\tlist = prepend(list, 1);
\tprint_list(list);
\treturn 0;
}
`,

	solution: `#include <stdio.h>
#include <stdlib.h>

struct Node {
\tint data;
\tstruct Node *next;
};

struct Node *prepend(struct Node *head, int data) {
\tstruct Node *n = (struct Node *)malloc(sizeof(struct Node));
\tn->data = data;
\tn->next = head;
\treturn n;
}

void print_list(struct Node *head) {
\tstruct Node *curr = head;
\twhile (curr != NULL) {
\t\tprintf("%d\\n", curr->data);
\t\tcurr = curr->next;
\t}
}

int main() {
\tstruct Node *list = NULL;
\tlist = prepend(list, 3);
\tlist = prepend(list, 2);
\tlist = prepend(list, 1);
\tprint_list(list);
\treturn 0;
}
`,

	tests: [
		{
			name: "prints 1 2 3",
			expected: "1\n2\n3\n",
		},
		{
			name: "single element list",
			code: `#include <stdio.h>
#include <stdlib.h>
{{FUNC}}
int main() {
\tstruct Node *list = NULL;
\tlist = prepend(list, 42);
\tprint_list(list);
\treturn 0;
}`,
			expected: "42\n",
		},
		{
			name: "five elements",
			code: `#include <stdio.h>
#include <stdlib.h>
{{FUNC}}
int main() {
\tstruct Node *list = NULL;
\tlist = prepend(list, 5);
\tlist = prepend(list, 4);
\tlist = prepend(list, 3);
\tlist = prepend(list, 2);
\tlist = prepend(list, 1);
\tprint_list(list);
\treturn 0;
}`,
			expected: "1\n2\n3\n4\n5\n",
		},
		{
			name: "prepend preserves order",
			code: `#include <stdio.h>
#include <stdlib.h>
{{FUNC}}
int main() {
\tstruct Node *list = NULL;
\tlist = prepend(list, 30);
\tlist = prepend(list, 20);
\tlist = prepend(list, 10);
\tprint_list(list);
\treturn 0;
}`,
			expected: "10\n20\n30\n",
		},
	],
};
