import type { Lesson } from "../../types";

export const doublyLinkedList: Lesson = {
	id: "doubly-linked-list",
	title: "Doubly Linked List",
	chapterId: "classic",
	content: `## Doubly Linked Lists

A **doubly linked list** extends the singly linked list by adding a \`prev\` pointer to each node. Every node knows both its successor and its predecessor.

\`\`\`c
struct DNode {
    int val;
    struct DNode *prev;
    struct DNode *next;
};
\`\`\`

\`\`\`
NULL <-> [1] <-> [2] <-> [3] <-> NULL
\`\`\`

### Insert at Head

Create a new node, point its \`next\` to the old head, and update the old head's \`prev\`:

\`\`\`c
struct DNode *insert_head(struct DNode *head, int val) {
    struct DNode *n = new_dnode(val);
    n->next = head;
    if (head != NULL) head->prev = n;
    return n;
}
\`\`\`

### Insert at Tail

Walk to the last node and attach:

\`\`\`c
struct DNode *insert_tail(struct DNode *head, int val) {
    struct DNode *n = new_dnode(val);
    if (head == NULL) return n;
    struct DNode *cur = head;
    while (cur->next != NULL) cur = cur->next;
    cur->next = n;
    n->prev = cur;
    return head;
}
\`\`\`

### Delete a Node by Value

Find the node, then rewire its neighbors to skip over it:

\`\`\`c
struct DNode *delete_node(struct DNode *head, int val) {
    struct DNode *cur = head;
    while (cur != NULL) {
        if (cur->val == val) {
            if (cur->prev) cur->prev->next = cur->next;
            else head = cur->next;          // deleting head
            if (cur->next) cur->next->prev = cur->prev;
            free(cur);
            return head;
        }
        cur = cur->next;
    }
    return head;
}
\`\`\`

### Backward Traversal

Because each node has a \`prev\` pointer, you can walk the list in reverse from the tail:

\`\`\`c
void print_reverse(struct DNode *head) {
    if (head == NULL) return;
    struct DNode *cur = head;
    while (cur->next != NULL) cur = cur->next;
    while (cur != NULL) {
        printf("%d\\n", cur->val);
        cur = cur->prev;
    }
}
\`\`\`

### Your Task

Implement all four functions: \`insert_head\`, \`insert_tail\`, \`delete_node\`, and \`print_reverse\`.`,

	starterCode: `#include <stdio.h>
#include <stdlib.h>

struct DNode {
\tint val;
\tstruct DNode *prev;
\tstruct DNode *next;
};

struct DNode *new_dnode(int val) {
\tstruct DNode *n = (struct DNode *)malloc(sizeof(struct DNode));
\tn->val = val;
\tn->prev = NULL;
\tn->next = NULL;
\treturn n;
}

void print_list(struct DNode *head) {
\tstruct DNode *cur = head;
\twhile (cur != NULL) { printf("%d\\n", cur->val); cur = cur->next; }
}

struct DNode *insert_head(struct DNode *head, int val) {
\t// Insert at the front, return new head
\treturn head;
}

struct DNode *insert_tail(struct DNode *head, int val) {
\t// Insert at the back, return head
\treturn head;
}

struct DNode *delete_node(struct DNode *head, int val) {
\t// Delete first node with given val, return head
\treturn head;
}

void print_reverse(struct DNode *head) {
\t// Walk to tail, then print backward
}

int main() {
\tstruct DNode *head = NULL;
\thead = insert_tail(head, 1);
\thead = insert_tail(head, 2);
\thead = insert_tail(head, 3);
\tprint_list(head);
\tprintf("---\\n");
\tprint_reverse(head);
\treturn 0;
}
`,

	solution: `#include <stdio.h>
#include <stdlib.h>

struct DNode {
\tint val;
\tstruct DNode *prev;
\tstruct DNode *next;
};

struct DNode *new_dnode(int val) {
\tstruct DNode *n = (struct DNode *)malloc(sizeof(struct DNode));
\tn->val = val;
\tn->prev = NULL;
\tn->next = NULL;
\treturn n;
}

void print_list(struct DNode *head) {
\tstruct DNode *cur = head;
\twhile (cur != NULL) { printf("%d\\n", cur->val); cur = cur->next; }
}

struct DNode *insert_head(struct DNode *head, int val) {
\tstruct DNode *n = new_dnode(val);
\tn->next = head;
\tif (head != NULL) head->prev = n;
\treturn n;
}

struct DNode *insert_tail(struct DNode *head, int val) {
\tstruct DNode *n = new_dnode(val);
\tif (head == NULL) return n;
\tstruct DNode *cur = head;
\twhile (cur->next != NULL) cur = cur->next;
\tcur->next = n;
\tn->prev = cur;
\treturn head;
}

struct DNode *delete_node(struct DNode *head, int val) {
\tstruct DNode *cur = head;
\twhile (cur != NULL) {
\t\tif (cur->val == val) {
\t\t\tif (cur->prev) cur->prev->next = cur->next;
\t\t\telse head = cur->next;
\t\t\tif (cur->next) cur->next->prev = cur->prev;
\t\t\tfree(cur);
\t\t\treturn head;
\t\t}
\t\tcur = cur->next;
\t}
\treturn head;
}

void print_reverse(struct DNode *head) {
\tif (head == NULL) return;
\tstruct DNode *cur = head;
\twhile (cur->next != NULL) cur = cur->next;
\twhile (cur != NULL) {
\t\tprintf("%d\\n", cur->val);
\t\tcur = cur->prev;
\t}
}

int main() {
\tstruct DNode *head = NULL;
\thead = insert_tail(head, 1);
\thead = insert_tail(head, 2);
\thead = insert_tail(head, 3);
\tprint_list(head);
\tprintf("---\\n");
\tprint_reverse(head);
\treturn 0;
}
`,

	tests: [
		{
			name: "insert_tail and forward print",
			expected: "1\n2\n3\n---\n3\n2\n1\n",
		},
		{
			name: "insert_head builds reversed order",
			code: `#include <stdio.h>
#include <stdlib.h>
{{FUNC}}
int main() {
\tstruct DNode *head = NULL;
\thead = insert_head(head, 1);
\thead = insert_head(head, 2);
\thead = insert_head(head, 3);
\tprint_list(head);
\treturn 0;
}`,
			expected: "3\n2\n1\n",
		},
		{
			name: "delete middle node",
			code: `#include <stdio.h>
#include <stdlib.h>
{{FUNC}}
int main() {
\tstruct DNode *head = NULL;
\thead = insert_tail(head, 1);
\thead = insert_tail(head, 2);
\thead = insert_tail(head, 3);
\thead = delete_node(head, 2);
\tprint_list(head);
\tprintf("---\\n");
\tprint_reverse(head);
\treturn 0;
}`,
			expected: "1\n3\n---\n3\n1\n",
		},
		{
			name: "delete head node",
			code: `#include <stdio.h>
#include <stdlib.h>
{{FUNC}}
int main() {
\tstruct DNode *head = NULL;
\thead = insert_tail(head, 10);
\thead = insert_tail(head, 20);
\thead = insert_tail(head, 30);
\thead = delete_node(head, 10);
\tprint_list(head);
\treturn 0;
}`,
			expected: "20\n30\n",
		},
	],
};
