import type { Lesson } from "../../types";

export const nthNode: Lesson = {
	id: "nth-node",
	title: "Nth Node",
	chapterId: "deletions",
	content: `## Getting the Nth Node

To get the node at index \`k\` (0-indexed), advance a pointer \`k\` times from the head:

\`\`\`c
struct Node *nth_node(struct Node *head, int k) {
    struct Node *cur = head;
    while (k > 0 && cur != NULL) {
        cur = cur->next;
        k--;
    }
    return cur;
}
\`\`\`

Returns a pointer to the node, or \`NULL\` if \`k\` is out of bounds.

### Example

\`\`\`
[10] -> [20] -> [30] -> [40] -> NULL

nth_node(head, 0) → [10]   (head)
nth_node(head, 2) → [30]
nth_node(head, 9) → NULL   (out of bounds)
\`\`\`

### O(n) Access

This is the "random access" cost of linked lists. Unlike arrays, there is no pointer arithmetic shortcut — you must walk the list each time.

This is why linked lists are poor choices when you frequently need elements by index.

### Your Task

Implement \`struct Node *nth_node(struct Node *head, int k)\` that returns the node at index \`k\`, or \`NULL\` if out of bounds.`,

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

struct Node *push_back(struct Node *head, int val) {
\tstruct Node *n = new_node(val);
\tif (head == NULL) return n;
\tstruct Node *cur = head;
\twhile (cur->next != NULL) cur = cur->next;
\tcur->next = n;
\treturn head;
}

struct Node *nth_node(struct Node *head, int k) {
\t// Return node at index k, or NULL if out of bounds
\treturn NULL;
}

int main() {
\tstruct Node *head = NULL;
\thead = push_back(head, 10);
\thead = push_back(head, 20);
\thead = push_back(head, 30);
\thead = push_back(head, 40);
\thead = push_back(head, 50);
\tprintf("%d\\n", nth_node(head, 0)->val);
\tprintf("%d\\n", nth_node(head, 2)->val);
\tprintf("%d\\n", nth_node(head, 4)->val);
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

struct Node *push_back(struct Node *head, int val) {
\tstruct Node *n = new_node(val);
\tif (head == NULL) return n;
\tstruct Node *cur = head;
\twhile (cur->next != NULL) cur = cur->next;
\tcur->next = n;
\treturn head;
}

struct Node *nth_node(struct Node *head, int k) {
\tstruct Node *cur = head;
\twhile (k > 0 && cur != NULL) {
\t\tcur = cur->next;
\t\tk--;
\t}
\treturn cur;
}

int main() {
\tstruct Node *head = NULL;
\thead = push_back(head, 10);
\thead = push_back(head, 20);
\thead = push_back(head, 30);
\thead = push_back(head, 40);
\thead = push_back(head, 50);
\tprintf("%d\\n", nth_node(head, 0)->val);
\tprintf("%d\\n", nth_node(head, 2)->val);
\tprintf("%d\\n", nth_node(head, 4)->val);
\treturn 0;
}
`,

	tests: [
		{
			name: "index 0, 2, 4 → 10, 30, 50",
			expected: "10\n30\n50\n",
		},
		{
			name: "out of bounds returns NULL",
			code: `#include <stdio.h>
#include <stdlib.h>
{{FUNC}}
int main() {
\tstruct Node *head = new_node(1);
\tprintf("%d\\n", nth_node(head, 5) == NULL ? 1 : 0);
\treturn 0;
}`,
			expected: "1\n",
		},
		{
			name: "index 0 is head",
			code: `#include <stdio.h>
#include <stdlib.h>
{{FUNC}}
int main() {
\tstruct Node *head = NULL;
\thead = push_back(head, 99);
\thead = push_back(head, 88);
\tprintf("%d\\n", nth_node(head, 0)->val);
\treturn 0;
}`,
			expected: "99\n",
		},
	],
};
