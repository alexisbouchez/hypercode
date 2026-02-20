import type { Lesson } from "../../types";

export const pushBack: Lesson = {
	id: "push-back",
	title: "Push Back",
	chapterId: "insertions",
	content: `## Inserting at the Back

Appending to the end of a list requires traversing to the last node — O(n).

Walk until you find the node whose \`next\` is \`NULL\`, then attach the new node:

\`\`\`c
struct Node *push_back(struct Node *head, int val) {
    struct Node *n = new_node(val);
    if (head == NULL) return n;
    struct Node *cur = head;
    while (cur->next != NULL)
        cur = cur->next;
    cur->next = n;
    return head;
}
\`\`\`

\`\`\`
Before: [1] -> [2] -> NULL
After:  [1] -> [2] -> [3] -> NULL   (push_back(head, 3))
\`\`\`

### Empty List Edge Case

If \`head\` is \`NULL\`, the new node IS the list — return it directly.

### Head Doesn't Change

Unlike \`push_front\`, appending doesn't change the head. We still return \`head\` so callers can use the same pattern (\`head = push_back(head, val)\`).

### Your Task

Implement \`struct Node *push_back(struct Node *head, int val)\` that appends a node to the end and returns the head.`,

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

struct Node *push_back(struct Node *head, int val) {
\t// Walk to end, attach new node, return head
\treturn head;
}

int main() {
\tstruct Node *head = NULL;
\thead = push_back(head, 1);
\thead = push_back(head, 2);
\thead = push_back(head, 3);
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

struct Node *push_back(struct Node *head, int val) {
\tstruct Node *n = new_node(val);
\tif (head == NULL) return n;
\tstruct Node *cur = head;
\twhile (cur->next != NULL)
\t\tcur = cur->next;
\tcur->next = n;
\treturn head;
}

int main() {
\tstruct Node *head = NULL;
\thead = push_back(head, 1);
\thead = push_back(head, 2);
\thead = push_back(head, 3);
\tprint_list(head);
\treturn 0;
}
`,

	tests: [
		{
			name: "push_back 1,2,3 → [1,2,3]",
			expected: "1\n2\n3\n",
		},
		{
			name: "push_back onto empty list",
			code: `#include <stdio.h>
#include <stdlib.h>
{{FUNC}}
int main() {
\tstruct Node *head = NULL;
\thead = push_back(head, 7);
\tprint_list(head);
\treturn 0;
}`,
			expected: "7\n",
		},
		{
			name: "head unchanged after push_back",
			code: `#include <stdio.h>
#include <stdlib.h>
{{FUNC}}
int main() {
\tstruct Node *head = new_node(1);
\tpush_back(head, 2);
\tpush_back(head, 3);
\tprintf("%d\\n", head->val);
\treturn 0;
}`,
			expected: "1\n",
		},
		{
			name: "push_back [5,4,3,2,1]",
			code: `#include <stdio.h>
#include <stdlib.h>
{{FUNC}}
int main() {
\tstruct Node *head = NULL;
\thead = push_back(head, 5);
\thead = push_back(head, 4);
\thead = push_back(head, 3);
\thead = push_back(head, 2);
\thead = push_back(head, 1);
\tprint_list(head);
\treturn 0;
}`,
			expected: "5\n4\n3\n2\n1\n",
		},
	],
};
