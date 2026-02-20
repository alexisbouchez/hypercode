import type { Lesson } from "../../types";

export const popFront: Lesson = {
	id: "pop-front",
	title: "Pop Front",
	chapterId: "insertions",
	content: `## Removing the First Node

Removing the head is O(1): advance the head pointer past the first node and free the old one.

\`\`\`c
struct Node *pop_front(struct Node *head) {
    if (head == NULL) return NULL;
    struct Node *old = head;
    head = head->next;
    free(old);
    return head;
}
\`\`\`

\`\`\`
Before: [1] -> [2] -> [3] -> NULL
After:  [2] -> [3] -> NULL   (pop_front)
\`\`\`

### Always Check for NULL

If the list is empty (\`head == NULL\`), return \`NULL\` immediately — don't dereference a null pointer.

### Memory

Save the old head pointer before advancing, then \`free\` it. Without \`free\`, the removed node leaks memory.

### Return the New Head

The caller's \`head\` must be updated:

\`\`\`c
head = pop_front(head);
\`\`\`

### Your Task

Implement \`struct Node *pop_front(struct Node *head)\` that removes the first node and returns the new head.`,

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

struct Node *pop_front(struct Node *head) {
\t// Remove first node, free it, return new head
\treturn head;
}

int main() {
\tstruct Node *head = new_node(1);
\thead->next = new_node(2);
\thead->next->next = new_node(3);
\thead = pop_front(head);
\thead = pop_front(head);
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

struct Node *pop_front(struct Node *head) {
\tif (head == NULL) return NULL;
\tstruct Node *old = head;
\thead = head->next;
\tfree(old);
\treturn head;
}

int main() {
\tstruct Node *head = new_node(1);
\thead->next = new_node(2);
\thead->next->next = new_node(3);
\thead = pop_front(head);
\thead = pop_front(head);
\tprint_list(head);
\treturn 0;
}
`,

	tests: [
		{
			name: "pop front twice from [1,2,3] → [3]",
			expected: "3\n",
		},
		{
			name: "pop empty list returns NULL",
			code: `#include <stdio.h>
#include <stdlib.h>
{{FUNC}}
int main() {
\tstruct Node *head = pop_front(NULL);
\tprintf("%d\\n", head == NULL ? 1 : 0);
\treturn 0;
}`,
			expected: "1\n",
		},
		{
			name: "pop single node → empty",
			code: `#include <stdio.h>
#include <stdlib.h>
{{FUNC}}
int main() {
\tstruct Node *head = new_node(5);
\thead = pop_front(head);
\tprintf("%d\\n", head == NULL ? 1 : 0);
\treturn 0;
}`,
			expected: "1\n",
		},
		{
			name: "pop reveals correct next value",
			code: `#include <stdio.h>
#include <stdlib.h>
{{FUNC}}
int main() {
\tstruct Node *head = new_node(10);
\thead->next = new_node(20);
\thead->next->next = new_node(30);
\thead = pop_front(head);
\tprintf("%d\\n", head->val);
\treturn 0;
}`,
			expected: "20\n",
		},
	],
};
