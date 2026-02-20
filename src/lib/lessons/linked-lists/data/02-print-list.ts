import type { Lesson } from "../../types";

export const printList: Lesson = {
	id: "print-list",
	title: "Print a List",
	chapterId: "the-node",
	content: `## Traversing a Linked List

To visit every node, start at \`head\` and follow \`next\` pointers until you hit \`NULL\`:

\`\`\`c
void print_list(struct Node *head) {
    struct Node *cur = head;
    while (cur != NULL) {
        printf("%d\\n", cur->val);
        cur = cur->next;
    }
}
\`\`\`

This is the fundamental linked list loop. It runs in O(n) time.

### The Pattern

Every linked list traversal follows this pattern:

\`\`\`c
struct Node *cur = head;
while (cur != NULL) {
    // do something with cur->val
    cur = cur->next;
}
\`\`\`

Notice:
- Start \`cur\` at \`head\` (don't modify \`head\` — you'd lose the list!)
- Check \`cur != NULL\` before accessing \`cur->val\`
- Advance with \`cur = cur->next\` at each step

### Printing vs. Modifying

When you only need to read the list, use a local pointer (\`cur\`) instead of moving \`head\`. Once \`head\` advances past a node, you can't get back to it.

### Your Task

Implement \`void print_list(struct Node *head)\` that prints each node's value on its own line.`,

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
\t// Print each node's value on its own line
}

int main() {
\tstruct Node *head = new_node(1);
\thead->next = new_node(2);
\thead->next->next = new_node(3);
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
\twhile (cur != NULL) {
\t\tprintf("%d\\n", cur->val);
\t\tcur = cur->next;
\t}
}

int main() {
\tstruct Node *head = new_node(1);
\thead->next = new_node(2);
\thead->next->next = new_node(3);
\tprint_list(head);
\treturn 0;
}
`,

	tests: [
		{
			name: "print [1,2,3]",
			expected: "1\n2\n3\n",
		},
		{
			name: "single node",
			code: `#include <stdio.h>
#include <stdlib.h>
{{FUNC}}
int main() {
\tstruct Node *head = new_node(42);
\tprint_list(head);
\treturn 0;
}`,
			expected: "42\n",
		},
		{
			name: "NULL list prints nothing",
			code: `#include <stdio.h>
#include <stdlib.h>
{{FUNC}}
int main() {
\tprint_list(NULL);
\tprintf("done\\n");
\treturn 0;
}`,
			expected: "done\n",
		},
		{
			name: "print [10,20,30,40]",
			code: `#include <stdio.h>
#include <stdlib.h>
{{FUNC}}
int main() {
\tstruct Node *head = new_node(10);
\thead->next = new_node(20);
\thead->next->next = new_node(30);
\thead->next->next->next = new_node(40);
\tprint_list(head);
\treturn 0;
}`,
			expected: "10\n20\n30\n40\n",
		},
	],
};
