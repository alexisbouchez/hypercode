import type { Lesson } from "../../types";

export const listLength: Lesson = {
	id: "list-length",
	title: "Length",
	chapterId: "the-node",
	content: `## Counting Nodes

To count the nodes in a list, traverse it and increment a counter:

\`\`\`c
int list_length(struct Node *head) {
    int count = 0;
    struct Node *cur = head;
    while (cur != NULL) {
        count++;
        cur = cur->next;
    }
    return count;
}
\`\`\`

This runs in O(n) — there is no shortcut. You must visit every node.

Unlike arrays, a linked list has no stored length. If you need the length frequently, you'd track it in a wrapper struct.

### Empty List

When \`head\` is \`NULL\`, the loop body never runs and the function returns \`0\`. Always handle the empty case.

### Your Task

Implement \`int list_length(struct Node *head)\` that returns the number of nodes.`,

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

int list_length(struct Node *head) {
\t// Return the number of nodes
\treturn 0;
}

int main() {
\tstruct Node *head = new_node(1);
\thead->next = new_node(2);
\thead->next->next = new_node(3);
\tprintf("%d\\n", list_length(head));
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

int list_length(struct Node *head) {
\tint count = 0;
\tstruct Node *cur = head;
\twhile (cur != NULL) {
\t\tcount++;
\t\tcur = cur->next;
\t}
\treturn count;
}

int main() {
\tstruct Node *head = new_node(1);
\thead->next = new_node(2);
\thead->next->next = new_node(3);
\tprintf("%d\\n", list_length(head));
\treturn 0;
}
`,

	tests: [
		{
			name: "length of [1,2,3] = 3",
			expected: "3\n",
		},
		{
			name: "empty list = 0",
			code: `#include <stdio.h>
#include <stdlib.h>
{{FUNC}}
int main() {
\tprintf("%d\\n", list_length(NULL));
\treturn 0;
}`,
			expected: "0\n",
		},
		{
			name: "single node = 1",
			code: `#include <stdio.h>
#include <stdlib.h>
{{FUNC}}
int main() {
\tstruct Node *head = new_node(99);
\tprintf("%d\\n", list_length(head));
\treturn 0;
}`,
			expected: "1\n",
		},
		{
			name: "length of 5-node list = 5",
			code: `#include <stdio.h>
#include <stdlib.h>
{{FUNC}}
int main() {
\tstruct Node *head = new_node(1);
\tstruct Node *cur = head;
\tcur->next = new_node(2); cur = cur->next;
\tcur->next = new_node(3); cur = cur->next;
\tcur->next = new_node(4); cur = cur->next;
\tcur->next = new_node(5);
\tprintf("%d\\n", list_length(head));
\treturn 0;
}`,
			expected: "5\n",
		},
	],
};
