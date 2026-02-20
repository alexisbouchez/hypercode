import type { Lesson } from "../../types";

export const deleteVal: Lesson = {
	id: "delete-val",
	title: "Delete by Value",
	chapterId: "deletions",
	content: `## Deleting a Node by Value

Deleting a node from the middle of the list requires a **previous pointer** — you need to update the preceding node's \`next\` to skip the deleted node.

\`\`\`c
struct Node *delete_val(struct Node *head, int val) {
    if (head == NULL) return NULL;
    if (head->val == val) {
        struct Node *next = head->next;
        free(head);
        return next;
    }
    struct Node *prev = head;
    struct Node *cur = head->next;
    while (cur != NULL) {
        if (cur->val == val) {
            prev->next = cur->next;
            free(cur);
            return head;
        }
        prev = cur;
        cur = cur->next;
    }
    return head;
}
\`\`\`

### The Three Cases

1. **List is empty** — return \`NULL\`
2. **Head matches** — new head is \`head->next\`; free old head
3. **Middle/tail matches** — set \`prev->next = cur->next\`; free \`cur\`

\`\`\`
Delete 3 from [1] -> [2] -> [3] -> [4] -> NULL
                         prev  cur
                         [2] -> [3] -> [4]
                         [2]  →  ×  →  [4]
Result: [1] -> [2] -> [4] -> NULL
\`\`\`

### Only the First Occurrence

This removes the first node with the matching value. If duplicates exist, they remain.

### Your Task

Implement \`struct Node *delete_val(struct Node *head, int val)\` that removes the first occurrence of \`val\` and returns the new head.`,

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

void print_list(struct Node *head) {
\tstruct Node *cur = head;
\twhile (cur != NULL) { printf("%d\\n", cur->val); cur = cur->next; }
}

struct Node *delete_val(struct Node *head, int val) {
\t// Remove first node with val, return new head
\treturn head;
}

int main() {
\tstruct Node *head = NULL;
\thead = push_back(head, 1);
\thead = push_back(head, 2);
\thead = push_back(head, 3);
\thead = push_back(head, 4);
\thead = push_back(head, 5);
\thead = delete_val(head, 3);
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

struct Node *push_back(struct Node *head, int val) {
\tstruct Node *n = new_node(val);
\tif (head == NULL) return n;
\tstruct Node *cur = head;
\twhile (cur->next != NULL) cur = cur->next;
\tcur->next = n;
\treturn head;
}

void print_list(struct Node *head) {
\tstruct Node *cur = head;
\twhile (cur != NULL) { printf("%d\\n", cur->val); cur = cur->next; }
}

struct Node *delete_val(struct Node *head, int val) {
\tif (head == NULL) return NULL;
\tif (head->val == val) {
\t\tstruct Node *next = head->next;
\t\tfree(head);
\t\treturn next;
\t}
\tstruct Node *prev = head;
\tstruct Node *cur = head->next;
\twhile (cur != NULL) {
\t\tif (cur->val == val) {
\t\t\tprev->next = cur->next;
\t\t\tfree(cur);
\t\t\treturn head;
\t\t}
\t\tprev = cur;
\t\tcur = cur->next;
\t}
\treturn head;
}

int main() {
\tstruct Node *head = NULL;
\thead = push_back(head, 1);
\thead = push_back(head, 2);
\thead = push_back(head, 3);
\thead = push_back(head, 4);
\thead = push_back(head, 5);
\thead = delete_val(head, 3);
\tprint_list(head);
\treturn 0;
}
`,

	tests: [
		{
			name: "delete 3 from [1,2,3,4,5]",
			expected: "1\n2\n4\n5\n",
		},
		{
			name: "delete head",
			code: `#include <stdio.h>
#include <stdlib.h>
{{FUNC}}
int main() {
\tstruct Node *head = NULL;
\thead = push_back(head, 1);
\thead = push_back(head, 2);
\thead = push_back(head, 3);
\thead = delete_val(head, 1);
\tprint_list(head);
\treturn 0;
}`,
			expected: "2\n3\n",
		},
		{
			name: "delete tail",
			code: `#include <stdio.h>
#include <stdlib.h>
{{FUNC}}
int main() {
\tstruct Node *head = NULL;
\thead = push_back(head, 1);
\thead = push_back(head, 2);
\thead = push_back(head, 3);
\thead = delete_val(head, 3);
\tprint_list(head);
\treturn 0;
}`,
			expected: "1\n2\n",
		},
		{
			name: "delete value not in list",
			code: `#include <stdio.h>
#include <stdlib.h>
{{FUNC}}
int main() {
\tstruct Node *head = NULL;
\thead = push_back(head, 1);
\thead = push_back(head, 2);
\thead = delete_val(head, 99);
\tprint_list(head);
\treturn 0;
}`,
			expected: "1\n2\n",
		},
	],
};
