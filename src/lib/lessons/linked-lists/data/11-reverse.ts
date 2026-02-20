import type { Lesson } from "../../types";

export const reverse: Lesson = {
	id: "reverse",
	title: "Reverse a List",
	chapterId: "classic",
	content: `## Reversing a Linked List In-Place

To reverse a list, walk through it and flip each \`next\` pointer to point backward. You need three pointers: **prev**, **cur**, and **next**.

\`\`\`c
struct Node *reverse(struct Node *head) {
    struct Node *prev = NULL;
    struct Node *cur = head;
    while (cur != NULL) {
        struct Node *next = cur->next;
        cur->next = prev;
        prev = cur;
        cur = next;
    }
    return prev;
}
\`\`\`

### Step-by-Step

\`\`\`
Initial: NULL <- ? [1] -> [2] -> [3] -> NULL
         prev   cur

Iteration 1:
  next = cur->next   → [2]
  cur->next = prev   → [1] -> NULL
  prev = cur         → [1]
  cur = next         → [2]

Iteration 2:
  next = cur->next   → [3]
  cur->next = prev   → [2] -> [1] -> NULL
  prev = cur         → [2]
  cur = next         → [3]

Iteration 3:
  next = cur->next   → NULL
  cur->next = prev   → [3] -> [2] -> [1] -> NULL
  prev = cur         → [3]
  cur = next         → NULL  ← loop ends

Return prev → [3] (new head)
\`\`\`

### Your Task

Implement \`struct Node *reverse(struct Node *head)\` that reverses the list in-place and returns the new head.`,

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

struct Node *reverse(struct Node *head) {
\t// Reverse the list in-place, return new head
\treturn head;
}

int main() {
\tstruct Node *head = NULL;
\thead = push_back(head, 1);
\thead = push_back(head, 2);
\thead = push_back(head, 3);
\thead = push_back(head, 4);
\thead = push_back(head, 5);
\thead = reverse(head);
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

struct Node *reverse(struct Node *head) {
\tstruct Node *prev = NULL;
\tstruct Node *cur = head;
\twhile (cur != NULL) {
\t\tstruct Node *next = cur->next;
\t\tcur->next = prev;
\t\tprev = cur;
\t\tcur = next;
\t}
\treturn prev;
}

int main() {
\tstruct Node *head = NULL;
\thead = push_back(head, 1);
\thead = push_back(head, 2);
\thead = push_back(head, 3);
\thead = push_back(head, 4);
\thead = push_back(head, 5);
\thead = reverse(head);
\tprint_list(head);
\treturn 0;
}
`,

	tests: [
		{
			name: "reverse [1,2,3,4,5] → [5,4,3,2,1]",
			expected: "5\n4\n3\n2\n1\n",
		},
		{
			name: "reverse single node",
			code: `#include <stdio.h>
#include <stdlib.h>
{{FUNC}}
int main() {
\tstruct Node *head = new_node(7);
\thead = reverse(head);
\tprint_list(head);
\treturn 0;
}`,
			expected: "7\n",
		},
		{
			name: "reverse NULL returns NULL",
			code: `#include <stdio.h>
#include <stdlib.h>
{{FUNC}}
int main() {
\tstruct Node *head = reverse(NULL);
\tprintf("%d\\n", head == NULL ? 1 : 0);
\treturn 0;
}`,
			expected: "1\n",
		},
		{
			name: "reverse [1,2] → [2,1]",
			code: `#include <stdio.h>
#include <stdlib.h>
{{FUNC}}
int main() {
\tstruct Node *head = NULL;
\thead = push_back(head, 1);
\thead = push_back(head, 2);
\thead = reverse(head);
\tprint_list(head);
\treturn 0;
}`,
			expected: "2\n1\n",
		},
	],
};
