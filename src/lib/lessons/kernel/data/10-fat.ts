import type { Lesson } from "../../types";

export const fat: Lesson = {
	id: "fat",
	title: "File Allocation Table",
	chapterId: "filesystem",
	content: `## The File Allocation Table

**FAT** (File Allocation Table) is a simple filesystem used on USB drives and SD cards. It stores a linked list of disk blocks for each file.

The FAT is an array indexed by **block number**. Each entry is either:
- the **next block** in the file's chain
- **-1** (end of file — \`EOF\`)
- **0** (free block)

\`\`\`
block:  0  1  2  3  4  5  6  7
fat:   -1  3  0  5  0  7  0 -1
             ↑
         block 1 → 3 → 5 → 7 → EOF
\`\`\`

To read a file starting at block 1: follow the chain 1 → 3 → 5 → 7 → EOF.

### Your Implementation

Write \`void fat_chain(int fat[], int start)\` that follows the chain from \`start\` and prints each block number until EOF (\`-1\`):

\`\`\`c
void fat_chain(int fat[], int start) {
    int cur = start;
    while (cur != -1) {
        printf("%d\\n", cur);
        cur = fat[cur];
    }
}
\`\`\`

### Your Task

Implement \`fat_chain\` that traverses the FAT linked list from \`start\` and prints each block number.`,

	starterCode: `#include <stdio.h>

void fat_chain(int fat[], int start) {
\t// Follow the chain from start, printing each block until -1
}

int main() {
\tint fat[8] = {-1, 3, 0, 5, 0, 7, 0, -1};
\tfat_chain(fat, 1);
\treturn 0;
}
`,

	solution: `#include <stdio.h>

void fat_chain(int fat[], int start) {
\tint cur = start;
\twhile (cur != -1) {
\t\tprintf("%d\\n", cur);
\t\tcur = fat[cur];
\t}
}

int main() {
\tint fat[8] = {-1, 3, 0, 5, 0, 7, 0, -1};
\tfat_chain(fat, 1);
\treturn 0;
}
`,

	tests: [
		{
			name: "follows chain 1→3→5→7",
			expected: "1\n3\n5\n7\n",
		},
		{
			name: "single-block file",
			code: `#include <stdio.h>
{{FUNC}}
int main() {
\tint fat[4] = {-1, -1, 0, 0};
\tfat_chain(fat, 1);
\treturn 0;
}`,
			expected: "1\n",
		},
		{
			name: "two-block chain",
			code: `#include <stdio.h>
{{FUNC}}
int main() {
\tint fat[4] = {0, 2, -1, 0};
\tfat_chain(fat, 1);
\treturn 0;
}`,
			expected: "1\n2\n",
		},
		{
			name: "chain starting at block 0",
			code: `#include <stdio.h>
{{FUNC}}
int main() {
\tint fat[5] = {4, 0, 0, 0, -1};
\tfat_chain(fat, 0);
\treturn 0;
}`,
			expected: "0\n4\n",
		},
	],
};
