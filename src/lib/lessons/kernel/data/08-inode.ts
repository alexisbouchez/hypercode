import type { Lesson } from "../../types";

export const inode: Lesson = {
	id: "inode",
	title: "Inode",
	chapterId: "filesystem",
	content: `## The Inode

Every file and directory in a Linux filesystem is represented by an **inode** (index node). The inode stores file metadata but **not the filename** — names live in directory entries, which point to inodes by number.

\`\`\`c
typedef struct {
    int   ino;       // inode number (unique within filesystem)
    int   mode;      // permissions: 0644 = rw-r--r--
    int   nlink;     // number of hard links
    int   uid;       // owner user ID
    int   gid;       // owner group ID
    int   size;      // file size in bytes
    int   blocks;    // number of 512-byte blocks allocated
} Inode;
\`\`\`

This mirrors the \`stat\` struct you get from \`stat(2)\` and the output of \`ls -li\`.

### Printing an Inode

\`stat\` output looks like this:

\`\`\`
inode:  42
mode:   644
links:  1
uid:    1000
gid:    1000
size:   1024
blocks: 8
\`\`\`

### Your Task

Implement \`void print_inode(Inode *ino)\` that prints each field in the format above.`,

	starterCode: `#include <stdio.h>

typedef struct {
\tint ino;
\tint mode;
\tint nlink;
\tint uid;
\tint gid;
\tint size;
\tint blocks;
} Inode;

void print_inode(Inode *n) {
\t// Print each field: "inode:  N\\n" etc.
}

int main() {
\tInode n = {42, 644, 1, 1000, 1000, 1024, 8};
\tprint_inode(&n);
\treturn 0;
}
`,

	solution: `#include <stdio.h>

typedef struct {
\tint ino;
\tint mode;
\tint nlink;
\tint uid;
\tint gid;
\tint size;
\tint blocks;
} Inode;

void print_inode(Inode *n) {
\tprintf("inode:  %d\\n", n->ino);
\tprintf("mode:   %d\\n", n->mode);
\tprintf("links:  %d\\n", n->nlink);
\tprintf("uid:    %d\\n", n->uid);
\tprintf("gid:    %d\\n", n->gid);
\tprintf("size:   %d\\n", n->size);
\tprintf("blocks: %d\\n", n->blocks);
}

int main() {
\tInode n = {42, 644, 1, 1000, 1000, 1024, 8};
\tprint_inode(&n);
\treturn 0;
}
`,

	tests: [
		{
			name: "prints inode fields",
			expected: "inode:  42\nmode:   644\nlinks:  1\nuid:    1000\ngid:    1000\nsize:   1024\nblocks: 8\n",
		},
		{
			name: "directory inode with multiple links",
			code: `#include <stdio.h>
{{FUNC}}
int main() {
\tInode n = {1, 755, 3, 0, 0, 4096, 8};
\tprint_inode(&n);
\treturn 0;
}`,
			expected: "inode:  1\nmode:   755\nlinks:  3\nuid:    0\ngid:    0\nsize:   4096\nblocks: 8\n",
		},
		{
			name: "empty file has size 0",
			code: `#include <stdio.h>
{{FUNC}}
int main() {
\tInode n = {7, 600, 1, 500, 500, 0, 0};
\tprint_inode(&n);
\treturn 0;
}`,
			expected: "inode:  7\nmode:   600\nlinks:  1\nuid:    500\ngid:    500\nsize:   0\nblocks: 0\n",
		},
		{
			name: "root inode",
			code: `#include <stdio.h>
{{FUNC}}
int main() {
\tInode n = {2, 755, 20, 0, 0, 4096, 8};
\tprint_inode(&n);
\treturn 0;
}`,
			expected: "inode:  2\nmode:   755\nlinks:  20\nuid:    0\ngid:    0\nsize:   4096\nblocks: 8\n",
		},
	],
};
