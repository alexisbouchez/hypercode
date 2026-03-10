import type { Lesson } from "../../types";

export const trie: Lesson = {
	id: "trie",
	title: "Trie",
	chapterId: "data-structures",
	content: `## Trie

A **Trie** (prefix tree) is a tree-like data structure used for efficiently storing and searching strings. Each node represents a single character, and paths from root to marked nodes form complete words.

### Node Structure

\`\`\`js
class TrieNode {
  constructor() {
    this.children = {};   // char → TrieNode
    this.isEnd = false;   // marks end of a word
  }
}
\`\`\`

### Insert

Walk down the tree character by character, creating nodes as needed. Mark the last node as a word end.

\`\`\`js
insert(word) {
  let node = this.root;
  for (const ch of word) {
    if (!node.children[ch]) node.children[ch] = new TrieNode();
    node = node.children[ch];
  }
  node.isEnd = true;
}
\`\`\`

### Search

Walk down the tree for each character. Return \`true\` only if every character exists **and** the final node is marked as a word end.

### StartsWith (Prefix Search)

Same as search, but you don't need \`isEnd\` to be true — just confirm every character in the prefix exists.

### Complexity

| Operation   | Time     |
|-------------|----------|
| insert      | O(m)     |
| search      | O(m)     |
| startsWith  | O(m)     |

Where \`m\` is the length of the word or prefix.

### Real-World Uses

- Autocomplete and search suggestions
- Spell checkers
- IP routing (longest prefix match)
- Word games (Boggle, Scrabble)

### Your Task

Implement a \`Trie\` class with \`insert\`, \`search\`, and \`startsWith\` methods.`,

	starterCode: `class TrieNode {
	constructor() {
		this.children = {};
		this.isEnd = false;
	}
}

class Trie {
	constructor() {
		this.root = new TrieNode();
	}

	insert(word) {
		// Insert word into the trie
	}

	search(word) {
		// Return true if the word exists in the trie
	}

	startsWith(prefix) {
		// Return true if any word starts with the given prefix
	}
}

const trie = new Trie();
trie.insert("apple");
trie.insert("app");
console.log(trie.search("apple"));
console.log(trie.search("app"));
console.log(trie.search("ap"));
console.log(trie.startsWith("ap"));
`,

	solution: `class TrieNode {
	constructor() {
		this.children = {};
		this.isEnd = false;
	}
}

class Trie {
	constructor() {
		this.root = new TrieNode();
	}

	insert(word) {
		let node = this.root;
		for (const ch of word) {
			if (!node.children[ch]) node.children[ch] = new TrieNode();
			node = node.children[ch];
		}
		node.isEnd = true;
	}

	search(word) {
		let node = this.root;
		for (const ch of word) {
			if (!node.children[ch]) return false;
			node = node.children[ch];
		}
		return node.isEnd;
	}

	startsWith(prefix) {
		let node = this.root;
		for (const ch of prefix) {
			if (!node.children[ch]) return false;
			node = node.children[ch];
		}
		return true;
	}
}

const trie = new Trie();
trie.insert("apple");
trie.insert("app");
console.log(trie.search("apple"));
console.log(trie.search("app"));
console.log(trie.search("ap"));
console.log(trie.startsWith("ap"));
`,

	tests: [
		{
			name: "search and startsWith basics",
			expected: "true\ntrue\nfalse\ntrue\n",
		},
		{
			name: "search for missing word",
			code: `{{FUNC}}
const trie = new Trie();
trie.insert("hello");
console.log(trie.search("hell"));
console.log(trie.search("hello"));
console.log(trie.search("helloo"));`,
			expected: "false\ntrue\nfalse\n",
		},
		{
			name: "startsWith with multiple words",
			code: `{{FUNC}}
const trie = new Trie();
trie.insert("car");
trie.insert("card");
trie.insert("care");
trie.insert("bat");
console.log(trie.startsWith("car"));
console.log(trie.startsWith("ca"));
console.log(trie.startsWith("bat"));
console.log(trie.startsWith("bad"));`,
			expected: "true\ntrue\ntrue\nfalse\n",
		},
		{
			name: "empty trie returns false",
			code: `{{FUNC}}
const trie = new Trie();
console.log(trie.search("any"));
console.log(trie.startsWith("a"));`,
			expected: "false\nfalse\n",
		},
	],
};
