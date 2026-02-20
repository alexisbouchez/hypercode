import type { Lesson } from "../../types";

export const literalTypes: Lesson = {
	id: "literal-types",
	title: "Literal Types",
	chapterId: "type-system",
	content: `## Literal Types

A **literal type** pins a variable to a specific value, not just a general type:

\`\`\`ts
let direction: "north" | "south" | "east" | "west";
direction = "north";  // OK
direction = "up";     // Error: '"up"' is not assignable to type '"north" | "south" | "east" | "west"'
\`\`\`

Literal types are string, number, or boolean values used as types.

### Why Literal Types?

They let you describe a limited set of valid values — like an enum, but lighter:

\`\`\`ts
type Status = "pending" | "active" | "closed";
type Coin = "heads" | "tails";
type HttpMethod = "GET" | "POST" | "PUT" | "DELETE";
\`\`\`

### Using Literal Types in Functions

\`\`\`ts
type Direction = "up" | "down" | "left" | "right";

function move(dir: Direction, steps: number): string {
    return "Move " + steps + " steps " + dir;
}

console.log(move("up", 3));    // Move 3 steps up
console.log(move("left", 1));  // Move 1 steps left
\`\`\`

### Your Task

Define \`type Suit = "clubs" | "diamonds" | "hearts" | "spades"\`. Write \`function cardSuit(suit: Suit): string\` that returns \`"The suit is: [suit]"\`.`,

	starterCode: `type Suit = "clubs" | "diamonds" | "hearts" | "spades";

function cardSuit(suit: Suit): string {
\t// Return "The suit is: " + suit
}

console.log(cardSuit("hearts"));
console.log(cardSuit("spades"));
`,

	solution: `type Suit = "clubs" | "diamonds" | "hearts" | "spades";

function cardSuit(suit: Suit): string {
\treturn "The suit is: " + suit;
}

console.log(cardSuit("hearts"));
console.log(cardSuit("spades"));
`,

	tests: [
		{
			name: "hearts and spades",
			expected: "The suit is: hearts\nThe suit is: spades\n",
		},
		{
			name: "clubs",
			code: `{{FUNC}}
console.log(cardSuit("clubs"));`,
			expected: "The suit is: clubs\n",
		},
		{
			name: "diamonds",
			code: `{{FUNC}}
console.log(cardSuit("diamonds"));`,
			expected: "The suit is: diamonds\n",
		},
	],
};
