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

### \`as const\` for Literal Types

When you declare a variable with \`const\`, TypeScript infers a literal type for primitives:

\`\`\`ts
const x = "hello";  // type is "hello", not string
\`\`\`

But objects and arrays are still mutable, so TypeScript widens their types:

\`\`\`ts
const config = { mode: "dark", level: 5 };
// type is { mode: string; level: number } — not literal
\`\`\`

Use \`as const\` to make the entire value deeply readonly with literal types:

\`\`\`ts
const config = { mode: "dark", level: 5 } as const;
// type is { readonly mode: "dark"; readonly level: 5 }
\`\`\`

This is especially useful for arrays that should be treated as tuples:

\`\`\`ts
const pair = [1, 2] as const;       // readonly [1, 2]
const dirs = ["up", "down"] as const; // readonly ["up", "down"]
\`\`\`

### Your Task

1. Define \`type Suit = "clubs" | "diamonds" | "hearts" | "spades"\`. Write \`function cardSuit(suit: Suit): string\` that returns \`"The suit is: [suit]"\`.
2. Create a \`const COLORS = ["red", "green", "blue"] as const\`. Write \`function describeColors(): string\` that returns the length and first element: \`"3 colors, first is red"\`.`,

	starterCode: `type Suit = "clubs" | "diamonds" | "hearts" | "spades";

function cardSuit(suit: Suit): string {
\t// Return "The suit is: " + suit
}

function describeColors(): string {
\tconst COLORS = ["red", "green", "blue"] as const;
\t// Return COLORS.length + " colors, first is " + COLORS[0]
}

console.log(cardSuit("hearts"));
console.log(cardSuit("spades"));
console.log(describeColors());
`,

	solution: `type Suit = "clubs" | "diamonds" | "hearts" | "spades";

function cardSuit(suit: Suit): string {
\treturn "The suit is: " + suit;
}

function describeColors(): string {
\tconst COLORS = ["red", "green", "blue"] as const;
\treturn COLORS.length + " colors, first is " + COLORS[0];
}

console.log(cardSuit("hearts"));
console.log(cardSuit("spades"));
console.log(describeColors());
`,

	tests: [
		{
			name: "hearts, spades, and describeColors",
			expected: "The suit is: hearts\nThe suit is: spades\n3 colors, first is red\n",
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
		{
			name: "describeColors returns correct format",
			code: `{{FUNC}}
console.log(describeColors());`,
			expected: "3 colors, first is red\n",
		},
	],
};
