import type { Lesson } from "../../types";

export const readonly: Lesson = {
	id: "readonly",
	title: "Readonly",
	chapterId: "advanced",
	content: `## Readonly

The \`readonly\` modifier prevents a property from being reassigned after initialization:

\`\`\`ts
interface Point {
    readonly x: number;
    readonly y: number;
}

const p: Point = { x: 3, y: 4 };
// p.x = 10;  // Error: Cannot assign to 'x' because it is a read-only property
\`\`\`

### Readonly Arrays

\`ReadonlyArray<T>\` (or \`readonly T[]\`) prevents mutation:

\`\`\`ts
const nums: readonly number[] = [1, 2, 3];
// nums.push(4);  // Error: Property 'push' does not exist on type 'readonly number[]'
console.log(nums[0]);  // 1
\`\`\`

### Readonly in Classes

\`\`\`ts
class Config {
    readonly host: string;
    readonly port: number;

    constructor(host: string, port: number) {
        this.host = host;
        this.port = port;
    }

    toString(): string {
        return this.host + ":" + this.port;
    }
}

const cfg = new Config("localhost", 8080);
console.log(cfg.toString());  // localhost:8080
// cfg.port = 9090;           // Error!
\`\`\`

### Your Task

Define \`interface Config\` with \`readonly host: string\` and \`readonly port: number\`. Write \`function configString(c: Config): string\` that returns \`c.host + ":" + c.port\`.`,

	starterCode: `interface Config {
\treadonly host: string;
\treadonly port: number;
}

function configString(c: Config): string {
\t// Return host + ":" + port
}

console.log(configString({ host: "localhost", port: 8080 }));
console.log(configString({ host: "example.com", port: 443 }));
`,

	solution: `interface Config {
\treadonly host: string;
\treadonly port: number;
}

function configString(c: Config): string {
\treturn c.host + ":" + c.port;
}

console.log(configString({ host: "localhost", port: 8080 }));
console.log(configString({ host: "example.com", port: 443 }));
`,

	tests: [
		{
			name: "localhost:8080 and example.com:443",
			expected: "localhost:8080\nexample.com:443\n",
		},
		{
			name: "custom host and port",
			code: `{{FUNC}}
console.log(configString({ host: "api.dev", port: 3000 }));`,
			expected: "api.dev:3000\n",
		},
		{
			name: "another config",
			code: `{{FUNC}}
console.log(configString({ host: "db.internal", port: 5432 }));`,
			expected: "db.internal:5432\n",
		},
	],
};
