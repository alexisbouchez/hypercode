import type { Lesson } from "../../types";

export const interfaces: Lesson = {
	id: "interfaces",
	title: "Interfaces",
	chapterId: "oop",
	content: `## Interfaces

An interface defines a contract — a set of methods and properties that implementing classes must provide:

\`\`\`csharp
interface ILogger {
    void Log(string message);
    string Prefix { get; }
}

class ConsoleLogger : ILogger {
    public string Prefix => "[INFO]";
    public void Log(string message) => Console.WriteLine($"{Prefix} {message}");
}

ILogger logger = new ConsoleLogger();
logger.Log("Application started"); // [INFO] Application started
\`\`\`

### Multiple Interfaces

A class can implement multiple interfaces (unlike inheritance, which allows only one base class):

\`\`\`csharp
interface ISaveable { void Save(); }
interface ILoadable { void Load(); }

class Document : ISaveable, ILoadable {
    public void Save() => Console.WriteLine("Saving...");
    public void Load() => Console.WriteLine("Loading...");
}
\`\`\`

### Your Task

Define an interface \`IGreeter\` with:
- \`string Greet(string name)\` method

Then create two implementations:
- \`FormalGreeter\` — returns \`"Good day, {name}."\`
- \`CasualGreeter\` — returns \`"Hey, {name}!"\``,

	starterCode: `interface IGreeter
{
    string Greet(string name);
}

class FormalGreeter : IGreeter
{
    // Implement Greet
}

class CasualGreeter : IGreeter
{
    // Implement Greet
}
`,

	solution: `interface IGreeter
{
    string Greet(string name);
}

class FormalGreeter : IGreeter
{
    public string Greet(string name) => $"Good day, {name}.";
}

class CasualGreeter : IGreeter
{
    public string Greet(string name) => $"Hey, {name}!";
}
`,

	tests: [
		{
			name: "formal greeting",
			expected: "Good day, Alice.\n",
			code: `IGreeter g = new FormalGreeter();
Console.WriteLine(g.Greet("Alice"));
{{FUNC}}`,
		},
		{
			name: "casual greeting",
			expected: "Hey, Bob!\n",
			code: `IGreeter g = new CasualGreeter();
Console.WriteLine(g.Greet("Bob"));
{{FUNC}}`,
		},
	],
};
