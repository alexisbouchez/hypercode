import type { Lesson } from "../../types";

export const inheritance: Lesson = {
  id: "inheritance",
  title: "Inheritance",
  chapterId: "classes-and-structures",
  content: `## Inheritance

HolyC supports a form of class inheritance. A class can embed another class as its first member, and in some HolyC implementations, a child class can be used wherever the parent class is expected.

### Embedding (Composition)

The most reliable pattern is explicit embedding — include the parent class as a named member:

\`\`\`holyc
class Animal {
  U8 *name;
  U8 *sound;
};

class Dog {
  Animal base;
  U8 *breed;
};

Dog d;
d.base.name  = "Rex";
d.base.sound = "Woof";
d.breed      = "Labrador";

Print("%s (%s) says %s\\n", d.base.name, d.breed, d.base.sound);
// Rex (Labrador) says Woof
\`\`\`

### Polymorphism via Pointers

By casting a \`Dog *\` to an \`Animal *\`, you can pass different "subtypes" to the same function:

\`\`\`holyc
class Animal {
  U8 *name;
  U8 *sound;
};

U0 Speak(Animal *a) {
  Print("%s says %s\\n", a->name, a->sound);
}

Animal cat;
cat.name  = "Cat";
cat.sound = "Meow";

Animal dog;
dog.name  = "Dog";
dog.sound = "Woof";

Speak(&cat);
Speak(&dog);
\`\`\`

This works because both \`cat\` and \`dog\` are the same type (\`Animal\`), but in larger programs you would use the embedding approach and cast the pointer.

### Your Task

Define an \`Animal\` class with \`U8 *name\` and \`U8 *sound\` fields. Write a \`U0 Speak(Animal *a)\` function that prints \`NAME says SOUND\`.

Create two animals: a \`"Cow"\` that says \`"Moo"\` and a \`"Duck"\` that says \`"Quack"\`. Call \`Speak\` on each.

Expected output:
\`\`\`
Cow says Moo
Duck says Quack
\`\`\``,

  starterCode: `// Define Animal class and Speak function

// Create Cow and Duck animals and call Speak on each
`,

  solution: `class Animal {
  U8 *name;
  U8 *sound;
};

U0 Speak(Animal *a) {
  Print("%s says %s\\n", a->name, a->sound);
}

Animal cow;
cow.name  = "Cow";
cow.sound = "Moo";

Animal duck;
duck.name  = "Duck";
duck.sound = "Quack";

Speak(&cow);
Speak(&duck);
`,

  tests: [
    {
      name: "prints animal sounds",
      expected: "Cow says Moo\nDuck says Quack\n",
    },
  ],
};
