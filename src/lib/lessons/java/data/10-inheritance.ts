import type { Lesson } from "../../types";

export const inheritance: Lesson = {
	id: "inheritance",
	title: "Inheritance",
	chapterId: "oop",
	content: `## Inheritance

A subclass **extends** a parent class, inheriting its fields and methods:

\`\`\`java
class Animal {
    String name;
    Animal(String name) { this.name = name; }
    String speak() { return name + " makes a sound"; }
}

class Dog extends Animal {
    Dog(String name) { super(name); }  // call parent constructor

    @Override
    String speak() { return name + " barks"; }  // override
}

Animal a = new Dog("Rex");
System.out.println(a.speak());  // Rex barks
\`\`\`

### Key Points

- \`extends\` declares inheritance
- \`super(args)\` calls the parent constructor (must be first line)
- \`@Override\` marks overridden methods (optional but good practice)
- Java has **single inheritance** for classes

### Polymorphism

An \`Animal\` reference can hold any subclass:

\`\`\`java
Animal[] pets = {new Dog("Rex"), new Cat("Luna")};
for (Animal a : pets) {
    System.out.println(a.speak());  // calls the right version
}
\`\`\`

### Your Task

Create \`Animal\`, \`Dog\`, and \`Cat\` classes. \`Dog.speak()\` appends \`"!"\` to the parent's result. Print each animal's \`speak()\` as shown.`,

	starterCode: `public class Main {
    static class Animal {
        private String name;
        private String sound;

        Animal(String name, String sound) {
            this.name = name;
            this.sound = sound;
        }

        String speak() {
            return name + " says " + sound;
        }
    }

    static class Dog extends Animal {
        Dog(String name) {
            super(name, "woof");
        }

        @Override
        String speak() {
            // return super.speak() + "!"
            return super.speak();
        }
    }

    static class Cat extends Animal {
        Cat(String name) {
            super(name, "meow");
        }
    }

    public static void main(String[] args) {
        Animal[] pets = {new Dog("Rex"), new Cat("Luna"), new Dog("Buddy")};
        for (Animal a : pets) {
            System.out.println(a.speak());
        }
    }
}
`,

	solution: `public class Main {
    static class Animal {
        private String name;
        private String sound;

        Animal(String name, String sound) {
            this.name = name;
            this.sound = sound;
        }

        String speak() {
            return name + " says " + sound;
        }
    }

    static class Dog extends Animal {
        Dog(String name) {
            super(name, "woof");
        }

        @Override
        String speak() {
            return super.speak() + "!";
        }
    }

    static class Cat extends Animal {
        Cat(String name) {
            super(name, "meow");
        }
    }

    public static void main(String[] args) {
        Animal[] pets = {new Dog("Rex"), new Cat("Luna"), new Dog("Buddy")};
        for (Animal a : pets) {
            System.out.println(a.speak());
        }
    }
}
`,

	tests: [
		{
			name: "Rex barks, Luna meows, Buddy barks",
			expected: "Rex says woof!\nLuna says meow\nBuddy says woof!\n",
		},
	],
};
