import type { Lesson } from "../../types";

export const interfaces: Lesson = {
	id: "interfaces",
	title: "Interfaces",
	chapterId: "oop",
	content: `## Interfaces

An interface defines a **contract** — a set of methods a class must implement:

\`\`\`java
interface Drawable {
    void draw();                    // abstract method
    default String color() {        // default method (Java 8+)
        return "black";
    }
}

class Circle implements Drawable {
    @Override
    public void draw() {
        System.out.println("Drawing a circle");
    }
}
\`\`\`

### Multiple Interfaces

A class can implement multiple interfaces (unlike extends):

\`\`\`java
class Square implements Drawable, Resizable {
    // must implement all abstract methods from both
}
\`\`\`

### Interface vs Abstract Class

| | Interface | Abstract Class |
|---|---|---|
| Fields | constants only | any |
| Methods | abstract + default | any |
| Inheritance | multiple | single |

### Your Task

Create:
- \`Printable\` interface with \`void print()\`
- \`Measurable\` interface with \`double measure()\` and a default method \`summary()\` returning \`"Measurement: " + measure()\`
- \`Box\` implementing both, with fields \`width\`, \`height\`, \`depth\`

Create \`Box(2.0, 3.0, 4.0)\`, call \`print()\`, \`measure()\`, and \`summary()\`.`,

	starterCode: `public class Main {
    interface Printable {
        void print();
    }

    interface Measurable {
        double measure();

        default String summary() {
            return "Measurement: " + measure();
        }
    }

    static class Box implements Printable, Measurable {
        double width, height, depth;

        Box(double width, double height, double depth) {
            this.width = width;
            this.height = height;
            this.depth = depth;
        }

        @Override
        public void print() {
            // print "Box(<width>x<height>x<depth>)"
        }

        @Override
        public double measure() {
            // return volume
            return 0;
        }
    }

    public static void main(String[] args) {
        Box b = new Box(2.0, 3.0, 4.0);
        b.print();
        System.out.println(b.measure());
        System.out.println(b.summary());
    }
}
`,

	solution: `public class Main {
    interface Printable {
        void print();
    }

    interface Measurable {
        double measure();

        default String summary() {
            return "Measurement: " + measure();
        }
    }

    static class Box implements Printable, Measurable {
        double width, height, depth;

        Box(double width, double height, double depth) {
            this.width = width;
            this.height = height;
            this.depth = depth;
        }

        @Override
        public void print() {
            System.out.println("Box(" + width + "x" + height + "x" + depth + ")");
        }

        @Override
        public double measure() {
            return width * height * depth;
        }
    }

    public static void main(String[] args) {
        Box b = new Box(2.0, 3.0, 4.0);
        b.print();
        System.out.println(b.measure());
        System.out.println(b.summary());
    }
}
`,

	tests: [
		{
			name: "Box print, volume, summary",
			expected: "Box(2.0x3.0x4.0)\n24.0\nMeasurement: 24.0\n",
		},
		{
			name: "unit cube (1x1x1)",
			expected: "Box(1.0x1.0x1.0)\n1.0\nMeasurement: 1.0\n",
			code: `public class Main {
    interface Printable { void print(); }
    interface Measurable {
        double measure();
        default String summary() { return "Measurement: " + measure(); }
    }
    static class Box implements Printable, Measurable {
        double width, height, depth;
        Box(double w, double h, double d) { width = w; height = h; depth = d; }
        @Override
        public void print() {
            System.out.println("Box(" + width + "x" + height + "x" + depth + ")");
        }
        @Override
        public double measure() { return width * height * depth; }
    }
    public static void main(String[] args) {
        Box b = new Box(1.0, 1.0, 1.0);
        b.print();
        System.out.println(b.measure());
        System.out.println(b.summary());
    }
}
`,
		},
		{
			name: "default summary method uses measure",
			expected: "Measurement: 60.0\n",
			code: `public class Main {
    interface Measurable {
        double measure();
        default String summary() { return "Measurement: " + measure(); }
    }
    static class Box implements Measurable {
        double width, height, depth;
        Box(double w, double h, double d) { width = w; height = h; depth = d; }
        @Override
        public double measure() { return width * height * depth; }
    }
    public static void main(String[] args) {
        Box b = new Box(3.0, 4.0, 5.0);
        System.out.println(b.summary());
    }
}
`,
		},
	],
};
