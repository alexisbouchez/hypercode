import type { Lesson } from "../../types";

export const accessModifiers: Lesson = {
	id: "access-modifiers",
	title: "Access Modifiers",
	chapterId: "oop",
	content: `## Access Modifiers

Java has four access levels that control visibility of classes, fields, and methods. Understanding them is essential for proper **encapsulation**.

### The Four Access Levels

| Modifier | Class | Package | Subclass | World |
|----------|-------|---------|----------|-------|
| \`public\` | Yes | Yes | Yes | Yes |
| \`protected\` | Yes | Yes | Yes | No |
| *(default)* | Yes | Yes | No | No |
| \`private\` | Yes | No | No | No |

### Private

\`private\` members are only accessible within the declaring class. This is the most restrictive level and the default choice for fields:

\`\`\`java
class BankAccount {
    private double balance;

    public double getBalance() { return balance; }
    public void deposit(double amount) {
        if (amount > 0) balance += amount;
    }
}
\`\`\`

By making \`balance\` private, we force all access through methods that can validate input. This is the core of encapsulation.

### Package-Private (Default)

When you write **no modifier**, the member is visible to all classes in the same package. This is called *package-private* or *default* access:

\`\`\`java
class Helper {
    int count = 0;           // package-private field
    void increment() { count++; } // package-private method
}
\`\`\`

Use this when classes within the same package need to collaborate closely, but outsiders should not depend on the implementation.

### Protected

\`protected\` is like package-private but also grants access to **subclasses** in other packages:

\`\`\`java
class Shape {
    protected String color;

    protected void describe() {
        System.out.println("A " + color + " shape");
    }
}

class Circle extends Shape {
    Circle(String color) {
        this.color = color;  // OK — subclass access
    }
}
\`\`\`

### Public

\`public\` members are accessible from anywhere. Use this for the API your class exposes to the outside world.

### Best Practices

1. **Start with \`private\`** — only widen access when you have a clear reason.
2. **Expose behavior, not data** — provide methods instead of public fields.
3. **Use \`protected\` sparingly** — it couples subclasses to implementation details.
4. **Prefer package-private for internal helpers** — they can change without breaking external code.

### Your Task

1. Create a class \`User\` with:
   - A \`private String name\` and \`private String email\` field
   - A \`public\` constructor taking both values
   - A \`public String getName()\` getter
   - A \`public String getEmail()\` getter
   - A \`private String maskEmail()\` method that returns the email with everything before \`@\` replaced by \`"***"\` (e.g. \`"alice@example.com"\` → \`"***@example.com"\`)
   - A \`public String getPublicProfile()\` that returns \`name + " <" + maskEmail() + ">"\`

2. Create a class \`Admin\` that extends \`User\` with:
   - A \`protected String role\` field
   - A constructor taking name, email, and role
   - A \`public String getPublicProfile()\` override that returns \`name + " [" + role + "] <" + maskEmail() + ">"\` — but since \`maskEmail()\` is private, you must call \`getEmail()\` and do the masking yourself

3. In \`main\`, create a \`User("Alice", "alice@example.com")\` and an \`Admin("Bob", "bob@corp.io", "SuperAdmin")\`. Print each one's public profile.`,

	starterCode: `public class Main {
    static class User {
        // private String name, email
        // public constructor, getters
        // private String maskEmail()
        // public String getPublicProfile()
    }

    static class Admin extends User {
        // protected String role
        // constructor
        // override getPublicProfile()
    }

    public static void main(String[] args) {
        // Create User and Admin, print public profiles
    }
}
`,

	solution: `public class Main {
    static class User {
        private String name;
        private String email;

        public User(String name, String email) {
            this.name = name;
            this.email = email;
        }

        public String getName() { return name; }
        public String getEmail() { return email; }

        private String maskEmail() {
            int at = email.indexOf('@');
            return "***" + email.substring(at);
        }

        public String getPublicProfile() {
            return name + " <" + maskEmail() + ">";
        }
    }

    static class Admin extends User {
        protected String role;

        public Admin(String name, String email, String role) {
            super(name, email);
            this.role = role;
        }

        public String getPublicProfile() {
            String email = getEmail();
            int at = email.indexOf('@');
            String masked = "***" + email.substring(at);
            return getName() + " [" + role + "] <" + masked + ">";
        }
    }

    public static void main(String[] args) {
        User alice = new User("Alice", "alice@example.com");
        Admin bob = new Admin("Bob", "bob@corp.io", "SuperAdmin");
        System.out.println(alice.getPublicProfile());
        System.out.println(bob.getPublicProfile());
    }
}
`,

	tests: [
		{
			name: "user and admin public profiles",
			expected:
				"Alice <***@example.com>\nBob [SuperAdmin] <***@corp.io>\n",
		},
		{
			name: "encapsulation with different users",
			expected: "Charlie <***@test.org>\nDiana [Moderator] <***@site.net>\n",
			code: `public class Main {
    static class User {
        private String name;
        private String email;
        public User(String name, String email) { this.name = name; this.email = email; }
        public String getName() { return name; }
        public String getEmail() { return email; }
        private String maskEmail() {
            int at = email.indexOf('@');
            return "***" + email.substring(at);
        }
        public String getPublicProfile() {
            return name + " <" + maskEmail() + ">";
        }
    }
    static class Admin extends User {
        protected String role;
        public Admin(String name, String email, String role) {
            super(name, email);
            this.role = role;
        }
        public String getPublicProfile() {
            String email = getEmail();
            int at = email.indexOf('@');
            String masked = "***" + email.substring(at);
            return getName() + " [" + role + "] <" + masked + ">";
        }
    }
    public static void main(String[] args) {
        User c = new User("Charlie", "charlie@test.org");
        Admin d = new Admin("Diana", "diana@site.net", "Moderator");
        System.out.println(c.getPublicProfile());
        System.out.println(d.getPublicProfile());
    }
}
`,
		},
		{
			name: "getters return correct values",
			expected: "Eve\neve@domain.com\n",
			code: `public class Main {
    static class User {
        private String name;
        private String email;
        public User(String name, String email) { this.name = name; this.email = email; }
        public String getName() { return name; }
        public String getEmail() { return email; }
        private String maskEmail() {
            int at = email.indexOf('@');
            return "***" + email.substring(at);
        }
        public String getPublicProfile() {
            return name + " <" + maskEmail() + ">";
        }
    }
    public static void main(String[] args) {
        User eve = new User("Eve", "eve@domain.com");
        System.out.println(eve.getName());
        System.out.println(eve.getEmail());
    }
}
`,
		},
	],
};
