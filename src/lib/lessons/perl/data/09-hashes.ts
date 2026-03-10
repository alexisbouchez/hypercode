import type { Lesson } from "../../types";

export const hashes: Lesson = {
  id: "hashes",
  title: "Hashes",
  chapterId: "data",
  content: `## Hashes

Hashes (associative arrays) map keys to values, prefixed with \`%\`:

\`\`\`perl
my %ages = (
    alice => 30,
    bob   => 25,
    carol => 35,
);
\`\`\`

The \`=>\` (fat comma) automatically quotes the key on its left.

### Accessing Values

Use \`$hash{key}\`:

\`\`\`perl
say $ages{alice};   # 30
say $ages{bob};     # 25
\`\`\`

### Adding / Modifying Entries

\`\`\`perl
$ages{dave} = 28;        # add new key
$ages{alice} = 31;       # update existing
\`\`\`

### Deleting Entries

\`\`\`perl
delete $ages{bob};
\`\`\`

### Checking Existence

\`\`\`perl
if (exists $ages{carol}) {
    say "found carol";
}
\`\`\`

### Your Task

Create a hash with three people and their ages, then print each person's name and age.`,

  starterCode: `my %ages = (
    alice => 30,
    bob => 25,
    carol => 35,
);
say $ages{alice};
say $ages{bob};
say $ages{carol};
`,

  solution: `my %ages = (
    alice => 30,
    bob => 25,
    carol => 35,
);
say $ages{alice};
say $ages{bob};
say $ages{carol};
`,

  tests: [
    {
      name: "prints three ages",
      expected: "30\n25\n35\n",
    },
    {
      name: "add and read key",
      expected: "28\n",
      code: `my %h = (a => 1);
$h{b} = 28;
say $h{b};`,
    },
    {
      name: "exists check",
      expected: "yes\n",
      code: `my %h = (x => 10);
if (exists $h{x}) {
    say "yes";
}`,
    },
  ],
};
