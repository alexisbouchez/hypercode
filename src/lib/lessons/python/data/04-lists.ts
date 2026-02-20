import type { Lesson } from "../../types";

export const lists: Lesson = {
	id: "lists",
	title: "Lists",
	chapterId: "foundations",
	content: `## Lists

A Python list is an ordered, mutable sequence of any type. Lists are created with square brackets.

### Creating and Accessing

\`\`\`python
nums = [3, 1, 4, 1, 5]
nums[0]   # 3
nums[-1]  # 5
nums[1:3] # [1, 4]
\`\`\`

### Mutating Lists

\`\`\`python
nums.append(9)       # add to end
nums.insert(0, 0)    # insert at index
nums.pop()           # remove last
nums.pop(0)          # remove at index
nums.remove(1)       # remove first occurrence of value
nums.sort()          # sort in-place
nums.reverse()       # reverse in-place
\`\`\`

### Useful Built-ins

\`\`\`python
len([1, 2, 3])       # 3
sum([1, 2, 3])       # 6
min([3, 1, 2])       # 1
max([3, 1, 2])       # 3
sorted([3, 1, 2])    # [1, 2, 3]  (returns new list)
list(range(5))       # [0, 1, 2, 3, 4]
\`\`\`

### Checking Membership

\`\`\`python
3 in [1, 2, 3]   # True
4 in [1, 2, 3]   # False
\`\`\`

### Your Task

Implement \`two_sum(nums, target)\` that:
- Returns \`True\` if any two distinct elements in \`nums\` sum to \`target\`
- Returns \`False\` otherwise

Hint: use a set to track which numbers you've seen.`,

	starterCode: `def two_sum(nums, target):
    # Return True if any two distinct elements sum to target
    pass

print(two_sum([2, 7, 11, 15], 9))
print(two_sum([1, 2, 3], 10))
print(two_sum([3, 3], 6))
`,

	solution: `def two_sum(nums, target):
    seen = set()
    for n in nums:
        if target - n in seen:
            return True
        seen.add(n)
    return False

print(two_sum([2, 7, 11, 15], 9))
print(two_sum([1, 2, 3], 10))
print(two_sum([3, 3], 6))
`,

	tests: [
		{
			name: "[2,7,11,15] target=9 → True",
			code: `{{FUNC}}
print(two_sum([2, 7, 11, 15], 9))`,
			expected: "True\n",
		},
		{
			name: "[1,2,3] target=10 → False",
			code: `{{FUNC}}
print(two_sum([1, 2, 3], 10))`,
			expected: "False\n",
		},
		{
			name: "[3,3] target=6 → True",
			code: `{{FUNC}}
print(two_sum([3, 3], 6))`,
			expected: "True\n",
		},
		{
			name: "[] target=0 → False",
			code: `{{FUNC}}
print(two_sum([], 0))`,
			expected: "False\n",
		},
	],
};
