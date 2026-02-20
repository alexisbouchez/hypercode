export interface LessonTestResult {
  course: "go" | "zig" | "sql" | "arm64" | "c" | "gleam" | "r" | "holyc" | "linux" | "coreutils" | "javascript" | "typescript" | "ruby" | "trees" | "kernel" | "linked-lists" | "haskell" | "linear-algebra" | "statistics";
  lessonId: string;
  lessonTitle: string;
  testName: string;
  passed: boolean;
  actual: string;
  expected: string;
}
