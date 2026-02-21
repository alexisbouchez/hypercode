export interface LessonTestResult {
  course: "go" | "zig" | "sql" | "sqlite" | "redis" | "arm64" | "c" | "gleam" | "r" | "holyc" | "linux" | "coreutils" | "javascript" | "typescript" | "ruby" | "trees" | "kernel" | "linked-lists" | "haskell" | "linear-algebra" | "statistics" | "calculus" | "calculus2" | "calculus3" | "circuits" | "music" | "cpp" | "quantum" | "classical-mechanics" | "genomics" | "microgpt" | "waves" | "electromagnetism" | "advanced-linear-algebra" | "advanced-quantum" | "thermodynamics" | "special-relativity" | "fluid-mechanics" | "general-relativity" | "optics" | "number-theory";
  lessonId: string;
  lessonTitle: string;
  testName: string;
  passed: boolean;
  actual: string;
  expected: string;
}
