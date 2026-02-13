export interface LessonTestResult {
  course: "go" | "zig" | "sql" | "arm64" | "c";
  lessonId: string;
  lessonTitle: string;
  testName: string;
  passed: boolean;
  actual: string;
  expected: string;
}
