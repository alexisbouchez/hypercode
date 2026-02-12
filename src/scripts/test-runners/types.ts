export interface LessonTestResult {
  course: "go" | "zig" | "sql";
  lessonId: string;
  lessonTitle: string;
  testName: string;
  passed: boolean;
  actual: string;
  expected: string;
}
