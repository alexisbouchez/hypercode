export interface Chapter {
  id: string;
  title: string;
}

export interface Test {
  name: string;
  expected: string;
  /**
   * Language-agnostic source template for testing.
   * Use {{FUNC}} as a placeholder for the user's extracted function definitions.
   * If not set, the user's full code is run as-is.
   */
  code?: string;
}

export interface Lesson {
  id: string;
  title: string;
  chapterId: string;
  content: string;
  starterCode: string;
  solution: string;
  tests: Test[];
}

export interface BaseRunResult {
  stdout: string;
  stderr: string;
  error: string;
}

export type RunResult = BaseRunResult & { generatedCode?: string; previewHtml?: string };

export interface TestResult {
  name: string;
  passed: boolean;
  actual: string;
  expected: string;
}
