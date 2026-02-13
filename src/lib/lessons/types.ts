export interface Chapter {
  id: string;
  title: string;
}

export interface Test {
  name: string;
  expected: string;
  /**
   * Complete Go source template for testing functions.
   * Use {{FUNC}} as placeholder for the user's function definitions.
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

export interface GoResult {
  stdout: string;
  stderr: string;
  error: string;
}

export type RunResult = GoResult & { generatedCode?: string };

export interface TestResult {
  name: string;
  passed: boolean;
  actual: string;
  expected: string;
}
