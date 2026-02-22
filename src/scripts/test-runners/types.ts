export interface LessonTestResult {
  course: "go" | "zig" | "sql" | "sqlite" | "redis" | "arm64" | "c" | "gleam" | "r" | "holyc" | "linux" | "coreutils" | "javascript" | "typescript" | "ruby" | "trees" | "kernel" | "linked-lists" | "haskell" | "lean" | "linear-algebra" | "statistics" | "calculus" | "calculus2" | "calculus3" | "circuits" | "music" | "cpp" | "raytracer" | "quantum" | "classical-mechanics" | "genomics" | "microgpt" | "waves" | "electromagnetism" | "advanced-linear-algebra" | "advanced-quantum" | "thermodynamics" | "special-relativity" | "fluid-mechanics" | "general-relativity" | "optics" | "number-theory" | "cryptography" | "nuclear-physics" | "particle-physics" | "machine-learning" | "signal-processing" | "information-theory" | "cosmology" | "astrophysics" | "plasma-physics" | "condensed-matter" | "mysql" | "rust" | "functional-diff-geo" | "diffeq" | "mathematical-physics" | "biophysics" | "complex-systems" | "chaos-theory" | "financial-math" | "quant-stats" | "portfolio-theory" | "options-pricing" | "time-series" | "algo-trading" | "risk-management" | "digital-logic" | "pcb-design" | "neural-networks" | "probability" | "discrete-math" | "html" | "css" | "tailwind" | "csharp" | "java";
  lessonId: string;
  lessonTitle: string;
  testName: string;
  passed: boolean;
  actual: string;
  expected: string;
}
