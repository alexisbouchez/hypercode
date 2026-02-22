"use client";

import type { Lesson, Chapter, RunResult, Test, TestResult } from "@/lib/lessons/types";
import { initGoRunner, isGoReady, runGo, runTests as runGoTests } from "@/lib/go-runner";
import { initZigRunner, isZigReady, runZig, runTests as runZigTests } from "@/lib/zig-runner";
import { initSqlRunner, isSqlReady, runSql, runTests as runSqlTests } from "@/lib/sql-runner";
import { initArm64Runner, isArm64Ready, runArm64, runTests as runArm64Tests } from "@/lib/arm64-runner";
import { initCRunner, isCReady, runC, runTests as runCTests } from "@/lib/c-runner";
import { initGleamRunner, isGleamReady, runGleam, runTests as runGleamTests } from "@/lib/gleam-runner";
import { initRRunner, isRReady, runR, runTests as runRTests } from "@/lib/r-runner";
import { initHolycRunner, isHolycReady, runHolyC, runTests as runHolycTests } from "@/lib/holyc-runner";
import { initLinuxRunner, isLinuxReady, runLinux, runTests as runLinuxTests } from "@/lib/linux-runner";
import { initJsRunner, isJsReady, runJs, runTests as runJsTests } from "@/lib/js-runner";
import { initTsRunner, isTsReady, runTs, runTests as runTsTests } from "@/lib/ts-runner";
import { initRubyRunner, isRubyReady, runRuby, runTests as runRubyTests } from "@/lib/ruby-runner";
import { initThreeJsRunner, isThreeJsReady, runThreeJs, runTests as runThreeJsTests } from "@/lib/threejs-runner";
import { initPythonRunner, isPythonReady, runPython, runTests as runPythonTests } from "@/lib/python-runner";
import { initHaskellRunner, isHaskellReady, runHaskell, runTests as runHaskellTests } from "@/lib/haskell-runner";
import { initSqliteRunner, isSqliteReady, runSqlite, runTests as runSqliteTests } from "@/lib/sqlite-runner";
import { initRedisRunner, isRedisReady, runRedis, runTests as runRedisTests } from "@/lib/redis-runner";
import { initCppRunner, isCppReady, runCpp, runTests as runCppTests } from "@/lib/cpp-runner";
import { initMusicRunner, isMusicReady, runMusic, runTests as runMusicTests } from "@/lib/music-runner";
import { initMysqlRunner, isMysqlReady, runMysql, runTests as runMysqlTests } from "@/lib/mysql-runner";
import { initRustRunner, isRustReady, runRust, runTests as runRustTests } from "@/lib/rust-runner";
import { initLeanRunner, isLeanReady, runLean, runTests as runLeanTests } from "@/lib/lean-runner";
import { initCSharpRunner, isCSharpReady, runCSharp, runTests as runCSharpTests } from "@/lib/csharp-runner";
import { initHtmlRunner, isHtmlReady, runHtml, runTests as runHtmlTests } from "@/lib/html-runner";
import { initTailwindRunner, isTailwindReady, runTailwind, runTests as runTailwindTests } from "@/lib/tailwind-runner";
import { initJavaRunner, isJavaReady, runJava, runTests as runJavaTests } from "@/lib/java-runner";
import { initKotlinRunner, isKotlinReady, runKotlin, runTests as runKotlinTests } from "@/lib/kotlin-runner";
import { initSwiftRunner, isSwiftReady, runSwift, runTests as runSwiftTests } from "@/lib/swift-runner";
import { initElixirRunner, isElixirReady, runElixir, runTests as runElixirTests } from "@/lib/elixir-runner";
import { initScalaRunner, isScalaReady, runScala, runTests as runScalaTests } from "@/lib/scala-runner";
import { initFSharpRunner, isFSharpReady, runFSharp, runTests as runFSharpTests } from "@/lib/fsharp-runner";
import { LessonShell } from "./lesson-shell";

interface LessonShellWrapperProps {
  courseId: string;
  language: string;
  runtimeLabel: string;
  pdfPath?: string;
  lesson: Lesson;
  lessons: Lesson[];
  chapters: Chapter[];
}

interface RunnerModule {
  init: () => Promise<void>;
  isReady: () => boolean;
  run: (code: string) => Promise<RunResult>;
  runTests: (code: string, tests: Test[]) => Promise<TestResult[]>;
}

const c: RunnerModule = { init: initCRunner, isReady: isCReady, run: runC, runTests: runCTests };
const js: RunnerModule = { init: initJsRunner, isReady: isJsReady, run: runJs, runTests: runJsTests };
const python: RunnerModule = { init: initPythonRunner, isReady: isPythonReady, run: runPython, runTests: runPythonTests };
const music: RunnerModule = { init: initMusicRunner, isReady: isMusicReady, run: runMusic, runTests: runMusicTests };
const cpp: RunnerModule = { init: initCppRunner, isReady: isCppReady, run: runCpp, runTests: runCppTests };

const RUNNER_MAP: Record<string, RunnerModule> = {
  postgresql:              { init: initSqlRunner,    isReady: isSqlReady,    run: runSql,     runTests: runSqlTests },
  zig:                     { init: initZigRunner,    isReady: isZigReady,    run: runZig,     runTests: runZigTests },
  arm64:                   { init: initArm64Runner,  isReady: isArm64Ready,  run: runArm64,   runTests: runArm64Tests },
  c,
  gleam:                   { init: initGleamRunner,  isReady: isGleamReady,  run: runGleam,   runTests: runGleamTests },
  r:                       { init: initRRunner,      isReady: isRReady,      run: runR,       runTests: runRTests },
  holyc:                   { init: initHolycRunner,  isReady: isHolycReady,  run: runHolyC,   runTests: runHolycTests },
  coreutils:               c,
  trees:                   c,
  kernel:                  c,
  "linked-lists":          c,
  calculus:                c,
  calculus2:               c,
  calculus3:               c,
  circuits:                c,
  "classical-mechanics":   c,
  linux:                   { init: initLinuxRunner,  isReady: isLinuxReady,  run: runLinux,   runTests: runLinuxTests },
  javascript:              js,
  typescript:              { init: initTsRunner,     isReady: isTsReady,     run: runTs,      runTests: runTsTests },
  ruby:                    { init: initRubyRunner,   isReady: isRubyReady,   run: runRuby,    runTests: runRubyTests },
  algorithms:              js,
  "distributed-systems":   js,
  "digital-logic":         js,
  "pcb-design":            js,
  threejs:                 { init: initThreeJsRunner, isReady: isThreeJsReady, run: runThreeJs, runTests: runThreeJsTests },
  python,
  graphs:                  python,
  "linear-algebra":        python,
  statistics:              python,
  probability:             python,
  "discrete-math":         python,
  diffeq:                  python,
  quantum:                 python,
  genomics:                python,
  microgpt:                python,
  electromagnetism:        python,
  "advanced-linear-algebra": python,
  "advanced-quantum":      python,
  thermodynamics:          python,
  "special-relativity":    python,
  "fluid-mechanics":       python,
  "general-relativity":    python,
  "nuclear-physics":       python,
  "particle-physics":      python,
  "number-theory":         python,
  optics:                  python,
  cryptography:            python,
  cosmology:               python,
  astrophysics:            python,
  "plasma-physics":        python,
  "condensed-matter":      python,
  biophysics:              python,
  "mathematical-physics":  python,
  "complex-systems":       python,
  "chaos-theory":          python,
  "signal-processing":     python,
  "machine-learning":      python,
  "neural-networks":       python,
  "information-theory":    python,
  haskell:                 { init: initHaskellRunner, isReady: isHaskellReady, run: runHaskell, runTests: runHaskellTests },
  sqlite:                  { init: initSqliteRunner,  isReady: isSqliteReady,  run: runSqlite,  runTests: runSqliteTests },
  redis:                   { init: initRedisRunner,   isReady: isRedisReady,   run: runRedis,   runTests: runRedisTests },
  cpp,
  raytracer:               cpp,
  music,
  waves:                   music,
  mysql:                   { init: initMysqlRunner,   isReady: isMysqlReady,   run: runMysql,   runTests: runMysqlTests },
  rust:                    { init: initRustRunner,    isReady: isRustReady,    run: runRust,    runTests: runRustTests },
  lean:                    { init: initLeanRunner,    isReady: isLeanReady,    run: runLean,    runTests: runLeanTests },
  html:                    { init: initHtmlRunner,    isReady: isHtmlReady,    run: runHtml,    runTests: runHtmlTests },
  css:                     { init: initHtmlRunner,    isReady: isHtmlReady,    run: runHtml,    runTests: runHtmlTests },
  tailwind:                { init: initTailwindRunner, isReady: isTailwindReady, run: runTailwind, runTests: runTailwindTests },
  csharp:                  { init: initCSharpRunner,   isReady: isCSharpReady,   run: runCSharp,   runTests: runCSharpTests },
  java:                    { init: initJavaRunner,     isReady: isJavaReady,     run: runJava,     runTests: runJavaTests },
  kotlin:                  { init: initKotlinRunner,   isReady: isKotlinReady,   run: runKotlin,   runTests: runKotlinTests },
  swift:                   { init: initSwiftRunner,    isReady: isSwiftReady,    run: runSwift,    runTests: runSwiftTests },
  elixir:                  { init: initElixirRunner,   isReady: isElixirReady,   run: runElixir,   runTests: runElixirTests },
  scala:                   { init: initScalaRunner,    isReady: isScalaReady,    run: runScala,    runTests: runScalaTests },
  fsharp:                  { init: initFSharpRunner,   isReady: isFSharpReady,   run: runFSharp,   runTests: runFSharpTests },
};

const GO_RUNNER: RunnerModule = {
  init: initGoRunner,
  isReady: isGoReady,
  run: runGo,
  runTests: runGoTests,
};

export function LessonShellWrapper({
  courseId,
  language,
  runtimeLabel,
  pdfPath,
  lesson,
  lessons,
  chapters,
}: LessonShellWrapperProps) {
  const runner = RUNNER_MAP[courseId] ?? GO_RUNNER;

  return (
    <LessonShell
      courseId={courseId}
      language={language}
      runtimeLabel={runtimeLabel}
      pdfPath={pdfPath}
      lesson={lesson}
      lessons={lessons}
      chapters={chapters}
      initRunner={runner.init}
      isRunnerReady={runner.isReady}
      runCode={runner.run}
      runTests={runner.runTests}
    />
  );
}
