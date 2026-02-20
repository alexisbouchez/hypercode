import { runGoTests } from "./test-runners/go-test-runner";
import { runZigTests } from "./test-runners/zig-test-runner";
import { runSqlTests } from "./test-runners/sql-test-runner";
import { runArm64Tests } from "./test-runners/arm64-test-runner";
import { runCTests } from "./test-runners/c-test-runner";
import { runGleamTests } from "./test-runners/gleam-test-runner";
import { runRTests } from "./test-runners/r-test-runner";
import { runHolycTests } from "./test-runners/holyc-test-runner";
import { runLinuxTests } from "./test-runners/linux-test-runner";
import { runCoreutilsTests } from "./test-runners/coreutils-test-runner";
import { runJsTests } from "./test-runners/js-test-runner";
import { runTsTests } from "./test-runners/ts-test-runner";
import { runRubyTests } from "./test-runners/ruby-test-runner";
import { runTreesTests } from "./test-runners/trees-test-runner";
import { runKernelTests } from "./test-runners/kernel-test-runner";
import { runLinkedListsTests } from "./test-runners/linked-lists-test-runner";
import { runHaskellTests } from "./test-runners/haskell-test-runner";
import { runLinearAlgebraTests } from "./test-runners/linear-algebra-test-runner";
import { runStatisticsTests } from "./test-runners/statistics-test-runner";
import type { LessonTestResult } from "./test-runners/types";

async function main() {
  const allResults: LessonTestResult[] = [];

  console.log("Running Go tests...");
  const goResults = runGoTests();
  allResults.push(...goResults);

  console.log("Running Zig tests...");
  const zigResults = runZigTests();
  allResults.push(...zigResults);

  console.log("Running SQL tests...");
  const sqlResults = await runSqlTests();
  allResults.push(...sqlResults);

  console.log("Running ARM64 tests...");
  const arm64Results = runArm64Tests();
  allResults.push(...arm64Results);

  console.log("Running C tests...");
  const cResults = await runCTests();
  allResults.push(...cResults);

  console.log("Running Gleam tests...");
  const gleamResults = runGleamTests();
  allResults.push(...gleamResults);

  console.log("Running R tests...");
  const rResults = runRTests();
  allResults.push(...rResults);

  console.log("Running HolyC tests...");
  const holycResults = runHolycTests();
  allResults.push(...holycResults);

  console.log("Running Linux tests...");
  const linuxResults = runLinuxTests();
  allResults.push(...linuxResults);

  console.log("Running Coreutils tests...");
  const coreutilsResults = await runCoreutilsTests();
  allResults.push(...coreutilsResults);

  console.log("Running JavaScript tests...");
  const jsResults = runJsTests();
  allResults.push(...jsResults);

  console.log("Running TypeScript tests...");
  const tsResults = runTsTests();
  allResults.push(...tsResults);

  console.log("Running Ruby tests...");
  const rubyResults = runRubyTests();
  allResults.push(...rubyResults);

  console.log("Running Trees tests...");
  const treesResults = await runTreesTests();
  allResults.push(...treesResults);

  console.log("Running Kernel tests...");
  const kernelResults = await runKernelTests();
  allResults.push(...kernelResults);

  console.log("Running Linked Lists tests...");
  const linkedListsResults = await runLinkedListsTests();
  allResults.push(...linkedListsResults);

  console.log("Running Haskell tests...");
  const haskellResults = runHaskellTests();
  allResults.push(...haskellResults);

  console.log("Running Linear Algebra tests...");
  const linearAlgebraResults = runLinearAlgebraTests();
  allResults.push(...linearAlgebraResults);

  console.log("Running Statistics tests...");
  const statisticsResults = runStatisticsTests();
  allResults.push(...statisticsResults);

  console.log("\n--- Results ---\n");

  const passed = allResults.filter((r) => r.passed);
  const failed = allResults.filter((r) => !r.passed);

  for (const r of allResults) {
    const icon = r.passed ? "\u2713" : "\u2717";
    console.log(`  ${icon} [${r.course}] ${r.lessonTitle} - ${r.testName}`);
  }

  if (failed.length > 0) {
    console.log("\n--- Failures ---\n");
    for (const r of failed) {
      console.log(`  [${r.course}] ${r.lessonTitle} - ${r.testName}`);
      console.log(`    expected: ${JSON.stringify(r.expected)}`);
      console.log(`    actual:   ${JSON.stringify(r.actual)}`);
      console.log();
    }
  }

  console.log(`\n${passed.length}/${allResults.length} tests passed`);

  if (failed.length > 0) {
    process.exit(1);
  }
}

main();
