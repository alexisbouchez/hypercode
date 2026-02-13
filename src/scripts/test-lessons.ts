import { runGoTests } from "./test-runners/go-test-runner";
import { runZigTests } from "./test-runners/zig-test-runner";
import { runSqlTests } from "./test-runners/sql-test-runner";
import { runArm64Tests } from "./test-runners/arm64-test-runner";
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
