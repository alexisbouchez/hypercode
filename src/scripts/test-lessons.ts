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
import { runSqliteTests } from "./test-runners/sqlite-test-runner";
import { runRedisTests } from "./test-runners/redis-test-runner";
import { runCppTests } from "./test-runners/cpp-test-runner";
import { runRaytracerTests } from "./test-runners/raytracer-test-runner";
import { runQuantumTests } from "./test-runners/quantum-test-runner";
import { runCalculusTests } from "./test-runners/calculus-test-runner";
import { runCalculus2Tests } from "./test-runners/calculus2-test-runner";
import { runCalculus3Tests } from "./test-runners/calculus3-test-runner";
import { runGenomicsTests } from "./test-runners/genomics-test-runner";
import { runMicrogptTests } from "./test-runners/microgpt-test-runner";
import { runAdvancedLinearAlgebraTests } from "./test-runners/advanced-linear-algebra-test-runner";
import { runAdvancedQuantumTests } from "./test-runners/advanced-quantum-test-runner";
import { runThermodynamicsTests } from "./test-runners/thermodynamics-test-runner";
import { runSpecialRelativityTests } from "./test-runners/special-relativity-test-runner";
import { runFluidMechanicsTests } from "./test-runners/fluid-mechanics-test-runner";
import { runOpticsTests } from "./test-runners/optics-test-runner";
import { runGeneralRelativityTests } from "./test-runners/general-relativity-test-runner";
import { runNuclearPhysicsTests } from "./test-runners/nuclear-physics-test-runner";
import { runParticlePhysicsTests } from "./test-runners/particle-physics-test-runner";
import { runNumberTheoryTests } from "./test-runners/number-theory-test-runner";
import { runCryptographyTests } from "./test-runners/cryptography-test-runner";
import { runCosmologyTests } from "./test-runners/cosmology-test-runner";
import { runAstrophysicsTests } from "./test-runners/astrophysics-test-runner";
import { runPlasmaPhysicsTests } from "./test-runners/plasma-physics-test-runner";
import { runCondensedMatterTests } from "./test-runners/condensed-matter-test-runner";
import { runBiophysicsTests } from "./test-runners/biophysics-test-runner";
import { runSignalProcessingTests } from "./test-runners/signal-processing-test-runner";
import { runMachineLearningTests } from "./test-runners/machine-learning-test-runner";
import { runInformationTheoryTests } from "./test-runners/information-theory-test-runner";
import { runMysqlTests } from "./test-runners/mysql-test-runner";
import { runCircuitsTests } from "./test-runners/circuits-test-runner";
import { runClassicalMechanicsTests } from "./test-runners/classical-mechanics-test-runner";
import { runDiffeqTests } from "./test-runners/diffeq-test-runner";
import { runMathematicalPhysicsTests } from "./test-runners/mathematical-physics-test-runner";
import { runComplexSystemsTests } from "./test-runners/complex-systems-test-runner";
import { runChaosTheoryTests } from "./test-runners/chaos-theory-test-runner";
import { runMusicTests } from "./test-runners/music-test-runner";
import { runWavesTests } from "./test-runners/waves-test-runner";
import { runRustTests } from "./test-runners/rust-test-runner";
import { runFunctionalDiffGeoTests } from "./test-runners/functional-diff-geo-test-runner";
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

  console.log("Running SQLite tests...");
  const sqliteResults = runSqliteTests();
  allResults.push(...sqliteResults);

  console.log("Running Redis tests...");
  const redisResults = runRedisTests();
  allResults.push(...redisResults);

  console.log("Running C++ tests...");
  const cppResults = runCppTests();
  allResults.push(...cppResults);

  console.log("Running Ray Tracer tests...");
  const raytracerResults = runRaytracerTests();
  allResults.push(...raytracerResults);

  console.log("Running Quantum Computing tests...");
  const quantumResults = runQuantumTests();
  allResults.push(...quantumResults);

  console.log("Running Calculus 1 tests...");
  const calculus1Results = await runCalculusTests();
  allResults.push(...calculus1Results);

  console.log("Running Calculus 2 tests...");
  const calculus2Results = await runCalculus2Tests();
  allResults.push(...calculus2Results);

  console.log("Running Calculus 3 tests...");
  const calculus3Results = await runCalculus3Tests();
  allResults.push(...calculus3Results);

  console.log("Running Genomics tests...");
  const genomicsResults = runGenomicsTests();
  allResults.push(...genomicsResults);

  console.log("Running MicroGPT tests...");
  const microgptResults = runMicrogptTests();
  allResults.push(...microgptResults);

  console.log("Running Advanced Linear Algebra tests...");
  const advancedLinearAlgebraResults = runAdvancedLinearAlgebraTests();
  allResults.push(...advancedLinearAlgebraResults);

  console.log("Running Advanced Quantum Computing tests...");
  const advancedQuantumResults = runAdvancedQuantumTests();
  allResults.push(...advancedQuantumResults);

  console.log("Running Thermodynamics tests...");
  const thermodynamicsResults = runThermodynamicsTests();
  allResults.push(...thermodynamicsResults);

  console.log("Running Special Relativity tests...");
  const specialRelativityResults = runSpecialRelativityTests();
  allResults.push(...specialRelativityResults);

  console.log("Running Fluid Mechanics tests...");
  const fluidMechanicsResults = runFluidMechanicsTests();
  allResults.push(...fluidMechanicsResults);

  console.log("Running Optics tests...");
  const opticsResults = runOpticsTests();
  allResults.push(...opticsResults);

  console.log("Running General Relativity tests...");
  const generalRelativityResults = runGeneralRelativityTests();
  allResults.push(...generalRelativityResults);

  console.log("Running Nuclear Physics tests...");
  const nuclearPhysicsResults = runNuclearPhysicsTests();
  allResults.push(...nuclearPhysicsResults);

  console.log("Running Particle Physics tests...");
  const particlePhysicsResults = runParticlePhysicsTests();
  allResults.push(...particlePhysicsResults);

  console.log("Running Number Theory tests...");
  const numberTheoryResults = runNumberTheoryTests();
  allResults.push(...numberTheoryResults);

  console.log("Running Cryptography tests...");
  const cryptographyResults = runCryptographyTests();
  allResults.push(...cryptographyResults);

  console.log("Running Cosmology tests...");
  const cosmologyResults = runCosmologyTests();
  allResults.push(...cosmologyResults);

  console.log("Running Astrophysics tests...");
  const astrophysicsResults = runAstrophysicsTests();
  allResults.push(...astrophysicsResults);

  console.log("Running Plasma Physics tests...");
  const plasmaPhysicsResults = runPlasmaPhysicsTests();
  allResults.push(...plasmaPhysicsResults);

  console.log("Running Condensed Matter tests...");
  const condensedMatterResults = runCondensedMatterTests();
  allResults.push(...condensedMatterResults);

  console.log("Running Biophysics tests...");
  const biophysicsResults = runBiophysicsTests();
  allResults.push(...biophysicsResults);

  console.log("Running Signal Processing tests...");
  const signalProcessingResults = runSignalProcessingTests();
  allResults.push(...signalProcessingResults);

  console.log("Running Machine Learning tests...");
  const machineLearningResults = runMachineLearningTests();
  allResults.push(...machineLearningResults);

  console.log("Running Information Theory tests...");
  const informationTheoryResults = runInformationTheoryTests();
  allResults.push(...informationTheoryResults);

  console.log("Running MySQL tests...");
  const mysqlResults = runMysqlTests();
  allResults.push(...mysqlResults);

  console.log("Running Circuits tests...");
  const circuitsResults = runCircuitsTests();
  allResults.push(...circuitsResults);

  console.log("Running Classical Mechanics tests...");
  const classicalMechanicsResults = runClassicalMechanicsTests();
  allResults.push(...classicalMechanicsResults);

  console.log("Running Differential Equations tests...");
  const diffeqResults = runDiffeqTests();
  allResults.push(...diffeqResults);

  console.log("Running Mathematical Physics tests...");
  const mathematicalPhysicsResults = runMathematicalPhysicsTests();
  allResults.push(...mathematicalPhysicsResults);

  console.log("Running Complex Systems tests...");
  const complexSystemsResults = runComplexSystemsTests();
  allResults.push(...complexSystemsResults);

  console.log("Running Chaos Theory tests...");
  const chaosTheoryResults = runChaosTheoryTests();
  allResults.push(...chaosTheoryResults);

  console.log("Running Music tests...");
  const musicResults = runMusicTests();
  allResults.push(...musicResults);

  console.log("Running Waves tests...");
  const wavesResults = runWavesTests();
  allResults.push(...wavesResults);

  console.log("Running Rust tests...");
  const rustResults = runRustTests();
  allResults.push(...rustResults);

  console.log("Running Functional Differential Geometry tests...");
  const functionalDiffGeoResults = runFunctionalDiffGeoTests();
  allResults.push(...functionalDiffGeoResults);

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
