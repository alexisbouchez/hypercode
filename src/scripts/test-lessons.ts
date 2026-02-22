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
import { runFinancialMathTests } from "./test-runners/financial-math-test-runner";
import { runQuantStatsTests } from "./test-runners/quant-stats-test-runner";
import { runPortfolioTheoryTests } from "./test-runners/portfolio-theory-test-runner";
import { runOptionsPricingTests } from "./test-runners/options-pricing-test-runner";
import { runTimeSeriesTests } from "./test-runners/time-series-test-runner";
import { runAlgoTradingTests } from "./test-runners/algo-trading-test-runner";
import { runRiskManagementTests } from "./test-runners/risk-management-test-runner";
import { runLeanTests } from "./test-runners/lean-test-runner";
import { runDigitalLogicTests } from "./test-runners/digital-logic-test-runner";
import { runPcbDesignTests } from "./test-runners/pcb-design-test-runner";
import { runNeuralNetworksTests } from "./test-runners/neural-networks-test-runner";
import { runProbabilityTests } from "./test-runners/probability-test-runner";
import { runDiscreteMathTests } from "./test-runners/discrete-math-test-runner";
import { runHtmlTests } from "./test-runners/html-test-runner";
import { runCssTests } from "./test-runners/css-test-runner";
import { runTailwindTests } from "./test-runners/tailwind-test-runner";
import { runCSharpTests } from "./test-runners/csharp-test-runner";
import { runJavaTests } from "./test-runners/java-test-runner";
import { runKotlinTests } from "./test-runners/kotlin-test-runner";
import { runSwiftTests } from "./test-runners/swift-test-runner";
import { runElixirTests } from "./test-runners/elixir-test-runner";
import { runScalaTests } from "./test-runners/scala-test-runner";
import { runFSharpTests } from "./test-runners/fsharp-test-runner";
import type { LessonTestResult } from "./test-runners/types";

type TestRunner = () => LessonTestResult[] | Promise<LessonTestResult[]>;

const runners: Array<[string, TestRunner]> = [
  ["Go", runGoTests],
  ["Zig", runZigTests],
  ["SQL", runSqlTests],
  ["ARM64", runArm64Tests],
  ["C", runCTests],
  ["Gleam", runGleamTests],
  ["R", runRTests],
  ["HolyC", runHolycTests],
  ["Linux", runLinuxTests],
  ["Coreutils", runCoreutilsTests],
  ["JavaScript", runJsTests],
  ["TypeScript", runTsTests],
  ["Ruby", runRubyTests],
  ["Trees", runTreesTests],
  ["Kernel", runKernelTests],
  ["Linked Lists", runLinkedListsTests],
  ["Haskell", runHaskellTests],
  ["Lean", runLeanTests],
  ["Linear Algebra", runLinearAlgebraTests],
  ["Statistics", runStatisticsTests],
  ["SQLite", runSqliteTests],
  ["Redis", runRedisTests],
  ["C++", runCppTests],
  ["Ray Tracer", runRaytracerTests],
  ["Quantum Computing", runQuantumTests],
  ["Calculus 1", runCalculusTests],
  ["Calculus 2", runCalculus2Tests],
  ["Calculus 3", runCalculus3Tests],
  ["Genomics", runGenomicsTests],
  ["MicroGPT", runMicrogptTests],
  ["Advanced Linear Algebra", runAdvancedLinearAlgebraTests],
  ["Advanced Quantum Computing", runAdvancedQuantumTests],
  ["Thermodynamics", runThermodynamicsTests],
  ["Special Relativity", runSpecialRelativityTests],
  ["Fluid Mechanics", runFluidMechanicsTests],
  ["Optics", runOpticsTests],
  ["General Relativity", runGeneralRelativityTests],
  ["Nuclear Physics", runNuclearPhysicsTests],
  ["Particle Physics", runParticlePhysicsTests],
  ["Number Theory", runNumberTheoryTests],
  ["Cryptography", runCryptographyTests],
  ["Cosmology", runCosmologyTests],
  ["Astrophysics", runAstrophysicsTests],
  ["Plasma Physics", runPlasmaPhysicsTests],
  ["Condensed Matter", runCondensedMatterTests],
  ["Biophysics", runBiophysicsTests],
  ["Signal Processing", runSignalProcessingTests],
  ["Machine Learning", runMachineLearningTests],
  ["Information Theory", runInformationTheoryTests],
  ["MySQL", runMysqlTests],
  ["Circuits", runCircuitsTests],
  ["Classical Mechanics", runClassicalMechanicsTests],
  ["Differential Equations", runDiffeqTests],
  ["Mathematical Physics", runMathematicalPhysicsTests],
  ["Complex Systems", runComplexSystemsTests],
  ["Chaos Theory", runChaosTheoryTests],
  ["Music", runMusicTests],
  ["Waves", runWavesTests],
  ["Rust", runRustTests],
  ["Functional Differential Geometry", runFunctionalDiffGeoTests],
  ["Financial Mathematics", runFinancialMathTests],
  ["Quantitative Statistics", runQuantStatsTests],
  ["Time Series Analysis", runTimeSeriesTests],
  ["Portfolio Theory", runPortfolioTheoryTests],
  ["Options Pricing", runOptionsPricingTests],
  ["Algorithmic Trading", runAlgoTradingTests],
  ["Risk Management", runRiskManagementTests],
  ["Digital Logic", runDigitalLogicTests],
  ["PCB Design", runPcbDesignTests],
  ["Neural Networks from Scratch", runNeuralNetworksTests],
  ["Probability Theory", runProbabilityTests],
  ["Discrete Mathematics", runDiscreteMathTests],
  ["HTML", runHtmlTests],
  ["CSS", runCssTests],
  ["Tailwind CSS", runTailwindTests],
  ["C#", runCSharpTests],
  ["Java", runJavaTests],
  ["Kotlin", runKotlinTests],
  ["Swift", runSwiftTests],
  ["Elixir", runElixirTests],
  ["Scala", runScalaTests],
  ["F#", runFSharpTests],
];

async function main() {
  const allResults: LessonTestResult[] = [];

  for (const [name, fn] of runners) {
    console.log(`Running ${name} tests...`);
    allResults.push(...await fn());
  }

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
