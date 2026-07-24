# Coverage target

The CI gate uses `covr::file_coverage()` over the production files in `R/` and
the historical headless/adversarial suites through
`tests/coverage_driver.R`. The target is **80% line coverage**. The current
local measurement is **82.6%** on R 4.4.2 with the pinned dependency set.

`test_pipeline.R` remains part of the normal 37-suite functional gate, but is
excluded from the covr driver because it deliberately re-sources every `R/`
file and relies on cross-process `<<-` fixture assignment; both behaviours
replace or bypass covr's instrumented environment. This is a measurement
boundary, not a functional-test exclusion.
