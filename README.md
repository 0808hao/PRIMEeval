# PRIMEeval 0.8.0.9001

A standalone pure-R implementation of budgeted sequential auditing of bounded
paired model losses. The default is the tested WIDTH / OFFSET / LEGACY / SUPPORT
recipe, with two pilot queries per task and half the total cost reserved for
confirmation. This is a source release candidate for package-level validation.

## Install in R

Use R >= 4.3.0. Place the downloaded `PRIMEeval.tar.gz` archive in your current
working directory (shown by `getwd()`), then run:

```r
if (!requireNamespace("digest", quietly = TRUE)) {
  install.packages("digest", repos = "https://cloud.r-project.org")
}
install.packages("PRIMEeval.tar.gz", repos = NULL, type = "source")
library(PRIMEeval)
example <- prime_example()
state <- prime_start(example$tasks, budget = 8)
state <- prime_run(state, example$query)
prime_summary(state)
```

The archive is a normal R source package; install it directly. Dependencies are
`digest` (plus R-supplied `stats` and `utils`); there is no compiled code or model-training dependency.
All package operations run in R. An installed copy needs no development scripts,
cache directories, data downloads or access to the author's server.

## What the package audits

For each task, unit values are paired loss differences (candidate minus reference)
in a fixed finite population. A unit may represent a record or a prespecified
cluster of records. Its public lower/upper support bounds must be valid for that
same unit value. The target is the unweighted mean across units; the record count
is a **cost**, not an estimand weight. The risk threshold defaults to 0.01.

`budget = 8` is a shared dataset-specific cost cap equal to the sum, across tasks,
of `mean unit cost * min(number of units, 8)`. It is not an eight-query cap for each
task. Queries stay within each task's saved maximum horizon (default 32), with
optional prohibition of its final unit. Actual quoted costs are always charged.

## Historical information and reproducibility

The historical predictor and per-step calibration cuts are part of the method.
Use `prime_history(fit = frozen_fit, cuts = frozen_cuts, ...)` to reuse exactly
those used in earlier experiments. Supplying fresh `training` and `calibration`
fits the historical variance model and calibrates the screening rule; it does
not fit the original candidate/reference prediction models. Historical task
families must be separate from targets. Without history, the package exposes an
explicit uncalibrated public-prior mode. The small included example exercises the
API in that mode and is not a reproduction of the reported 342/357 experiment.

## Labels arrive through a query interface

`prime_task()` receives public quantities only. Keep target labels outside tasks.
`prime_next()` returns a state and one pending request. Obtain that requested
unit's loss difference and pass it to `prime_update()`. `prime_run()` executes
this loop through a callback. Saving/restoring the whole state with
`saveRDS()` / `readRDS()` preserves continuation, including a pending request.

## Interpretation

A support certificate is deterministic conditional on valid public bounds and
the declared finite population. An unconfirmed task has not been certified as
safe or null. Current eBH, current e-value and single-task running-maximum flags
are development diagnostics reported separately; global cross-task adaptive
stopping and post-selection guarantees have not been established. Do not replace
current e-values with running maxima in eBH. Task-family separation is not a
patient-level or raw-record holdout.

The augmented historical-value allocator is experimental: in step 88 it improved
several sparse Sleep-EDF constructions but could degrade ordinary and transferred
history settings. It is an explicit option, not an automatic best-of-methods rule.

The detailed Chinese guide is installed as `doc/README_zh.md`. Exact numerical
kernels are retained as internal functions for reproducibility. The old 0.6.0
perturbation/retraining interfaces are outside the scope of this release.


## Deterministic certificate schedulers

Use `scheduler = "affine_full"` for the Step 93 current-screen affine
continuation, `"bound_greedy"` for the matched allocation control, or
`"guarded_affine"` for the Step 94 early reversal policy. The guarded
strategy requires a public family label for every task in a named character
vector supplied as `task_families`. See `?prime_start` and the installed
Chinese guide for an example and gate diagnostics. These schedulers support
resumable streaming requests and the full finite horizon by default. They
report support certificates separately from the legacy statistical readouts.
