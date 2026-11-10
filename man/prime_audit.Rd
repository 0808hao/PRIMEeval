\name{prime_audit}
\alias{prime_task}
\alias{prime_history}
\alias{prime_start}
\alias{prime_next}
\alias{prime_update}
\alias{prime_run}
\alias{prime_summary}
\alias{prime_trace}
\alias{prime_log}
\alias{prime_gate}
\title{Budgeted Sequential Auditing of Bounded Paired Losses}
\description{Construct public task inputs and explicitly calibrated history, then audit one requested label at a time using legacy SUPPORT allocation or deterministic certificate schedulers.}
\usage{
prime_task(public_units, task_id, dataset = "default", history = NULL)
prime_history(fit = NULL, cuts = NULL, training = NULL, calibration = NULL,
  threshold = .01, seed = 820081L, max_queries = 32L,
  fold_id = "PRIME_HISTORY", training_ids = character(),
  calibration_ids = character())
prime_start(tasks, budget = 8, cost_budget = NULL, threshold = .01,
  alpha = .025, fdr_alpha = .05, seed = 820081L, max_queries = 32L,
  allow_full_census = TRUE,
  scheduler = c("support", "history_value", "affine_full",
    "guarded_affine", "bound_greedy"),
  value_models = NULL, task_families = NULL)
prime_next(state)
prime_update(state, request, value)
prime_run(state, query, max_steps = Inf)
prime_summary(state)
prime_trace(state, task_id)
prime_log(state)
prime_gate(state)
}
\arguments{
\item{public_units}{Nonempty data frame with numeric \code{support_lower}, \code{support_upper} and \code{proxy_generic}. Optional \code{unit_id} is unique; \code{n_audit_records} is a finite cost at least one. Optional public proxies are described in the installed guide. Hidden target labels or scenario/truth columns must not be included.}
\item{task_id}{Unique nonempty task identifier. To reproduce existing trajectories, preserve the exact identifier.}
\item{dataset}{Dataset identifier. Each dataset has its own total cost budget.}
\item{history}{A \code{prime_history} object, or \code{NULL} for an explicitly uncalibrated public-prior mode.}
\item{fit}{Frozen historical fit containing \code{variance} and \code{prior}, or \code{NULL} when fitting fresh history.}
\item{cuts}{Data frame with unique \code{step} and nonnegative \code{cut} values through \code{max_queries}. A full historical calibration table is filtered to PLUGIN_IMPUTE/ERROR_Q. Positive infinity represents unavailable screening calibration.}
\item{training,calibration}{Separate lists of historical \code{list(task = prime_task(...), values = numeric_vector)} objects, with disjoint task families. These fit a variance model and screening cuts, not the original prediction models.}
\item{threshold}{Risk threshold for the unweighted finite-population mean of candidate-minus-reference unit loss differences.}
\item{seed}{Positive integer seed. The default preserves the development experiment's deterministic query streams and tie breaking. Caller RNG state is preserved.}
\item{max_queries}{Maximum number of labels per task, before the optional final-unit exclusion. When omitted, deterministic certificate schedulers use the full finite horizon. Those schedulers also accept an explicit positive integer cap or \code{Inf}.}
\item{fold_id}{Identifier for fresh historical fitting. It affects the shared feature projection and must be preserved for exact replay.}
\item{training_ids,calibration_ids}{Known historical task IDs for an imported fit. They allow task-family exclusion checks. Empty metadata cannot certify separation.}
\item{tasks}{List of public \code{prime_task} objects with globally unique task IDs.}
\item{budget}{Positive integer defining the shared dataset cost cap as the sum of mean unit cost times \code{min(n_units, budget)}. It is not a per-task query cap. Must not exceed \code{max_queries} for the legacy schedulers.}
\item{cost_budget}{Optional explicit nonnegative total cost cap, named by dataset when there are multiple datasets.}
\item{alpha}{Single-task development e-value threshold level.}
\item{fdr_alpha}{Level for the current-e-value development eBH readout, separately within each dataset. This is not a claim of global cross-task stopping validity.}
\item{allow_full_census}{Whether a task's last unit may be queried.}
\item{scheduler}{Legacy default \code{"support"}, experimental legacy \code{"history_value"}, or the deterministic schedulers \code{"affine_full"}, \code{"guarded_affine"}, and \code{"bound_greedy"}.}
\item{value_models}{For the experimental scheduler, a list named by target task ID of frozen step-88 model parameters. Required fields are \code{feature_names}, \code{cuts}, \code{lookup}, \code{intercept}, \code{range_min}, \code{range_max}, \code{usable}. Historical-ID metadata, when provided, is checked. An unusable model causes dataset-wide SUPPORT fallback.}
\item{task_families}{For \code{"guarded_affine"}, a character vector named by every task ID. Values specify public family blocks. Tasks from the same family within a dataset share a block.}
\item{state}{Audit state returned by \code{prime_start}, \code{prime_update}, \code{prime_run}, or the \code{state} component of \code{prime_next}. It can be saved using \code{saveRDS}.}
\item{request}{The exact unmodified pending request returned by \code{prime_next}.}
\item{value}{One finite numeric paired loss difference for the requested unit, within its declared support.}
\item{query}{Function \code{function(task_id, unit_id, dataset)} returning only the requested unit value. Keep its hidden target labels outside public tasks and the audit state.}
\item{max_steps}{Maximum additional query/update steps in this call, or infinity.}
}
\value{
\code{prime_task} and \code{prime_history} construct input objects.
\code{prime_start}, \code{prime_update} and \code{prime_run} return a serializable audit state.
\code{prime_next} returns a list containing an updated state and one request data frame, or a NULL request when no payable query remains.
\code{prime_summary} returns one row per task with used labels, actual cost, support bounds, confirmation flags (including bound_certificate, valid even before a query), historical scope, effective scheduler and development readouts.
\code{prime_trace} returns the queried within-task trace; \code{prime_log} returns the completed query log. \code{prime_gate} returns one diagnostic row per dataset for \code{"guarded_affine"}, or an empty data frame for the other schedulers.
}
\details{
The default recipe samples units proportionally to public support width divided by square-root cost, mixed with a 0.15 uniform floor. Each task receives up to two pilot queries. Half the dataset cost cap is reserved for SUPPORT confirmation allocation. Budgets are kept separate across datasets; task limits and actual quoted costs are enforced. The allocator may continue spending residual budget after some tasks are confirmed to preserve the development cost comparison convention.

The deterministic schedulers query units in descending public support-width to cost ratio, breaking ties by input row order. Each dataset uses a charged pilot of up to two queries per eligible task. After the pilot, affine continuation prioritizes unresolved tasks using the current affine screen and support bounds. The boundary-greedy control uses the same unit order and support bounds, with task priority based on distance to the threshold and cost.
 
The guarded policy evaluates one dataset-level rule before the first non-pilot request. It requires at least 16 family blocks and two observed units for at least 80 percent of initially eligible tasks. If Spearman correlation of the family means of public proxies and pilot observations is at most -0.20, allocation switches to boundary greedy for the rest of that dataset. Missing coverage or constant ranks retain affine allocation. Gate diagnostics include coverage, reason, observed correlation, and charged pilot cost.
 
Deterministic task summaries report \code{RISK_CERTIFIED}, \code{RISK_EXCLUDED_BY_BOUNDS}, or \code{INSUFFICIENT_BOUND_EVIDENCE}. These schedulers use public inputs and observed labels only. They require tasks without historical fits and do not compute e-values. The arguments \code{alpha} and \code{fdr_alpha} apply to the legacy statistical readouts.
 
The support interval is deterministic conditional on valid public bounds, the declared fixed finite population and correctly supplied observations. Its target is the equal-unit mean; record counts affect cost only. An unconfirmed task is not certified safe or null.

Columns named formal_current, formal_anytime and formal_ebh retain old names for replay compatibility but are development readouts. Current eBH uses current values, never within-task running maxima. Global cross-task adaptive stopping and post-selection error guarantees have not been established. Historical family separation is not a raw-record or patient holdout.

Exact historical-experiment performance requires matching frozen fits, step-specific cuts, IDs, ordering and settings. The no-history mode is usable for the API but is not a reproduction of calibrated development performance.
}
\examples{
ex <- prime_example()
s <- prime_start(ex$tasks, budget = 8)
s <- prime_run(s, ex$query)
prime_summary(s)[, c("task_id", "used", "record_cost", "support_only")]
}
\seealso{\code{\link{prime_example}}}
