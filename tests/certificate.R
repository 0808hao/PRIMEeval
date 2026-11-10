library(PRIMEeval)

failed <- function(expr) tryCatch({force(expr); FALSE}, error = function(e) TRUE)
task_ids <- sprintf("GATE_%02d", seq_len(20L))
proxy <- stats::setNames(seq(-.03, .05, length.out = 20L), task_ids)
families <- stats::setNames(task_ids, task_ids)
tasks <- lapply(task_ids, function(id) {
  units <- data.frame(unit_id = as.character(seq_len(8L)), support_lower = -.1,
                      support_upper = .1, proxy_generic = proxy[[id]])
  prime_task(units, id, "gate")
})
start <- function(method, budget = 4L, family = families) {
  prime_start(tasks, budget = budget, scheduler = method, task_families = family)
}
query <- function(task_id, unit_id, dataset) -proxy[[task_id]]
first <- prime_next(start("guarded_affine"))
request <- first$request
bad_request <- request
bad_request$unit_id <- "changed"
stopifnot(failed(prime_update(first$state, bad_request, 0)),
          failed(prime_update(first$state, request, 1)),
          identical(prime_next(first$state)$request, request))
restored <- unserialize(serialize(first$state, NULL))
guarded <- prime_run(restored, query)
direct <- prime_run(start("guarded_affine"), query)
stopifnot(identical(prime_log(guarded), prime_log(direct)),
          identical(prime_gate(guarded), prime_gate(direct)))
bound <- prime_run(start("bound_greedy"), query)
log <- prime_log(guarded)
gate <- prime_gate(guarded)
cols <- c("task_id", "unit_id", "cost", "phase", "is_pilot")
stopifnot(identical(log[cols], prime_log(bound)[cols]),
          gate$gate_mode == "BOUND", gate$gate_pilot_queries == 40L,
          gate$gate_pilot_cost == 40, sum(log$cost) <= 80,
          !anyDuplicated(log[c("task_id", "unit_id")]))
aligned <- prime_run(start("guarded_affine"), function(task_id, unit_id, dataset) proxy[[task_id]])
stopifnot(prime_gate(aligned)$gate_mode == "AFFINE")
short <- prime_run(start("guarded_affine", budget = 1L), query)
stopifnot(prime_gate(short)$gate_reason == "NO_POST_PILOT_DECISION",
          sum(prime_log(short)$cost) == 20)
shared_family <- prime_run(start("guarded_affine", family = stats::setNames(rep("shared", 20L), task_ids)), query)
stopifnot(prime_gate(shared_family)$gate_family_blocks == 1L,
          prime_gate(shared_family)$gate_reason == "PILOT_INSUFFICIENT")
stopifnot(failed(prime_start(tasks, scheduler = "guarded_affine")))

wide <- data.frame(support_lower = -.1, support_upper = .1, proxy_generic = .03)[rep(1L, 80L), ]
settled <- data.frame(support_lower = .03, support_upper = .04, proxy_generic = .03)[rep(1L, 80L), ]
continuation <- list(prime_task(wide, "wide", "shared"), prime_task(settled, "settled", "shared"))
result <- prime_run(prime_start(continuation, budget = 40L, scheduler = "affine_full"),
                    function(task_id, unit_id, dataset) {
                      stopifnot(task_id == "wide")
                      .03
                    })
summary <- prime_summary(result)
stopifnot(summary$used[1] > 64L, summary$used[2] == 0L,
          all(summary$bound_certificate), sum(summary$record_cost) <= 80)
