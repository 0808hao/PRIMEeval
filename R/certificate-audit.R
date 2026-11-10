.prime_cert_schedulers <- c("affine_full", "guarded_affine", "bound_greedy")

.prime_cert_start <- function(tasks, ids, ds, budget, cost_budget, threshold, seed,
                              max_queries, allow_full_census, scheduler, task_families) {
  p82_need(is.numeric(max_queries) && length(max_queries) == 1L && !is.na(max_queries) &&
    (identical(max_queries, Inf) || (is.finite(max_queries) && max_queries >= 1 &&
      max_queries <= .Machine$integer.max && max_queries == floor(max_queries))), "Invalid max_queries")
  p82_need(all(vapply(tasks, function(t) is.null(t$history), TRUE)),
    "Deterministic certificate schedulers require tasks without a historical fit")
  if (scheduler == "guarded_affine" || !is.null(task_families)) {
    p82_need(is.character(task_families) && !anyNA(task_families) && all(nzchar(task_families)) &&
      !is.null(names(task_families)) && !anyDuplicated(names(task_families)) &&
      setequal(names(task_families), ids), "task_families must name a public family for every task_id")
  }
  st <- lapply(tasks, function(t) {
    limit <- min(max_queries, t$public$n - as.integer(!allow_full_census))
    .prime_cert_task(t, limit)
  })
  n <- length(st)
  v <- list(N = vapply(st, function(t) t$public$n, integer(1L)),
    used = integer(n), limit = vapply(st, function(t) as.integer(t$limit), integer(1L)),
    initial = vapply(st, function(t) mean(t$public$lo), 0),
    upper = vapply(st, function(t) mean(t$public$hi), 0),
    width = vapply(st, function(t) mean(t$public$width), 0),
    proxy = vapply(st, function(t) mean(t$public$generic), 0),
    mean_cost = vapply(st, function(t) mean(t$public$cost), 0),
    quote = vapply(st, function(t) if (t$limit > 0L) t$public$cost[t$order[1L]] else Inf, 0),
    score = rep(-Inf, n), rawmean = rep(NA_real_, n))
  v$lower <- v$initial
  v$eligible <- v$N >= 2L & v$lower <= threshold + 1e-12 & v$upper > threshold
  if (!is.null(task_families)) v$family <- unname(task_families[ids])
  groups <- stats::setNames(lapply(sort(unique(ds)), function(d) {
    ix <- which(ds == d)
    cap <- if (is.null(cost_budget)) sum(v$mean_cost[ix] * pmin(v$N[ix], budget)) else
      if (is.null(names(cost_budget))) cost_budget[1L] else cost_budget[[d]]
    p82_need(is.finite(cap), "Invalid dataset cost budget")
    list(indices = ix, cost_budget = cap, cost = 0, finished = FALSE, gate = NULL)
  }), sort(unique(ds)))
  structure(list(schema = "PRIMEeval_0.8_cert_v1", scheduler = scheduler,
    settings = list(threshold = threshold), budget = as.integer(budget), task_ids = ids,
    tie_hash = vapply(ids, function(id) p82_seed(seed, id, "ALLOCATION_TIE"), integer(1L)),
    tasks = st, groups = groups, cache = v, pending = NULL, steps = 0L,
    query_rows = list()), class = "prime_audit")
}

.prime_cert_gate <- function(v, cost, decided = TRUE, finished = FALSE) {
  # Step 94 evaluates a single gate using only observations from the charged pilot.
  p82_need(all(v$used <= 2L), "Gate attempted after a post-pilot query")
  completed <- v$eligible & v$used == 2L & is.finite(v$rawmean) & is.finite(v$proxy)
  blocks <- unique(v$family[completed])
  coverage <- sum(completed) / max(1, sum(v$eligible))
  result <- list(gate_mode = "AFFINE", gate_reason = "PILOT_INSUFFICIENT", gate_rho = NA_real_,
    gate_eligible_tasks = sum(v$eligible), gate_completed_tasks = sum(completed),
    gate_family_blocks = length(blocks), gate_pilot_coverage = coverage,
    gate_pilot_cost = cost, gate_pilot_queries = sum(v$used))
  if (!decided) {
    result$gate_reason <- if (finished) "NO_POST_PILOT_DECISION" else "AWAITING_POST_PILOT"
    return(result)
  }
  if (length(blocks) < 16L || coverage < .80) return(result)
  g <- match(v$family[completed], blocks)
  count <- tabulate(g, nbins = length(blocks))
  x <- as.vector(rowsum(v$proxy[completed], g, reorder = TRUE)) / count
  y <- as.vector(rowsum(v$rawmean[completed], g, reorder = TRUE)) / count
  if (length(unique(x)) < 2L || length(unique(y)) < 2L) {
    result$gate_reason <- "CONSTANT_PILOT_RANK"
    return(result)
  }
  result$gate_rho <- stats::cor(x, y, method = "spearman")
  reversed <- is.finite(result$gate_rho) && result$gate_rho <= -.20
  result$gate_mode <- if (reversed) "BOUND" else "AFFINE"
  result$gate_reason <- if (reversed) "EARLY_RANK_REVERSAL" else "NO_STRONG_REVERSAL"
  result
}

.prime_cert_pick <- function(v, available, ties, threshold, policy) {
  gain <- pmax(0, (v$lower - v$initial) * v$N)
  rate <- (v$width + gain) / (2 + v$used)
  priority <- -pmax(0, threshold - v$lower) * v$N / pmax(rate, 1e-10) * v$mean_cost
  if (policy == "AFFINE") {
    screen <- available[v$score[available] > 0]
    if (length(screen)) available <- screen else priority <- -abs(v$score)
  }
  value <- priority[available]
  value[is.na(value)] <- -Inf
  best <- available[value == max(value)]
  best[order(v$used[best], ties[best])][1L]
}

.prime_cert_next <- function(state) {
  if (!is.null(state$pending)) return(list(state = state, request = state$pending$request))
  threshold <- state$settings$threshold
  for (d in names(state$groups)) {
    g <- state$groups[[d]]
    if (g$finished) next
    ix <- g$indices
    v <- lapply(state$cache, function(x) x[ix])
    ties <- state$tie_hash[ix]
    available <- which(v$used < v$limit & v$quote <= g$cost_budget - g$cost + 1e-8 &
      v$lower <= threshold + 1e-12 & v$upper > threshold)
    if (!length(available)) {
      state$groups[[d]]$finished <- TRUE
      next
    }
    pilot <- available[v$used[available] < min(2L, state$budget)]
    is_pilot <- length(pilot) > 0L
    if (is_pilot) {
      i <- pilot[order(v$used[pilot], ties[pilot])][1L]
    } else {
      if (state$scheduler == "guarded_affine" && is.null(g$gate))
        g$gate <- .prime_cert_gate(v, g$cost)
      policy <- if (state$scheduler == "bound_greedy" || identical(g$gate$gate_mode, "BOUND")) "BOUND" else "AFFINE"
      i <- .prime_cert_pick(v, available, ties, threshold, policy)
    }
    idx <- ix[i]
    task <- state$tasks[[idx]]
    k <- task$order[v$used[i] + 1L]
    quote <- v$quote[i]
    token <- p82_hash(list(state$schema, state$scheduler, state$steps, task$task_id, task$seen, task$y, k, quote))
    request <- data.frame(token = token, task_id = task$task_id, dataset = d,
      unit_id = task$unit_ids[k], index = k, q = 1, cost = quote,
      phase = if (is_pilot) 1L else 2L, stringsAsFactors = FALSE)
    state$groups[[d]] <- g
    state$pending <- list(request = request, task_index = idx, dataset = d, is_pilot = is_pilot)
    return(list(state = state, request = request))
  }
  list(state = state, request = NULL)
}

.prime_cert_update <- function(state, pending, value) {
  q <- pending$request
  idx <- pending$task_index
  g <- state$groups[[pending$dataset]]
  p82_need(g$cost + q$cost <= g$cost_budget + 1e-8, "Actual cost exceeds dataset budget")
  t <- .prime_cert_observe(state$tasks[[idx]], q, value, state$settings$threshold)
  n <- length(t$seen)
  last <- t$trace[n, , drop = FALSE]
  g$cost <- g$cost + q$cost
  state$groups[[pending$dataset]] <- g
  state$tasks[[idx]] <- t
  state$cache$used[idx] <- n
  state$cache$quote[idx] <- if (n < t$limit) t$public$cost[t$order[n + 1L]] else Inf
  state$cache$lower[idx] <- last$support_lower
  state$cache$upper[idx] <- last$support_upper
  state$cache$score[idx] <- last$screen_score
  state$cache$rawmean[idx] <- last$rawmean
  state$steps <- state$steps + 1L
  q$value <- value
  q$task_step <- n
  q$total_step <- state$steps
  q$is_pilot <- pending$is_pilot
  q$cumulative_cost <- g$cost
  q$certificate <- last$support
  state$query_rows[[state$steps]] <- q
  state$pending <- NULL
  state
}

.prime_cert_summary <- function(state) {
  out <- do.call(rbind, lapply(state$tasks, function(t) {
    n <- length(t$seen)
    r <- t$trace[n, , drop = FALSE]
    data.frame(task_id = t$task_id, dataset = t$dataset, n_units = t$public$n,
      used = n, record_cost = if (n) r$cost else 0,
      estimate = if (n) r$PLUGIN_IMPUTE else state$settings$threshold,
      frozen_public_mean = mean(t$public$generic),
      full_census = n == t$public$n, screen_current = if (n) r$screen_red else FALSE)
  }))
  v <- state$cache
  threshold <- state$settings$threshold
  out$screen_score <- v$score
  out$support_lower <- v$lower
  out$support_upper <- v$upper
  out$bound_certificate <- v$lower > threshold + 1e-12
  out$support_only <- out$bound_certificate
  out$risk_status <- ifelse(out$bound_certificate, "RISK_CERTIFIED",
    ifelse(v$upper <= threshold, "RISK_EXCLUDED_BY_BOUNDS", "INSUFFICIENT_BOUND_EVIDENCE"))
  out$history_scope <- "NO_HISTORY"
  out$inferential_scope <- "DETERMINISTIC_CERTIFICATE_CONDITIONAL_ON_VALID_BOUNDS"
  out$requested_scheduler <- state$scheduler
  out$effective_scheduler <- if (state$scheduler == "guarded_affine") {
    vapply(out$dataset, function(d)
      if (identical(state$groups[[d]]$gate$gate_mode, "BOUND")) "bound_greedy" else "affine_full", "")
  } else state$scheduler
  rownames(out) <- NULL
  out
}

prime_gate <- function(state) {
  .prime_validate_state(state)
  if (state$scheduler != "guarded_affine") return(data.frame())
  rows <- lapply(names(state$groups), function(d) {
    g <- state$groups[[d]]
    gate <- g$gate
    if (is.null(gate)) {
      v <- lapply(state$cache, function(x) x[g$indices])
      gate <- .prime_cert_gate(v, g$cost, decided = FALSE, finished = g$finished)
    }
    data.frame(dataset = d, gate, stringsAsFactors = FALSE)
  })
  do.call(rbind, rows)
}
