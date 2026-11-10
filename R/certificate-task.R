# Deterministic query order and affine screen from the Step93 research strategy.
# The fitted screen prioritizes requests; only the public support interval certifies a decision.
.prime_cert_task <- function(task, limit) {
  p <- task$public
  n <- p$n
  limit <- min(n, limit)
  query_order <- order(-p$width / p$cost, seq_len(n))
  width <- pmax(p$width, 1e-10)
  fraction <- pmin(1, pmax(0, (p$generic - p$lo) / width))
  scale <- stats::sd(fraction)
  if (!is.finite(scale) || scale < 1e-8) scale <- 1
  standardized <- pmax(-4, pmin(4, (fraction - mean(fraction)) / scale))
  list(task_id = task$task_id, dataset = task$dataset, unit_ids = task$unit_ids,
       public = p, N = n, order = query_order, limit = limit,
       seen = integer(), y = numeric(), trace = data.frame(),
       affine_fraction = fraction, affine_width = width, affine_z = standardized,
       lower_total = sum(p$lo), upper_total = sum(p$hi),
       lower_prefix = cumsum(p$lo[query_order]), upper_prefix = cumsum(p$hi[query_order]),
       cost_prefix = cumsum(p$cost[query_order]))
}

.prime_cert_observe <- function(task, req, value, threshold) {
  p <- task$public
  step <- length(task$seen) + 1L
  index <- req$index
  p82_need(step <= task$limit && isTRUE(index == task$order[step]) &&
    isTRUE(req$cost == p$cost[index]), "Invalid deterministic query")
  p82_need(is.numeric(value) && length(value) == 1L && is.finite(value) &&
    value >= p$lo[index] && value <= p$hi[index], "Observed value violates public support")

  task$seen <- c(task$seen, index)
  task$y <- c(task$y, value)
  total <- sum(task$y)
  lower <- (total + task$lower_total - task$lower_prefix[step]) / p$n
  upper <- (total + task$upper_total - task$upper_prefix[step]) / p$n
  estimate <- if (step == p$n) {
    mean(task$y)
  } else {
    seen <- task$seen
    remaining <- task$order[seq.int(step + 1L, p$n)]
    # Step93 sums remaining predictions in original unit order.
    remaining <- sort.int(remaining)
    target <- (task$y - p$lo[seen]) / task$affine_width[seen]
    design <- cbind(1, task$affine_z[seen])
    residual <- target - task$affine_fraction[seen]
    beta <- as.numeric(solve(crossprod(design) + diag(c(4, 2)), crossprod(design, residual)))
    fraction <- task$affine_fraction[remaining] + beta[1L] + beta[2L] * task$affine_z[remaining]
    prediction <- p$lo[remaining] + p$width[remaining] * pmin(1, pmax(0, fraction))
    (total + sum(prediction)) / p$n
  }
  scale <- max(1e-10, (upper - lower) / sqrt(max(1L, p$n - step)))
  score <- (estimate - threshold) / scale - 1.645
  support <- lower > threshold + 1e-12
  trace_row <- data.frame(step = step, cost = task$cost_prefix[step],
    PLUGIN_IMPUTE = estimate, PLUGIN_IMPUTE_se = scale, support = support,
    support_lower = lower, support_upper = upper, screen_score = score,
    screen_red = score > 0, rawmean = total / step, unit_cost_quote = req$cost)
  task$trace <- rbind(task$trace, trace_row)
  task
}
