#' Small reproducible public-only audit example.
prime_example <- function() {
  ids <- c('demo_candidate_A','demo_candidate_B','demo_candidate_C')
  proxy <- c(.018,.002,.012);values <- c(.030,-.005,.018)
  tasks <- lapply(seq_along(ids),function(i) {
    public <- data.frame(unit_id=seq_len(12L),support_lower=-.02,support_upper=.04,
      proxy_generic=proxy[i],n_audit_records=1)
    prime_task(public,ids[i],dataset='example')
  })
  labels <- stats::setNames(lapply(values,function(y)rep(y,12L)),ids)
  # The simulated hidden labels live in the callback, not in prime_task/audit.
  query <- local({hidden <- labels;function(task_id,unit_id,dataset) {
    if(!identical(dataset,'example')||!task_id%in%names(hidden))stop('Unknown example task')
    hidden[[task_id]][as.integer(unit_id)]
  }})
  list(tasks=tasks,query=query,scope='SIMULATED_API_EXAMPLE_WITHOUT_HISTORICAL_CALIBRATION')
}
