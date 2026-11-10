library(PRIMEeval)
example <- prime_example()
state <- prime_start(example$tasks, budget = 8)
state <- prime_run(state, example$query)
print(prime_summary(state))

# Request/update use: only the requested value enters the audit.
state <- prime_start(example$tasks, budget = 8)
step <- prime_next(state)
state <- step$state
request <- step$request
value <- example$query(request$task_id, request$unit_id, request$dataset)
state <- prime_update(state, request, value)

# Resume after closing R (the query source is managed separately).
checkpoint <- tempfile(fileext = '.rds')
saveRDS(state, checkpoint)
state <- readRDS(checkpoint)
state <- prime_run(state, example$query)
print(prime_summary(state))
unlink(checkpoint)
