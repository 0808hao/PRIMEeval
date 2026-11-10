\name{prime_example}
\alias{prime_example}
\title{Small Simulated Audit Example}
\description{Create three public bounded-loss tasks and an external simulated query callback. This example has no historical calibration and is intended to demonstrate the API.}
\usage{prime_example()}
\value{A list with \code{tasks}, \code{query} and a scope label. Target values reside in the external callback, not in public tasks.}
\examples{
ex <- prime_example()
s <- prime_run(prime_start(ex$tasks, budget = 8), ex$query)
prime_summary(s)
}
