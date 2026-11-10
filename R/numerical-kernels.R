# Internal numerical kernels preserved from the tested PRIME development engine.
# Only the fixed WIDTH/OFFSET/KELLY recipe is exposed by this package.
p82_need <- function (ok, msg) 
if (!isTRUE(ok)) stop(msg, call. = FALSE)

p82_hash <- function (x) 
tolower(digest::digest(x, algo = "sha256", serialize = TRUE, serializeVersion = 3L))

p82_seed <- function (...) 
{
    h <- digest::digest(enc2utf8(paste(..., collapse = "|")), algo = "sha256", serialize = FALSE)
    as.integer(strtoi(substr(h, 1L, 7L), 16L)%%2147483629 + 1L)
}

p82_rng <- function (seed, fun) 
{
    present <- exists(".Random.seed", .GlobalEnv, inherits = FALSE)
    kind <- RNGkind()
    if (present) 
        old <- get(".Random.seed", .GlobalEnv, inherits = FALSE)
    on.exit({
        do.call(RNGkind, as.list(kind))
        if (present) assign(".Random.seed", old, .GlobalEnv) else if (exists(".Random.seed", .GlobalEnv, inherits = FALSE)) rm(".Random.seed", 
            envir = .GlobalEnv)
    }, add = TRUE)
    set.seed(seed, kind = "L'Ecuyer-CMRG", normal.kind = "Inversion", sample.kind = "Rejection")
    fun()
}

p82_public <- function (u) 
{
    get <- function(k, default = 0) {
        v <- if (k %in% names(u)) 
            as.numeric(u[[k]])
        else rep(default, length.out = nrow(u))
        p82_need(length(v) == nrow(u), paste("\u5b57\u6bb5\u957f\u5ea6\u9519\u8bef", k))
        v[!is.finite(v)] <- 0
        v
    }
    lo0 <- get("support_lower")
    hi0 <- get("support_upper")
    pad <- 1e-10 * (1 + pmax(abs(lo0), abs(hi0)))
    lo <- lo0 - pad
    hi <- hi0 + pad
    generic <- pmin(hi0, pmax(lo0, get("proxy_generic")))
    source <- if ("source_id" %in% names(u)) 
        as.character(u$source_id)
    else rep("ALL", nrow(u))
    source[is.na(source) | !nzchar(source)] <- "UNKNOWN"
    rel <- get("proxy_relation")
    list(n = nrow(u), lo = lo, hi = hi, width = hi - lo, ranking_width = hi0 - lo0, generic = generic, signed = get("proxy_signed_confidence", 
        generic), frozen = get("proxy_frozen_disagreement", generic), relation = rel, prior = get("proxy_relation_prior", 
        rel), exposure = get("exposure_score"), uncertainty = get("uncertainty_score", hi0 - lo0), cost = get("n_audit_records", 
        1), source = source)
}

p82_shuffle <- function (pub, id, seed) 
{
    ix <- p82_rng(p82_seed(seed, id, "RELATION_NEGATIVE_CONTROL"), function() sample.int(pub$n))
    pub$relation <- pub$relation[ix]
    pub$prior <- pub$prior[ix]
    pub$exposure <- pub$exposure[ix]
    pub$source <- pub$source[ix]
    pub
}

p82_features <- function (pub, relation = "NONE", task_id = "", seed = 1L) 
{
    if (relation == "SHUFFLED") 
        pub <- p82_shuffle(pub, task_id, seed)
    w <- pmax(pub$width, 1e-08)
    sc <- max(mean(pub$width), 1e-08)
    n <- pub$n
    clip <- function(x) pmax(-10, pmin(10, x))
    norm <- function(x) {
        z <- sqrt(mean((x - mean(x))^2))
        if (z < 1e-10) 
            rep(0, n)
        else clip((x - mean(x))/z)
    }
    z <- cbind(width = clip(w/sc), lo = clip(pub$lo/sc), generic = clip(pub$generic/sc), generic_fraction = clip((pub$generic - 
        pub$lo)/w), signed_residual = clip((pub$signed - pub$generic)/w), frozen_residual = clip((pub$frozen - pub$generic)/w), 
        uncertainty = clip(pub$uncertainty/sc), record_cost = clip(log1p(pub$cost)/max(mean(log1p(pub$cost)), 1e-08)), generic_rank = (rank(pub$generic, 
            ties.method = "average") - 0.5)/n, width_rank = (rank(w, ties.method = "average") - 0.5)/n)
    context <- c(log_units = log1p(n), log_width = log(sc), mean_lo = mean(pub$lo)/sc, mean_generic = mean(pub$generic)/sc, 
        sd_generic = sqrt(mean((pub$generic - mean(pub$generic))^2))/sc, mean_signed_difference = mean(pub$signed - pub$generic)/sc, 
        mean_frozen_difference = mean(pub$frozen - pub$generic)/sc)
    rel <- if (relation == "NONE") 
        matrix(0, n, 15L)
    else {
        r <- norm(pub$relation)
        pr <- norm(pub$prior)
        ex <- norm(pub$exposure)
        counts <- table(pub$source)
        fraction <- as.numeric(counts[pub$source])/n
        cbind(r, pr, ex, fraction, r^2 - mean(r^2), r * z[, "generic_fraction"], r * z[, "signed_residual"], r * z[, "frozen_residual"], 
            clip(pub$relation), clip(pub$prior), clip(pub$exposure), rep(mean(clip(pub$relation)), n), rep(sqrt(mean((pub$relation - 
                mean(pub$relation))^2)), n), rep(length(unique(pub$source))/n, n), clip(pub$relation) * norm(pub$generic))
    }
    colnames(rel) <- c("relation_z", "prior_z", "exposure_z", "source_fraction", "relation_square", "relation_gain", "relation_signed", 
        "relation_frozen", "relation_raw", "prior_raw", "exposure_raw", "relation_mean", "relation_sd", "source_count_fraction", 
        "relation_raw_gain")
    out <- cbind(z, rel, matrix(rep(context, each = n), n, dimnames = list(NULL, names(context))))
    out[!is.finite(out)] <- 0
    out
}

p82_design <- function (X, center = NULL, scale = NULL, projection = NULL, nonlinear = FALSE, seed = 1L, k = 32L) 
{
    X <- as.matrix(X)
    if (is.null(center)) 
        center <- colMeans(X)
    if (is.null(scale)) 
        scale <- sqrt(colMeans(sweep(X, 2L, center, "-")^2))
    scale[!is.finite(scale) | scale < 1e-08] <- 1
    Z <- sweep(sweep(X, 2L, center, "-"), 2L, scale, "/")
    Z[] <- pmax(-8, pmin(8, Z))
    if (nonlinear) {
        if (is.null(projection)) 
            projection <- p82_rng(seed, function() matrix(stats::rnorm(ncol(Z) * k)/sqrt(ncol(Z)), ncol(Z), k))
        Z <- cbind(Z, tanh(Z %*% projection))
    }
    list(Z = cbind(intercept = 1, Z), center = center, scale = scale, projection = projection, nonlinear = nonlinear)
}

p82_ridge <- function (X, y, w, nonlinear, s, seed, penalty = s$ridge) 
{
    des <- p82_design(X, nonlinear = nonlinear, seed = seed, k = s$random_features)
    w <- w/sum(w)
    A <- crossprod(des$Z * sqrt(w))
    diag(A) <- diag(A) + penalty
    A[1, 1] <- A[1, 1] - penalty + 1e-08
    coef <- as.numeric(solve(A, crossprod(des$Z, w * y)))
    p82_need(all(is.finite(coef)), "\u8de8\u4efb\u52a1\u56de\u5f52\u4ea7\u751f\u975e\u6709\u9650\u7cfb\u6570")
    list(coef = coef, design = des[c("center", "scale", "projection", "nonlinear")])
}

p82_ridge_predict <- function (fit, X) 
{
    d <- fit$design
    z <- p82_design(X, center = d$center, scale = d$scale, projection = d$projection, nonlinear = d$nonlinear)
    as.numeric(z$Z %*% fit$coef)
}

p82_clip <- function (x, lo = 0, hi = 1) 
{
    if (is.matrix(x)) {
        z <- x
        z[] <- pmax(lo, pmin(hi, x))
        z
    }
    else pmax(lo, pmin(hi, x))
}

p82_iso_fit <- function (x, y, w = rep(1, length(y))) 
{
    o <- order(x, seq_along(x))
    x <- x[o]
    y <- y[o]
    w <- w[o]
    u <- unique(x)
    j <- match(x, u)
    sw <- as.numeric(rowsum(w, j, reorder = FALSE))
    sy <- as.numeric(rowsum(w * y, j, reorder = FALSE))
    k <- 0L
    left <- right <- integer(length(u))
    weight <- value <- numeric(length(u))
    for (i in seq_along(u)) {
        k <- k + 1L
        left[k] <- right[k] <- i
        weight[k] <- sw[i]
        value[k] <- sy[i]/sw[i]
        while (k > 1L && value[k - 1L] > value[k]) {
            value[k - 1L] <- (weight[k - 1L] * value[k - 1L] + weight[k] * value[k])/(weight[k - 1L] + weight[k])
            weight[k - 1L] <- weight[k - 1L] + weight[k]
            right[k - 1L] <- right[k]
            k <- k - 1L
        }
    }
    out <- numeric(length(u))
    for (i in seq_len(k)) out[left[i]:right[i]] <- value[i]
    list(x = u, y = out)
}

p82_iso_predict <- function (fit, x) 
if (length(fit$x) == 1L) rep(fit$y, length(x)) else as.numeric(stats::approx(fit$x, fit$y, xout = x, rule = 2, ties = "ordered")$y)

p82_fit_history <- function (fold, memo, s, deadline = Inf) 
{
    pieces <- lapply(fold$train, function(j) {
        p82_check(deadline)
        m <- memo[[j]]
        p <- m$pub
        ii <- p82_rng(p82_seed(s$seed, m$task_id, "TRAIN_UNITS"), function() sort(sample.int(p$n, min(p$n, s$units_per_training_task))))
        X <- p82_features(p, "RELATION", m$task_id, s$seed)
        X0 <- p82_features(p, "NONE", m$task_id, s$seed)
        XS <- p82_features(p, "SHUFFLED", m$task_id, s$seed)
        y <- (m$y[ii] - p$generic[ii])/pmax(p$width[ii], 1e-08)
        list(X = X[ii, , drop = FALSE], X0 = X0[ii, , drop = FALSE], XS = XS[ii, , drop = FALSE], y = y, w = rep(1/length(ii), 
            length(ii)), xfrac = (p$generic[ii] - p$lo[ii])/pmax(p$width[ii], 1e-08), yfrac = (m$y[ii] - p$lo[ii])/pmax(p$width[ii], 
            1e-08), n = length(ii), cost = sum(p$cost[ii]))
    })
    extract <- function(k) do.call(rbind, lapply(pieces, function(a) a[[k]]))
    X <- extract("X")
    w <- unlist(lapply(pieces, function(a) a$w))
    y <- unlist(lapply(pieces, function(a) a$y))
    seed <- p82_seed(s$seed, fold$fold_id, "SHARED_PROJECTION")
    fit <- list(base = p82_ridge(extract("X0"), y, w, TRUE, s, seed), relation = p82_ridge(X, y, w, TRUE, s, seed), shuffle = p82_ridge(extract("XS"), 
        y, w, TRUE, s, seed), variance = p82_ridge(X, pmin(1, y^2), w, TRUE, s, seed), prior = max(1e-05, sum(w * y^2)/sum(w)), 
        iso = p82_iso_fit(unlist(lapply(pieces, function(a) a$xfrac)), unlist(lapply(pieces, function(a) a$yfrac)), w))
    fit$ledger <- data.frame(fold_id = fold$fold_id, protocol = fold$protocol, training_tasks = length(fold$train), training_units = sum(vapply(pieces, 
        function(a) a$n, integer(1L))), training_record_cost = sum(vapply(pieces, function(a) a$cost, numeric(1L))), label_reuse_scope = "SHARED_FIT_ACROSS_RECIPES_NO_RAW_RECORD_ID_DISCOUNT")
    fit
}

p82_kernel <- function (X, graph = FALSE) 
{
    Z <- as.matrix(X)
    center <- colMeans(Z)
    scale <- sqrt(colMeans(sweep(Z, 2, center, "-")^2))
    scale[scale < 1e-06] <- 1
    Z <- sweep(sweep(Z, 2, center, "-"), 2, scale, "/")
    Z <- p82_clip(Z, -4, 4)
    D <- outer(rowSums(Z^2), rowSums(Z^2), "+") - 2 * tcrossprod(Z)
    D[] <- pmax(0, D)
    h <- stats::median(D[D > 1e-10])
    if (!is.finite(h) || h <= 0) 
        h <- 1
    K <- exp(-D/(2 * h))
    if (graph && nrow(K) > 1L) {
        A <- matrix(0, nrow(K), ncol(K))
        diag(K) <- 0
        for (i in seq_len(nrow(K))) {
            j <- head(order(-K[i, ], seq_len(ncol(K))), min(8L, nrow(K) - 1L))
            A[i, j] <- K[i, j]
        }
        A <- pmax(A, t(A))
        diag(A) <- 1
        P <- A/rowSums(A)
        K <- (diag(nrow(P)) + P + P %*% P)/3
        K <- K/pmax(apply(K, 1L, max), 1e-08)
    }
    K
}

p82_prepare <- function (pub, fit, recipe, id, s) 
{
    N <- pub$n
    w <- pmax(pub$width, 1e-08)
    frac <- function(p) p82_clip((p - pub$lo)/w)
    generic <- frac(pub$generic)
    experts <- cbind(generic, frac(pub$signed), frac(pub$frozen), 1 - generic, midpoint = rep(0.5, N))
    norm <- function(x) {
        a <- stats::sd(x)
        if (!is.finite(a) || a < 1e-08) 
            rep(0, N)
        else p82_clip((x - mean(x))/a, -4, 4)
    }
    Xbase <- cbind(generic, experts[, 2], experts[, 3], norm(log(w)), norm(pub$uncertainty), norm(log(pub$cost)))
    Xrel <- cbind(Xbase, norm(pub$relation), norm(pub$prior), norm(pub$exposure))
    XB <- cbind(intercept = 1, generic = generic - 0.5, signed = experts[, 2] - generic, frozen = experts[, 3] - generic, 
        width = norm(w), uncertainty = norm(pub$uncertainty), relation = norm(pub$relation), prior = norm(pub$prior))
    p0 <- pub$generic
    mode <- recipe$predictor
    feats <- if (recipe$variance == "HISTORY" || mode %in% c("HIST_BASE", "HIST_REL", "HIST_SHUFFLE")) 
        p82_features(pub, "RELATION", id, s$seed)
    else NULL
    g <- if (recipe$variance == "HISTORY") 
        p82_clip((1 - s$variance_shrink) * fit$prior + s$variance_shrink * p82_clip(p82_ridge_predict(fit$variance, feats), 
            1e-06, 1), 1e-06, 1)
    else rep(0.05, N)
    if (mode %in% c("HIST_BASE", "HIST_REL", "HIST_SHUFFLE")) {
        kind <- if (mode == "HIST_BASE") 
            "NONE"
        else if (mode == "HIST_SHUFFLE") 
            "SHUFFLED"
        else "RELATION"
        targetfit <- fit[[switch(mode, HIST_BASE = "base", HIST_REL = "relation", HIST_SHUFFLE = "shuffle")]]
        p0 <- pub$lo + w * p82_clip(generic + p82_ridge_predict(targetfit, p82_features(pub, kind, id, s$seed)))
    }
    if (mode == "HIST_ISOTONIC") 
        p0 <- pub$lo + w * p82_clip(p82_iso_predict(fit$iso, generic))
    if (mode == "MIDPOINT") 
        p0 <- (pub$lo + pub$hi)/2
    K <- NULL
    if (mode %in% c("KERNEL_BASE", "KERNEL_REL", "GRAPH", "STACK") || recipe$acquisition == "DIVERSITY") 
        K <- p82_kernel(if (mode %in% c("KERNEL_REL", "GRAPH")) 
            Xrel
        else Xbase, graph = mode == "GRAPH")
    strata <- integer(N)
    strata[order(pub$relation, seq_len(N))] <- pmin(4L, ceiling(seq_len(N) * 4/N))
    list(pub = pub, p0 = p0, x0 = frac(p0), generic = generic, experts = experts, g0 = g, XB = XB, K = K, strata = strata)
}

p82_weights <- function (st) 
{
    n <- length(st$seen)
    if (!n) 
        return(list(w = numeric(), ess = 0))
    a <- 1/pmax(1e-08, st$q * (st$N - seq_len(n) + 1))
    a <- pmin(a, 20)
    ess <- sum(a)^2/sum(a^2)
    list(w = a/sum(a) * ess, ess = ess)
}

p82_predict <- function (a, st, mode, s) 
{
    pub <- a$pub
    N <- pub$n
    ii <- st$seen
    t <- length(ii)
    ww <- p82_weights(st)
    w <- ww$w
    ess <- ww$ess
    yf <- if (t) 
        (st$y - pub$lo[ii])/pmax(pub$width[ii], 1e-08)
    else numeric()
    offset <- function(base) if (t) 
        sum(w * (yf - base[ii]))/(4 + ess)
    else 0
    b0 <- a$x0
    b <- b0 + offset(b0)
    gate <- "UNASSESSED"
    beta <- 1
    C <- NULL
    stack <- NULL
    affine <- function() {
        prior <- c(0.5, 1)
        if (!t) 
            return(list(pred = a$generic, beta = prior))
        X <- cbind(1, a$generic[ii] - 0.5)
        A <- crossprod(X * sqrt(w)) + diag(c(4, 4))
        z <- as.numeric(solve(A, crossprod(X, w * yf) + c(4, 4) * prior))
        list(pred = as.numeric(cbind(1, a$generic - 0.5) %*% z), beta = z)
    }
    hedge <- function() {
        loss <- st$expert_loss
        logits <- -loss/sqrt(1 + t)
        logits <- logits - max(logits)
        mix <- exp(logits)/sum(exp(logits))
        z <- as.numeric(a$experts %*% mix)
        z + offset(z)
    }
    local <- function() {
        if (!t) 
            return(b0)
        KW <- sweep(a$K[, ii, drop = FALSE], 2, w, "*")
        b0 + as.numeric(KW %*% (yf - b0[ii]))/(1 + rowSums(KW))
    }
    if (mode == "MIDPOINT" || mode == "GENERIC") 
        b <- b0
    else if (mode == "AFFINE") {
        z <- affine()
        b <- z$pred
        beta <- z$beta[2]
    }
    else if (mode == "PILOT_FREEZE") {
        if (is.null(st$frozen) && t >= 4L) 
            st$frozen <- p82_clip(affine()$pred)
        b <- if (is.null(st$frozen)) 
            b
        else st$frozen
    }
    else if (mode %in% c("ISOTONIC", "BIDIR_ISOTONIC")) {
        if (t >= 4L && length(unique(a$generic[ii])) >= 2L) {
            direction <- 1
            if (mode == "BIDIR_ISOTONIC" && sum(w * (a$generic[ii] - sum(w * a$generic[ii])/sum(w)) * (yf - sum(w * yf)/sum(w))) < 
                0) 
                direction <- -1
            iso <- p82_iso_fit(direction * a$generic[ii], yf, w)
            pred <- p82_iso_predict(iso, direction * a$generic)
            b <- (4 * b0 + ess * pred)/(4 + ess)
            beta <- direction
        }
    }
    else if (mode == "HEDGE") 
        b <- hedge()
    else if (mode == "GATE") {
        z <- affine()
        beta <- z$beta[2]
        if (t >= 4L) {
            mx <- sum(w * a$generic[ii])/sum(w)
            my <- sum(w * yf)/sum(w)
            vx <- sum(w * (a$generic[ii] - mx)^2)
            vy <- sum(w * (yf - my)^2)
            cor <- sum(w * (a$generic[ii] - mx) * (yf - my))/sqrt(max(1e-12, vx * vy))
            if (cor < -0.2) {
                gate <- "REVERSED"
                base <- 1 - a$generic
                b <- base + offset(base)
            }
            else if (cor > 0.2) {
                gate <- "DIRECT"
                b <- z$pred
            }
            else {
                gate <- "FALLBACK"
                b <- rep((2 + sum(w * yf))/(4 + ess), N)
            }
        }
    }
    else if (mode %in% c("KERNEL_BASE", "KERNEL_REL", "GRAPH")) 
        b <- local()
    else if (mode == "SOURCE") {
        b <- b0 + offset(b0)
        for (k in unique(pub$source)) {
            obs <- which(pub$source[ii] == k)
            jj <- which(pub$source == k)
            if (length(obs)) 
                b[jj] <- b0[jj] + (2 * offset(b0) + sum(w[obs] * (yf[obs] - b0[ii[obs]])))/(2 + sum(w[obs]))
        }
    }
    else if (mode == "BAYES") {
        X <- a$XB
        prior <- c(0.5, 1, rep(0, ncol(X) - 2L))
        A <- diag(4, ncol(X))
        rhs <- 4 * prior
        if (t) {
            A <- A + crossprod(X[ii, , drop = FALSE] * sqrt(w))
            rhs <- rhs + as.numeric(crossprod(X[ii, , drop = FALSE], w * yf))
        }
        C <- solve(A)
        coef <- as.numeric(C %*% rhs)
        b <- as.numeric(X %*% coef)
        beta <- coef[2]
    }
    else if (mode == "STACK") {
        stack <- cbind(affine()$pred, hedge(), local())
        logits <- -st$stack_loss/sqrt(1 + t)
        logits <- logits - max(logits)
        mix <- exp(logits)/sum(exp(logits))
        b <- as.numeric(stack %*% mix)
    }
    else p82_need(mode %in% c("OFFSET", "HIST_BASE", "HIST_REL", "HIST_SHUFFLE", "HIST_ISOTONIC"), paste("\u672a\u77e5\u9884\u6d4b\u5668", 
        mode))
    fraction <- p82_clip(b)
    pred <- pub$lo + pub$width * fraction
    err <- st$prequential_error
    noise <- if (length(err)) 
        mean(pmin(1, err^2))
    else mean(a$g0)
    g <- p82_clip((4 * a$g0 + ess * noise)/(4 + ess), 1e-06, 1)
    list(pred = pred, g = g, fraction = fraction, gate = gate, beta = beta, C = C, stack = stack)
}

p82_probability <- function (a, pred, st, recipe, s, tau) 
{
    p <- a$pub
    rem <- setdiff(seq_len(p$n), st$seen)
    n <- length(rem)
    mode <- recipe$acquisition
    normalize <- function(v) {
        v[!is.finite(v) | v < 0] <- 0
        if (!any(v > 0)) 
            v[] <- 1
        z <- s$query_floor/n + (1 - s$query_floor) * v/sum(v)
        z/sum(z)
    }
    width <- p$width[rem]/sqrt(p$cost[rem])
    rms <- p$width[rem] * sqrt(pred$g[rem])
    qwidth <- normalize(width)
    v <- switch(mode, UNIFORM = rep(1, n), WIDTH = width, CONDVAR = rms/sqrt(p$cost[rem]), DISAGREE = (rms + p$width[rem] * 
        sqrt(rowMeans((a$experts[rem, , drop = FALSE] - rowMeans(a$experts[rem, , drop = FALSE]))^2)))/sqrt(p$cost[rem]), 
        THRESHOLD = rms * exp(-abs(pred$pred[rem] - tau)/pmax(rms, 1e-08))/sqrt(p$cost[rem]), CERTIFICATE = pmax(pred$pred[rem] - 
            p$lo[rem], 0.05 * p$width[rem])/sqrt(p$cost[rem]), CERTIFICATE_COST = pmax(pred$pred[rem] - p$lo[rem], 0.05 * 
            p$width[rem])/p$cost[rem], DIVERSITY = {
            d <- if (length(st$seen)) 1 - apply(a$K[rem, st$seen, drop = FALSE], 1L, max) else rep(1, n)
            rms * (0.1 + d)/sqrt(p$cost[rem])
        }, SOURCE_BALANCE = {
            all <- table(p$source)
            counts <- table(factor(p$source[st$seen], levels = names(all)))
            rms * as.numeric(all[p$source[rem]])/(1 + as.numeric(counts[p$source[rem]]))/sqrt(p$cost[rem])
        }, STRATA_BALANCE = {
            all <- table(a$strata)
            counts <- table(factor(a$strata[st$seen], levels = names(all)))
            rms * as.numeric(all[as.character(a$strata[rem])])/(1 + as.numeric(counts[as.character(a$strata[rem])]))/sqrt(p$cost[rem])
        }, INFOGAIN = {
            C <- pred$C
            X <- a$XB
            mean_x <- colSums(X[rem, , drop = FALSE] * p$width[rem])/p$n
            cov <- as.numeric(X[rem, , drop = FALSE] %*% C %*% mean_x)
            diag <- rowSums((X[rem, , drop = FALSE] %*% C) * X[rem, , drop = FALSE])
            (cov^2/(0.02 + diag) + 1e-06 * rms^2)/p$cost[rem]
        }, GATED = {
            if (length(st$seen) < 4L) rep(1, n) else if (pred$gate == "FALLBACK") width else pmax(pred$pred[rem] - p$lo[rem], 
                0.05 * p$width[rem])/sqrt(p$cost[rem])
        }, MINIMAX = {
            if (pred$gate == "FALLBACK" || pred$gate == "UNASSESSED") width else rms/sqrt(p$cost[rem])
        }, LOGWEALTH = width, stop(paste("\u672a\u77e5\u91c7\u6837\u5668", mode), call. = FALSE))
    q <- normalize(v)
    if (mode == "LOGWEALTH") {
        candidates <- list(qwidth, normalize(rms/sqrt(p$cost[rem])), normalize(pmax(pred$pred[rem] - p$lo[rem], 0.05 * p$width[rem])/sqrt(p$cost[rem])), 
            rep(1/n, n))
        base <- (sum(st$y) + sum(pred$pred[rem]))/p$n
        evaluate <- function(qq) {
            lower <- base + min((p$lo[rem] - pred$pred[rem])/qq)/p$n
            cap <- if (tau > lower) 
                0.9/(tau - lower)
            else 0
            variance <- sum((p$width[rem]^2 * pred$g[rem])/qq)/p$n^2
            lambda <- if (base > tau) 
                min(cap, (base - tau)/(variance + (base - tau)^2 + 1e-20))
            else 0
            if (lambda <= 0) 
                return(0)
            loss <- vapply(c(-1, 0, 1), function(k) {
                yy <- p82_clip(pred$pred[rem] + k * rms, p$lo[rem], p$hi[rem])
                sum(qq * log(pmax(1e-12, 1 + lambda * (base + (yy - pred$pred[rem])/(p$n * qq) - tau))))
            }, numeric(1L))
            sum(c(0.25, 0.5, 0.25) * loss)/sum(qq * p$cost[rem])
        }
        values <- vapply(candidates, evaluate, numeric(1L))
        q <- candidates[[which.max(values)]]
    }
    p82_need(all(is.finite(q) & q > 0) && abs(sum(q) - 1) < 1e-09, "\u975e\u6b63\u6216\u672a\u5f52\u4e00\u5316\u91c7\u6837\u6982\u7387")
    list(rem = rem, q = q, l1 = sum(abs(q - qwidth)))
}

p82_lse <- function (x) 
{
    a <- max(x)
    if (!is.finite(a)) 
        a
    else a + log(sum(exp(x - a)))
}

p82_ebh <- function (loge, alpha) 
{
    n <- length(loge)
    o <- order(-loge, seq_len(n))
    k <- which(loge[o] >= log(n/(alpha * seq_len(n))))
    out <- rep(FALSE, n)
    if (length(k)) 
        out[o[seq_len(max(k))]] <- TRUE
    out
}

p82_check <- function (deadline) 
if (is.finite(deadline) && as.numeric(Sys.time()) >= deadline) stop("PRIME82_TIME_LIMIT", call. = FALSE)

p82_cut <- function (d, s, kind) 
{
    ix <- is.finite(d$estimate) & is.finite(d$scale) & d$scale > 0 & d$true_delta <= s$threshold
    z <- (d$estimate[ix] - d$true_delta[ix])/d$scale[ix]
    n <- length(z)
    if (n < s$min_calibration_nulls) 
        return(list(cut = Inf, n_null = n, available = FALSE))
    q <- sort(z, decreasing = TRUE)[min(n, floor(s$alpha * n) + 1L)]
    bias_scale <- stats::median(z) + s$score_floor * max(1, sqrt(mean((z - stats::median(z))^2)))
    list(cut = max(s$score_floor, if (kind == "BIAS_SCALE") bias_scale else q), n_null = n, available = TRUE)
}

p88_need <- function (ok, msg) 
if (!isTRUE(ok)) stop(paste0("PRIME088\uff1a", msg), call. = FALSE)

p88_features <- function (N, t, B, initial, width, maxwidth, lower, estimate, se, loge, ess, recent_gain, loge_change, h, tau) 
{
    W <- max(width, 1e-08)
    gain <- max(0, (lower - initial) * N)
    rate <- (W + gain)/(2 + t)
    gap <- (tau + 1e-12 - lower) * N
    h <- as.numeric(h)
    m <- length(h)
    repm <- function(x) rep(as.numeric(x), m)
    z <- cbind(log_n = repm(log1p(N)), sample_fraction = repm(t/N), remaining_fraction = repm((B - t)/N), initial_gap = repm((tau - 
        initial)/W), current_gap = repm((tau - lower)/W), gain_rate = repm(gain/((t + 0.5) * W)), recent_gain_rate = repm(recent_gain/W), 
        gain_acceleration = repm((recent_gain - gain/max(1, t))/W), plugin_z = repm((estimate - tau)/max(se, 1e-10)), plugin_margin = repm((estimate - 
            tau)/W), plugin_uncertainty = repm(se/W), current_loge = repm(loge/5), loge_change = repm(loge_change/5), ess_fraction = repm(ess/max(1, 
            t)), max_width_ratio = repm(maxwidth/W), log_h = log1p(h), h_fraction = h/max(1, B - t), h_population_fraction = h/N, 
        reaches_census = as.numeric(t + h == N), projected_gap = (gap - h * rate)/(N * W), max_gain_gap = (gap - h * maxwidth)/(N * 
            W), need_per_h = pmax(0, gap)/(h * rate + W))
    z[] <- pmax(-32, pmin(32, z))
    p88_need(all(is.finite(z)), "\u5b66\u4e60\u7279\u5f81\u51fa\u73b0\u975e\u6709\u9650\u503c")
    z
}

p88_horizons <- function (t, B, s) 
sort(unique(c(s$value_horizons[s$value_horizons <= B - t], B - t)))

p88_predict_value <- function (fit, X) 
{
    p88_need(identical(colnames(X), fit$feature_names) && all(is.finite(X)), "\u9884\u6d4b\u7279\u5f81\u67b6\u6784\u4e0d\u540c")
    eta <- rep(fit$intercept, nrow(X))
    outside <- matrix(FALSE, nrow(X), ncol(X))
    for (j in seq_len(ncol(X))) {
        eta <- eta + fit$lookup[[j]][findInterval(X[, j], fit$cuts[[j]]) + 1L]
        outside[, j] <- X[, j] < fit$range_min[j] - 1e-10 | X[, j] > fit$range_max[j] + 1e-10
    }
    list(probability = pmax(1e-07, pmin(1 - 1e-07, stats::plogis(eta))), outside_fraction = rowMeans(outside))
}

