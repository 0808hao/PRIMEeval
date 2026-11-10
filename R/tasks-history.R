.prime_settings <- function(threshold=.01,alpha=.025,fdr_alpha=.05,seed=820081L,max_queries=32L) {
  list(threshold=threshold,alpha=alpha,fdr_alpha=fdr_alpha,seed=as.integer(seed),budgets=as.integer(max_queries),
    query_floor=.15,variance_shrink=.5,units_per_training_task=24L,random_features=32L,ridge=.4,
    min_calibration_nulls=20L,score_floor=1.645,value_horizons=c(1L,2L,4L,8L))
}
.prime_scalar <- function(x,name,lower=-Inf,upper=Inf,integer=FALSE) {
  p82_need(is.numeric(x)&&length(x)==1L&&is.finite(x)&&x>=lower&&x<=upper&&(!integer||x==floor(x)),paste('Invalid',name))
  invisible(TRUE)
}
.prime_family <- function(x)sub('__S[0-9]+$','',x)

prime_task <- function(public_units,task_id,dataset='default',history=NULL) {
  p82_need(is.data.frame(public_units)&&nrow(public_units)>0L,'public_units must be a nonempty data frame')
  forbidden <- c('y','truth','true_red','true_delta','truth_loss_difference','scenario_id','cohort_id','oracle','task_oracle')
  p82_need(!any(forbidden%in%names(public_units)),'Target truth and scenario fields are forbidden in public_units')
  p82_need(is.character(task_id)&&length(task_id)==1L&&!is.na(task_id)&&nzchar(task_id),'Invalid task_id')
  p82_need(is.character(dataset)&&length(dataset)==1L&&!is.na(dataset)&&nzchar(dataset),'Invalid dataset')
  u <- as.data.frame(public_units,stringsAsFactors=FALSE)
  p82_need(all(c('support_lower','support_upper','proxy_generic')%in%names(u)),'Missing support_lower, support_upper, or proxy_generic')
  numeric_fields <- intersect(c('support_lower','support_upper','proxy_generic','proxy_signed_confidence','proxy_frozen_disagreement',
    'proxy_relation','proxy_relation_prior','exposure_score','uncertainty_score','n_audit_records'),names(u))
  for(k in numeric_fields)p82_need(is.numeric(u[[k]])&&all(is.finite(u[[k]])),paste('Nonfinite or nonnumeric public field',k))
  p82_need(all(u$support_upper>=u$support_lower),'Public support bounds are reversed')
  if(!'unit_id'%in%names(u))u$unit_id <- seq_len(nrow(u))
  p82_need((is.character(u$unit_id)||is.numeric(u$unit_id))&&is.null(dim(u$unit_id))&&length(u$unit_id)==nrow(u)&&
    (!is.numeric(u$unit_id)||all(is.finite(u$unit_id)))&&!anyNA(u$unit_id)&&!anyDuplicated(u$unit_id)&&all(nzchar(as.character(u$unit_id))),
    'unit_id must be a unique, nonmissing character or numeric vector')
  if(!'n_audit_records'%in%names(u))u$n_audit_records <- 1
  p82_need(all(u$n_audit_records>=1),'n_audit_records must be at least one')
  pub <- p82_public(u)
  if(!is.null(history)) {
    p82_need(inherits(history,'prime_history'),'history must be made by prime_history()')
    historical_ids <- c(history$training_ids,history$calibration_ids)
    p82_need(!task_id%in%historical_ids&&! .prime_family(task_id)%in%.prime_family(historical_ids),'Target task or family overlaps history')
  }
  structure(list(task_id=task_id,dataset=dataset,unit_ids=u$unit_id,public=pub,history=history),class='prime_task')
}

.prime_memo <- function(items) {
  p82_need(is.list(items)&&length(items)>0L,'History must be a nonempty list of list(task=..., values=...)')
  out <- lapply(items,function(z) {
    p82_need(is.list(z)&&inherits(z$task,'prime_task')&&is.numeric(z$values),'Each historical item needs task and numeric values')
    t <- z$task;p <- t$public;y <- z$values
    p82_need(length(y)==p$n&&all(is.finite(y))&&all(y>=p$lo&y<=p$hi),'Historical values violate public support')
    list(task_id=t$task_id,dataset=t$dataset,pub=p,y=y,family=.prime_family(t$task_id))
  })
  ids <- vapply(out,function(z)z$task_id,character(1L));p82_need(!anyDuplicated(ids),'Duplicate historical task IDs');out
}

prime_history <- function(fit=NULL,cuts=NULL,training=NULL,calibration=NULL,threshold=.01,seed=820081L,max_queries=32L,
                          fold_id='PRIME_HISTORY',training_ids=character(),calibration_ids=character()) {
  .prime_scalar(threshold,'threshold');.prime_scalar(seed,'seed',1,2147483646,TRUE);.prime_scalar(max_queries,'max_queries',1,64,TRUE)
  s <- .prime_settings(threshold=threshold,seed=seed,max_queries=max_queries)
  fresh <- !is.null(training)||!is.null(calibration)
  if(fresh) {
    p82_need(is.null(fit)&&is.null(cuts)&&!is.null(training)&&!is.null(calibration),'Provide both separate training and calibration, or frozen fit/cuts')
    tr <- .prime_memo(training);ca <- .prime_memo(calibration)
    training_ids <- vapply(tr,function(z)z$task_id,character(1L));calibration_ids <- vapply(ca,function(z)z$task_id,character(1L))
    p82_need(!length(intersect(.prime_family(training_ids),.prime_family(calibration_ids))),'Training and calibration families overlap')
    fit <- p82_fit_history(list(train=seq_along(tr),fold_id=fold_id,protocol='USER_HISTORY'),tr,s)
    provisional <- structure(list(fit=fit[c('variance','prior')],cuts=data.frame(step=seq_len(max_queries),cut=1.645),
      threshold=threshold,seed=seed,max_queries=max_queries,training_ids=training_ids,calibration_ids=character(),
      scope='HISTORICAL_CALIBRATION',settings=s),class='prime_history')
    calpaths <- lapply(ca,function(m) {
      task <- m$pub;st <- .prime_new_task(list(task_id=m$task_id,dataset=m$dataset,public=task,unit_ids=seq_len(task$n),history=provisional),s,max_queries)
      while(length(st$seen)<st$limit) {
        req <- .prime_task_request(st,s);st <- .prime_observe(st,req,m$y[req$index],s)
      }
      list(trace=st$trace,true_delta=mean(m$y))
    })
    cuts <- do.call(rbind,lapply(seq_len(max_queries),function(step) {
      d <- do.call(rbind,lapply(calpaths,function(m) {
        r <- m$trace[min(step,nrow(m$trace)),,drop=FALSE]
        data.frame(estimate=r$PLUGIN_IMPUTE,scale=r$PLUGIN_IMPUTE_se,true_delta=m$true_delta)
      }))
      cc <- p82_cut(d,s,'ERROR_Q');data.frame(step=step,cut=cc$cut,available=cc$available,n_null=cc$n_null)
    }))
  }
  p82_need(is.list(fit)&&all(c('variance','prior')%in%names(fit)),'Frozen fit must contain variance and prior')
  p82_need(is.numeric(fit$prior)&&length(fit$prior)==1L&&is.finite(fit$prior)&&fit$prior>0,'Invalid historical variance prior')
  p82_need(is.list(fit$variance)&&all(c('coef','design')%in%names(fit$variance)),'Invalid frozen variance model')
  p82_need(is.data.frame(cuts),'cuts must be a data frame')
  if(all(c('readout','calibration')%in%names(cuts)))cuts <- cuts[cuts$readout=='PLUGIN_IMPUTE'&cuts$calibration=='ERROR_Q',,drop=FALSE]
  p82_need(all(c('step','cut')%in%names(cuts))&&!anyDuplicated(cuts$step),'cuts needs unique step and cut columns')
  p82_need(is.numeric(cuts$step)&&all(is.finite(cuts$step)&cuts$step>=1&cuts$step==floor(cuts$step))&&
    is.numeric(cuts$cut)&&!anyNA(cuts$cut)&&all(cuts$cut>=0),'Invalid calibration cut values')
  p82_need(all(seq_len(max_queries)%in%cuts$step),'cuts must cover every step through max_queries')
  p82_need(is.character(training_ids)&&is.character(calibration_ids)&&!anyNA(c(training_ids,calibration_ids)),'Invalid historical IDs')
  p82_need(!length(intersect(.prime_family(training_ids),.prime_family(calibration_ids))),'Training and calibration families overlap')
  structure(list(fit=fit[c('variance','prior')],cuts=as.data.frame(cuts),threshold=threshold,seed=seed,max_queries=as.integer(max_queries),
    training_ids=training_ids,calibration_ids=calibration_ids,settings=s,
    scope=if(fresh)'FRESH_SEPARATE_HISTORICAL_TRAIN_AND_CALIBRATION' else 'USER_SUPPLIED_FROZEN_HISTORY'),class='prime_history')
}

.prime_new_task <- function(task,s,max_queries) {
  history <- task$history;p <- task$public
  if(!is.null(history)) {
    historical_ids <- c(history$training_ids,history$calibration_ids)
    p82_need(!task$task_id%in%historical_ids&&! .prime_family(task$task_id)%in%.prime_family(historical_ids),
      'Target task or family overlaps history')
  }
  if(!is.null(history))p82_need(abs(history$threshold-s$threshold)<1e-14&&history$seed==s$seed&&history$max_queries>=min(p$n,max_queries),
    'History threshold, seed, or query horizon differs from audit settings')
  recipe <- list(predictor='OFFSET',acquisition='WIDTH',variance=if(is.null(history))'NONE' else 'HISTORY',betting='KELLY')
  a <- p82_prepare(p,if(is.null(history))NULL else history$fit,recipe,task$task_id,s)
  L <- min(p$n,max_queries);uniform <- p82_rng(p82_seed(s$seed,task$task_id,'COMMON_QUERY_UNIFORMS'),function()stats::runif(L))
  seen <- integer();ord <- integer(L);qq <- numeric(L)
  for(t in seq_len(L)) {
    rem <- setdiff(seq_len(p$n),seen);v <- p$width[rem]/sqrt(p$cost[rem]);v[!is.finite(v)|v<0] <- 0
    if(!any(v>0))v[] <- 1
    q <- s$query_floor/length(rem)+(1-s$query_floor)*v/sum(v);q <- q/sum(q)
    k <- which(cumsum(q)>=uniform[t])[1L];ord[t] <- rem[k];qq[t] <- q[k];seen <- c(seen,ord[t])
  }
  list(task_id=task$task_id,dataset=task$dataset,unit_ids=task$unit_ids,public=p,N=p$n,a=a,recipe=recipe,
    cuts=if(is.null(history))NULL else history$cuts,history_scope=if(is.null(history))'UNCALIBRATED_PUBLIC_PRIOR' else history$scope,
    order=ord,order_q=qq,limit=L,seen=integer(),y=numeric(),q=numeric(),prequential_error=numeric(),
    expert_loss=rep(0,ncol(a$experts)),stack_loss=rep(0,3L),frozen=NULL,capital=rep(0,3L),ZZ=numeric(),VV=numeric(),
    trace=data.frame(),ever_screen=FALSE)
}
