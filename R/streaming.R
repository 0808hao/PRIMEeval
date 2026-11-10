.prime_task_request <- function(st,s) {
  p <- st$public;N <- p$n;t <- length(st$seen)+1L
  p82_need(t<=st$limit,'Task query limit exhausted')
  pred <- p82_predict(st$a,st,'OFFSET',s);rem <- setdiff(seq_len(N),st$seen)
  v <- p$width[rem]/sqrt(p$cost[rem]);v[!is.finite(v)|v<0] <- 0;if(!any(v>0))v[] <- 1
  q <- s$query_floor/length(rem)+(1-s$query_floor)*v/sum(v);q <- q/sum(q)
  i <- st$order[t];k <- match(i,rem)
  p82_need(!is.na(k)&&abs(q[k]-st$order_q[t])<1e-14,'Predictable WIDTH probability changed')
  base <- (sum(st$y)+sum(pred$pred[rem]))/N
  lower_Z <- base+min((p$lo[rem]-pred$pred[rem])/q)/N
  variance <- sum((p$width[rem]^2*pred$g[rem]+(pred$pred[rem]-st$a$p0[rem])^2)/q)/N^2
  cap <- if(s$threshold>lower_Z).9/(s$threshold-lower_Z) else 0
  kelly <- if(base>s$threshold)min(cap,(base-s$threshold)/(variance+(base-s$threshold)^2+1e-20)) else 0
  list(index=i,q=q[k],cost=p$cost[i],rem=rem,pred=pred$pred,variance=variance,base=base,lambda=pmin(cap,c(.25,1,4)*kelly))
}

.prime_observe <- function(st,req,value,s) {
  p <- st$public;N <- p$n;i <- req$index;t <- length(st$seen)+1L
  p82_need(length(value)==1L&&is.numeric(value)&&is.finite(value)&&value>=p$lo[i]&&value<=p$hi[i],'Observed value violates public support')
  Z <- req$base+(value-req$pred[i])/(N*req$q)
  factor <- 1+req$lambda*(Z-s$threshold);p82_need(all(is.finite(factor)&factor>0),'Invalid sequential evidence factor')
  st$capital <- st$capital+log(factor);loge <- p82_lse(st$capital)-log(length(st$capital))
  yf <- (value-p$lo[i])/max(p$width[i],1e-8);importance <- min(20,1/(length(req$rem)*req$q))
  st$expert_loss <- st$expert_loss+importance*(yf-st$a$experts[i,])^2
  st$prequential_error <- c(st$prequential_error,(value-req$pred[i])/max(p$width[i],1e-8))
  st$seen <- c(st$seen,as.integer(i));st$y <- c(st$y,value);st$q <- c(st$q,req$q)
  remaining <- setdiff(req$rem,i);st$ZZ <- c(st$ZZ,Z);st$VV <- c(st$VV,max(req$variance,1e-20))
  after <- p82_predict(st$a,st,'OFFSET',s);ess <- p82_weights(st)$ess
  j <- seq_len(t);v <- if(t==N)rep(1,t) else 1+(N-t)/(N-j)*(1/((N-j+1)*st$q)-1)
  lure <- mean(st$a$p0)+mean(v*(st$y-st$a$p0[st$seen]))
  lure_se <- max(1e-10,sqrt(sum((v*p$width[st$seen])^2*st$a$g0[st$seen]))/t)
  raw <- mean(v*st$y)
  raw_se <- if(t>1L)max(1e-10,stats::sd(v*st$y)/sqrt(t)*sqrt((N-t)/max(1,N-1L))) else max(1e-10,sqrt(mean(p$width^2))*.5)
  impute <- (sum(st$y)+sum(after$pred[remaining]))/N;noise <- mean(pmin(1,st$prequential_error^2))
  impute_se <- max(1e-10,sqrt(sum(p$width[remaining]^2*after$g[remaining])+sum(p$width[remaining])^2*(noise+1e-5)/(4+ess))/N)
  aipw <- sum(j*st$ZZ[j])/sum(j);aipw_se <- max(1e-10,sqrt(sum(j^2*st$VV[j]))/sum(j))
  if(t==N){raw <- lure <- impute <- aipw <- mean(st$y);raw_se <- lure_se <- impute_se <- aipw_se <- 1e-10}
  lower <- (sum(st$y)+sum(p$lo[remaining]))/N;support <- lower>s$threshold+1e-12
  cut <- if(is.null(st$cuts))1.645 else st$cuts$cut[match(t,st$cuts$step)]
  p82_need(length(cut)==1L&&!is.na(cut),'Missing screen calibration cut')
  score <- (impute-s$threshold)/impute_se-cut
  previous_max <- if(nrow(st$trace))tail(st$trace$max_loge,1L) else 0
  previous_support <- if(nrow(st$trace))tail(st$trace$ever_support,1L) else FALSE
  row <- data.frame(step=t,cost=sum(p$cost[st$seen]),LURE_FIXED=lure,LURE_FIXED_se=lure_se,RAW_LURE=raw,RAW_LURE_se=raw_se,
    PLUGIN_IMPUTE=impute,PLUGIN_IMPUTE_se=impute_se,PREDICTABLE_AIPW=aipw,PREDICTABLE_AIPW_se=aipw_se,
    loge=loge,support=support,support_lower=lower,ess=ess,max_loge=max(previous_max,0,loge),ever_support=previous_support||support,
    unit_cost_quote=req$cost,screen_score=score,screen_red=score>0)
  st$trace <- rbind(st$trace,row);st$ever_screen <- st$ever_screen||score>0;st$value_action <- NULL;st
}

.prime_validate_value_model <- function(m) {
  fields <- c('feature_names','cuts','lookup','intercept','range_min','range_max','usable')
  p82_need(is.list(m)&&all(fields%in%names(m)),'Invalid frozen value model')
  expected <- colnames(p88_features(10,2,10,0,.1,.2,.001,.02,.01,0,2,.03,.1,1,.01))
  p82_need(identical(m$feature_names,expected),'Frozen value model feature schema differs from the tested recipe')
  p82_need(is.logical(m$usable)&&length(m$usable)==1L&&!is.na(m$usable),'Invalid frozen value model usable flag')
  .prime_scalar(m$intercept,'value model intercept')
  k <- length(expected)
  p82_need(is.list(m$cuts)&&length(m$cuts)==k&&is.list(m$lookup)&&length(m$lookup)==k,'Invalid frozen value model tables')
  for(j in seq_len(k)) {
    cc <- m$cuts[[j]];ll <- m$lookup[[j]]
    p82_need(is.numeric(cc)&&all(is.finite(cc))&&all(diff(cc)>0)&&
      is.numeric(ll)&&length(ll)==length(cc)+1L&&all(is.finite(ll)),'Invalid frozen value model cut or lookup values')
  }
  p82_need(is.numeric(m$range_min)&&length(m$range_min)==k&&all(is.finite(m$range_min))&&
    is.numeric(m$range_max)&&length(m$range_max)==k&&all(is.finite(m$range_max))&&all(m$range_min<=m$range_max),
    'Invalid frozen value model feature ranges')
  for(key in c('training_task_ids','selection_task_ids'))if(!is.null(m[[key]]))
    p82_need(is.character(m[[key]])&&!anyNA(m[[key]])&&!anyDuplicated(m[[key]])&&all(nzchar(m[[key]])),
      'Invalid frozen value model historical task IDs')
  invisible(TRUE)
}

prime_start <- function(tasks,budget=8,cost_budget=NULL,threshold=.01,alpha=.025,fdr_alpha=.05,seed=820081L,
                        max_queries=32L,allow_full_census=TRUE,scheduler=c('support','history_value','affine_full','guarded_affine','bound_greedy'),
                        value_models=NULL,task_families=NULL) {
  p82_need(is.list(tasks)&&length(tasks)>0L&&all(vapply(tasks,inherits,logical(1L),'prime_task')),'tasks must be a list of prime_task objects')
  .prime_scalar(budget,'budget',1,64,TRUE);.prime_scalar(threshold,'threshold');.prime_scalar(alpha,'alpha',1e-12,1-1e-12)
  .prime_scalar(fdr_alpha,'fdr_alpha',1e-12,1-1e-12);.prime_scalar(seed,'seed',1,2147483646,TRUE)
  p82_need(is.logical(allow_full_census)&&length(allow_full_census)==1L&&!is.na(allow_full_census),'Invalid allow_full_census')
  scheduler <- match.arg(scheduler)
  ids <- vapply(tasks,function(t)t$task_id,character(1L));p82_need(!anyDuplicated(ids),'task_id must be globally unique')
  ds <- vapply(tasks,function(t)t$dataset,character(1L));groups <- list()
  if(!is.null(cost_budget)) {
    p82_need(is.numeric(cost_budget)&&all(is.finite(cost_budget)&cost_budget>=0),'Invalid cost_budget')
    p82_need((is.null(names(cost_budget))&&length(cost_budget)==1L)||
      (!is.null(names(cost_budget))&&!anyNA(names(cost_budget))&&!anyDuplicated(names(cost_budget))&&all(nzchar(names(cost_budget)))),
      'cost_budget must be one unnamed number or have unique nonempty dataset names')
    if(length(unique(ds))>1L)p82_need(!is.null(names(cost_budget))&&all(unique(ds)%in%names(cost_budget)),'Multi-dataset cost_budget must be named by dataset')
    if(!is.null(names(cost_budget)))p82_need(setequal(names(cost_budget),unique(ds)),'Named cost_budget must match datasets exactly')
  }
  if(scheduler %in% .prime_cert_schedulers) {
    p82_need(is.null(value_models),'value_models are only used by history_value')
    limit <- if(missing(max_queries)) Inf else max_queries
    return(.prime_cert_start(tasks,ids,ds,budget,cost_budget,threshold,seed,limit,
      allow_full_census,scheduler,task_families))
  }
  p82_need(is.null(task_families),'task_families are only used by deterministic certificate schedulers')
  .prime_scalar(max_queries,'max_queries',1,64,TRUE)
  p82_need(budget<=max_queries,'budget exceeds max_queries')
  s <- .prime_settings(threshold,alpha,fdr_alpha,seed,max_queries)
  st <- lapply(tasks,.prime_new_task,s=s,max_queries=as.integer(max_queries))
  if(!allow_full_census)st <- lapply(st,function(t){t$limit <- min(t$limit,t$public$n-1L);t})
  for(d in sort(unique(ds))) {
    ix <- which(ds==d);cap <- sum(vapply(st[ix],function(t)mean(t$public$cost)*min(t$public$n,budget),numeric(1L)))
    if(!is.null(cost_budget))cap <- if(is.null(names(cost_budget)))cost_budget[1L] else cost_budget[[d]]
    p82_need(length(cap)==1L&&is.finite(cap)&&cap>=0,'Invalid dataset cost budget')
    effective <- scheduler;fallback <- ''
    if(scheduler=='history_value') {
      p82_need(is.list(value_models)&&!is.null(names(value_models))&&all(ids[ix]%in%names(value_models)),'value_models must be named by every task_id')
      for(id in ids[ix]) {
        m <- value_models[[id]];.prime_validate_value_model(m)
        hist <- c(m$training_task_ids,m$selection_task_ids)
        p82_need(!id%in%hist&&! .prime_family(id)%in%.prime_family(hist),'Target task or family overlaps value model history')
      }
      if(any(!vapply(value_models[ids[ix]],function(m)isTRUE(m$usable),logical(1L)))) {
        effective <- 'support';fallback <- 'ONE_OR_MORE_VALUE_MODELS_UNUSABLE_WHOLE_DATASET_SUPPORT_FALLBACK'
      }
    }
    groups[[d]] <- list(indices=ix,cost_budget=cap,cost=0,phase=1L,phase_cost=c(0,0),phase_queries=c(0L,0L),
      pilot_cost=0,finished=FALSE,effective_scheduler=effective,learner_fallback_reason=fallback)
  }
  if(scheduler=='history_value')value_models <- lapply(value_models[ids],function(m)m[c('feature_names','cuts','lookup','intercept','range_min','range_max','usable')])
  cache <- .prime_stats(st)
  tie_hash <- vapply(ids,function(id)p82_seed(s$seed,id,'ALLOCATION_TIE'),integer(1L))
  structure(list(schema='PRIMEeval_0.7_stream_v1',settings=s,tasks=st,groups=groups,budget=as.integer(budget),
    allow_full_census=allow_full_census,scheduler=scheduler,value_models=value_models,pending=NULL,
    query_rows=list(),steps=0L,cache=cache,task_ids=ids,tie_hash=tie_hash),class='prime_audit')
}

.prime_validate_state <- function(state) {
  p82_need(inherits(state,'prime_audit')&&length(state$schema)==1L&&
    state$schema %in% c('PRIMEeval_0.7_stream_v1','PRIMEeval_0.8_cert_v1'),'Invalid or unsupported audit state')
  invisible(TRUE)
}
.prime_stats <- function(tasks) {
  list(used=vapply(tasks,function(t)length(t$seen),integer(1L)),N=vapply(tasks,function(t)t$public$n,integer(1L)),
    limit=vapply(tasks,function(t)t$limit,integer(1L)),cost=vapply(tasks,function(t)if(nrow(t$trace))tail(t$trace$cost,1L) else 0,numeric(1L)),
    mean_cost=vapply(tasks,function(t)mean(t$public$cost),numeric(1L)),initial=vapply(tasks,function(t)mean(t$public$lo),numeric(1L)),
    width=vapply(tasks,function(t)mean(t$public$width),numeric(1L)),maxwidth=vapply(tasks,function(t)max(t$public$width),numeric(1L)),
    score=vapply(tasks,function(t)if(nrow(t$trace))tail(t$trace$screen_score,1L) else -Inf,numeric(1L)),
    lower=vapply(tasks,function(t)if(nrow(t$trace))tail(t$trace$support_lower,1L) else mean(t$public$lo),numeric(1L)),
    support=vapply(tasks,function(t)if(nrow(t$trace))tail(t$trace$support,1L) else FALSE,logical(1L)),
    ever_screen=vapply(tasks,function(t)t$ever_screen,logical(1L)),
    quote=vapply(tasks,function(t)if(length(t$seen)<t$limit)t$public$cost[t$order[length(t$seen)+1L]] else Inf,numeric(1L)))
}

.prime_value_action <- function(t,model,s,quote) {
  if(!is.null(t$value_action))return(t$value_action)
  n <- t$public$n;used <- length(t$seen);tr <- t$trace;old <- max(0L,used-3L)
  h <- p88_horizons(used,t$limit,s);r <- tr[used,,drop=FALSE]
  oldlower <- if(old)tr$support_lower[old] else mean(t$public$lo);oldloge <- if(used>1L)tr$loge[used-1L] else 0
  X <- p88_features(n,used,t$limit,mean(t$public$lo),mean(t$public$width),max(t$public$width),r$support_lower,
    r$PLUGIN_IMPUTE,r$PLUGIN_IMPUTE_se,r$loge,r$ess,max(0,(r$support_lower-oldlower)*n/(used-old)),r$loge-oldloge,h,s$threshold)
  pred <- p88_predict_value(model,X);cost <- quote+(h-1)*mean(t$public$cost)
  list(horizon=h,probability=pred$probability,predicted_cost=cost,log_value=log(pred$probability)-log(cost),outside_fraction=pred$outside_fraction)
}

prime_next <- function(state) {
  .prime_validate_state(state)
  if(identical(state$schema,'PRIMEeval_0.8_cert_v1'))return(.prime_cert_next(state))
  if(!is.null(state$pending))return(list(state=state,request=state$pending$request))
  s <- state$settings
  for(d in names(state$groups)) {
    g <- state$groups[[d]];if(g$finished)next
    ix <- g$indices;tasks <- state$tasks[ix];v <- lapply(state$cache,function(z)z[ix])
    allowed <- which(v$used<v$limit&v$quote<=g$cost_budget-g$cost+1e-8)
    if(!length(allowed)){state$groups[[d]]$finished <- TRUE;next}
    ids <- state$task_ids[ix];hashes <- state$tie_hash[ix]
    pilot <- allowed[v$used[allowed]<min(2L,state$budget)];is_pilot <- length(pilot)>0L
    if(is_pilot)i <- pilot[order(v$used[pilot],hashes[pilot])][1L] else {
      if(g$phase==1L) {
        phasecap <- .5*g$cost_budget;first <- allowed[v$quote[allowed]<=phasecap-g$cost+1e-8]
        if(g$cost>=phasecap-1e-8||!length(first))g$phase <- 2L else allowed <- first
      }
      if(g$phase==1L){priority <- -abs(v$score);priority[v$score>0] <- -Inf} else {
        if(g$effective_scheduler=='support') {
          gain <- pmax(0,(v$lower-v$initial)*v$N);rate <- (v$width+gain)/(2+v$used)
          need <- pmax(0,s$threshold-v$lower)*v$N/pmax(rate,1e-10);priority <- -need*v$mean_cost
          pool <- allowed[v$ever_screen[allowed]&!v$support[allowed]]
          if(length(pool))allowed <- pool else {
            pool <- allowed[!v$support[allowed]]
            if(length(pool)){allowed <- pool;priority <- -abs(v$score)} else priority <- -v$used
          }
        } else {
          pool <- allowed[!v$support[allowed]];priority <- -v$used
          if(length(pool)) {
            allowed <- pool;priority[] <- -Inf
            for(j in allowed) {
              ac <- .prime_value_action(tasks[[j]],state$value_models[[ids[j]]],s,v$quote[j]);state$tasks[[ix[j]]]$value_action <- ac
              ok <- which(ac$predicted_cost<=g$cost_budget-g$cost+1e-8)
              if(length(ok))priority[j] <- max(ac$log_value[ok])
            }
          }
        }
      }
      value <- priority[allowed];value[is.na(value)] <- -Inf;best <- allowed[value==max(value)]
      i <- best[order(v$used[best],hashes[best])][1L]
    }
    idx <- ix[i];t <- state$tasks[[idx]];math <- .prime_task_request(t,s)
    token <- p82_hash(list(state$schema,state$steps,t$task_id,length(t$seen),t$seen,t$y,t$q,math$index,math$q,math$cost))
    request <- data.frame(token=token,task_id=t$task_id,dataset=d,unit_id=t$unit_ids[math$index],index=math$index,
      q=math$q,cost=math$cost,phase=g$phase,stringsAsFactors=FALSE)
    state$groups[[d]] <- g;state$pending <- list(request=request,math=math,task_index=idx,dataset=d,is_pilot=is_pilot)
    return(list(state=state,request=request))
  }
  list(state=state,request=NULL)
}

prime_update <- function(state,request,value) {
  .prime_validate_state(state);p82_need(!is.null(state$pending),'No outstanding query; call prime_next() first')
  pending <- state$pending
  p82_need(is.data.frame(request)&&identical(request,pending$request),'Query request was changed or is stale')
  if(identical(state$schema,'PRIMEeval_0.8_cert_v1'))return(.prime_cert_update(state,pending,value))
  idx <- pending$task_index;old <- state$tasks[[idx]];updated <- .prime_observe(old,pending$math,value,state$settings)
  g <- state$groups[[pending$dataset]];cost <- pending$math$cost
  p82_need(g$cost+cost<=g$cost_budget+1e-6,'Actual cost exceeds dataset budget')
  g$cost <- g$cost+cost;g$phase_cost[g$phase] <- g$phase_cost[g$phase]+cost;g$phase_queries[g$phase] <- g$phase_queries[g$phase]+1L
  if(pending$is_pilot)g$pilot_cost <- g$pilot_cost+cost
  state$tasks[[idx]] <- updated;state$groups[[pending$dataset]] <- g;state$steps <- state$steps+1L
  # Maintain only the queried task's observed state. Public means and tie hashes
  # were computed once in prime_start; no future label is used by this cache.
  n <- length(updated$seen);last <- updated$trace[n,,drop=FALSE]
  state$cache$used[idx] <- n;state$cache$cost[idx] <- last$cost
  state$cache$score[idx] <- last$screen_score;state$cache$lower[idx] <- last$support_lower
  state$cache$support[idx] <- last$support;state$cache$ever_screen[idx] <- updated$ever_screen
  state$cache$quote[idx] <- if(n<updated$limit)updated$public$cost[updated$order[n+1L]] else Inf
  row <- pending$request;row$value <- value;row$task_step <- length(updated$seen);row$total_step <- state$steps
  state$query_rows[[state$steps]] <- row;state$pending <- NULL;state
}

prime_run <- function(state,query,max_steps=Inf) {
  .prime_validate_state(state);p82_need(is.function(query),'query must be a function(task_id, unit_id, dataset)')
  p82_need(is.numeric(max_steps)&&length(max_steps)==1L&&!is.na(max_steps)&&max_steps>=0&&(is.infinite(max_steps)||max_steps==floor(max_steps)),'Invalid max_steps')
  steps <- 0L
  while(steps<max_steps) {
    z <- prime_next(state);state <- z$state;if(is.null(z$request))break
    q <- z$request;value <- query(q$task_id,q$unit_id,q$dataset);state <- prime_update(state,q,value);steps <- steps+1L
  }
  state
}

prime_summary <- function(state) {
  .prime_validate_state(state)
  if(identical(state$schema,'PRIMEeval_0.8_cert_v1'))return(.prime_cert_summary(state))
  s <- state$settings
  out <- do.call(rbind,lapply(state$tasks,function(t) {
    n <- length(t$seen);r <- if(n)t$trace[n,,drop=FALSE] else NULL
    lower <- if(n)r$support_lower else mean(t$public$lo);upper <- (sum(t$y)+sum(t$public$hi[setdiff(seq_len(t$public$n),t$seen)]))/t$public$n
    support <- if(n)r$ever_support else FALSE;loge <- if(n)r$loge else 0;maxloge <- if(n)r$max_loge else 0
    data.frame(task_id=t$task_id,dataset=t$dataset,n_units=t$public$n,used=n,record_cost=if(n)r$cost else 0,
      estimate=if(n)r$PLUGIN_IMPUTE else s$threshold,screen_score=if(n)r$screen_score else -Inf,support_lower=lower,support_upper=upper,
      support_only=support,screen_current=if(n)r$screen_red else FALSE,screen_ever=t$ever_screen,
      formal_current=support||loge>=log(1/s$alpha),formal_anytime=support||maxloge>=log(1/s$alpha),formal_ebh=FALSE,
      current_loge=loge,full_census=n==t$public$n,history_scope=t$history_scope,
      inferential_scope='SUPPORT_IS_DETERMINISTIC_GIVEN_VALID_BOUNDS_EBH_AND_EVALUES_ARE_DEVELOPMENT_READOUTS_NO_GLOBAL_POST_SELECTION_GUARANTEE',stringsAsFactors=FALSE)
  }))
  for(d in unique(out$dataset)) {
    ix <- which(out$dataset==d);cur <- out$current_loge[ix];cur[out$support_only[ix]] <- Inf
    out$formal_ebh[ix] <- p82_ebh(cur,s$fdr_alpha)
  }
  # This direct bound certificate is meaningful even before any query. Keep the
  # legacy support_only / formal_* flags unchanged for exact experiment replay.
  out$bound_certificate <- out$support_lower>s$threshold+1e-12
  out$requested_scheduler <- state$scheduler
  out$effective_scheduler <- vapply(out$dataset,function(d)state$groups[[d]]$effective_scheduler,character(1L))
  out$learner_fallback_reason <- vapply(out$dataset,function(d)state$groups[[d]]$learner_fallback_reason,character(1L))
  rownames(out) <- NULL;out
}
prime_trace <- function(state,task_id) {
  .prime_validate_state(state);ids <- vapply(state$tasks,function(t)t$task_id,character(1L));i <- match(task_id,ids)
  p82_need(length(i)==1L&&!is.na(i),'Unknown task_id');state$tasks[[i]]$trace
}
prime_log <- function(state) {
  .prime_validate_state(state)
  if(!length(state$query_rows))return(data.frame())
  out <- do.call(rbind,state$query_rows);rownames(out) <- NULL;out
}
