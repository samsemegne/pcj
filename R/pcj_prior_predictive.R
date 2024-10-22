

is.pcj_prior_predictive = function(x) inherits(x, "pcj_prior_predictive")


new_prior_predictive = function(
    pci_params,
    prior_mu,
    prior_sigma,
    prior_predictive_params,
    evaluate = FALSE
  )
{
  stopifnot(exprs = {
    is.pci_params(pci_params)
    is_valid__pci_params(pci_params)
    is_pcj_prior(prior_mu)
    is_pcj_prior(prior_sigma)
    !(is_pcj_point_prior(prior_mu) && is_pcj_point_prior(prior_sigma))
    is.prior_predictive_params(prior_predictive_params)
    is_valid__prior_predictive_params(prior_predictive_params)
    vek::is_lgl_vec_x1(evaluate)
  })

  if (is_pcj_point_prior(prior_sigma))
    stopifnot(prior_sigma > 0L)

  seed = prior_predictive_params$seed
  rng_kind = prior_predictive_params$rng_kind
  rng_version = prior_predictive_params$rng_version
  sample_size = prior_predictive_params$sample_size

  RNGversion(rng_version)
  set.seed(seed, kind = rng_kind)
  stopifnot(RNGkind()[1L] == rng_kind)

  obj = list(condition = list(), output = list(), result = list())
  content = list(
    pci_params = pci_params,
    prior_mu = store_prior(prior_mu),
    prior_sigma = store_prior(prior_sigma),
    prior_predictive_params = prior_predictive_params,
    r_version = R.version$version.string,
    evaluate = evaluate
  )

  if (!evaluate) {
    obj$result = content
    obj = as.environment(obj)
    class(obj) = "pcj_prior_predictive"
    lockEnvironment(obj, bindings = TRUE)
    return(obj)
  }

  target = pci_params$target
  lsl = pci_params$lsl
  usl = pci_params$usl
  dl = pci_params$dl

  f = function() {
    RNGversion(rng_version)
    set.seed(seed, kind = rng_kind)
    stopifnot(RNGkind()[1L] == rng_kind)

    df = new_df(
      mu = pcj_rng(prior_mu, n = sample_size),
      sigma = pcj_rng(prior_sigma, n = sample_size)
    )

    df$data = stats::rnorm(sample_size, df$mu, df$sigma)

    if ("C_p" %in% pci_params$capability_indices)
      df$C_p =  pci::C_p(df$sigma, lsl, usl, dl)

    if ("C_pl" %in% pci_params$capability_indices)
      df$C_pl = pci::C_pl(df$mu, df$sigma, lsl, dl / 2L)

    if ("C_pu" %in% pci_params$capability_indices)
      df$C_pu = pci::C_pu(df$mu, df$sigma, usl, dl / 2L)

    if ("C_pk" %in% pci_params$capability_indices)
      df$C_pk = pci::C_pk(df$mu, df$sigma, lsl, usl, dl)

    if ("C_pm" %in% pci_params$capability_indices)
      df$C_pm = pci::C_pm(df$mu, df$sigma, target, lsl, usl, dl)

    df$p_nonconformance = 1L - bqc__norm__prob(df$mu, df$sigma, lsl, usl)
    df$p_nonconformance_above = bqc__norm__prob(df$mu, df$sigma, usl, Inf)
    df$p_nonconformance_below = bqc__norm__prob(df$mu, df$sigma, -Inf, lsl)

    df = subset(df, select = -c(mu, sigma))

    stopifnot(exprs = {
      nrow(df) == sample_size
      all(apply(df, 2L, vek::is_num_vec_xyz), na.rm = FALSE)
    })

    return(list(prior_predictive_sample = df))
  }

  obj = pcj_safely(f())
  class(obj) = NULL
  obj$result = c(obj$result, content)

  obj = as.environment(obj)
  class(obj) = "pcj_prior_predictive"

  lockEnvironment(obj, bindings = TRUE)

  return(obj)
}



get_error.pcj_prior_predictive = get_error_

get_warning.pcj_prior_predictive = get_warning_

get_message.pcj_prior_predictive = get_message_

get_condition.pcj_prior_predictive = get_condition_

get_result.pcj_prior_predictive = get_result_


variable.names.pcj_prior_predictive = function(object, distribution) {
  stopifnot(exprs = {
    is.pcj_prior_predictive(object)
    is.pci_params(get_result(object)$pci_params)
    vek::is_chr_vec_xb1(distribution)
    distribution %in% c("prior", "prior_predictive")
  })

  if (distribution == "prior") {
    return(get_model1_prior_var_name())
  } else if (distribution == "prior_predictive") {
    return(c(
      get_result(object)$pci_params$capability_indices,
      get_nonconformance_var_name()
    ))
  } else {
    stop()
  }
}


summary.pcj_prior_predictive = function(object, stat = NULL) {
  stopifnot(exprs = {
    is.pcj_prior_predictive(object)
    is_empty(check_stat(stat, "stat"))
  })

  var_name = variable.names(object, "prior_predictive")

  if (has_error(object)) {
    cols = c("x", "distribution", "mean", "sd", "q.025", "q.25", "q.5",
             "q.75", "q.975")

    var_name = variable.names(object, "prior_predictive")

    df = matrix(NaN, nrow = length(var_name), ncol = length(cols))
    colnames(df) = cols
    df$x = var_name
    df$distribution = "prior_predictive"

    res = list(
      condition = get_condition(object),
      output = list(),
      result = df
    )

    class(res) = "pcj_prior_predictive_summary"
    return(res)
  }

  res = lapply(var_name, \(x) {
    samples = get_sample(object, x)

    stat_res_ = pcj_safely(default_stats(samples))
    stat_res = recursive_unclass(get_result(stat_res_), 5L) # TODO
    stat_check = check_stat_result(stat_res, "stat")

    at = c("mean", "q.025", "q.25", "q.5", "q.75", "q.975")
    if (is_empty(stat_check)) {
      at_res = get_at(at, samples, stat_res_)
      sd_res = obtain_stat_sd(samples, stat_res)
    } else {
      cond = list(simpleError("Invalid stat result"))
      val = rep_len(NaN, length(at))
      names(val) = at

      at_res = structure(list(
        condition = cond,
        output = list(),
        result = val
      ), class = "pcj_result")

      sd_res = structure(list(
        condition = cond,
        output = list(),
        result = NaN
      ), class = "pcj_result")
    }

    return(list(
      x = x,
      at = at_res,
      sd = sd_res,
      stat = stat_res_,
      stat_check = stat_check
    ))
  })

  df_rows = lapply(res, \(k) {
    at = get_result(k$at)
    return(new_df(
      x = k$x,
      distribution = "prior_predictive",
      mean =  at["mean"],
      sd =    get_result(k$sd),
      q.025 = at["q.025"],
      q.25 =  at["q.25"],
      q.5 =   at["q.5"],
      q.75 =  at["q.75"],
      q.975 = at["q.975"]
    ))
  })

  df = do.call(rbind.data.frame, df_rows) # TODO rbind.data.frame set params
  row.names(df) = 1:nrow(df)

  cond_ = lapply(res, \(k) {
    return(c(
      get_condition(k$at),
      get_condition(k$sd),
      get_condition(k$stat),
      k$stat_check
    ))
  })

  cond = list()
  for (k in cond_)
    cond = c(cond, k)

  cond = c(get_condition(object), cond)

  summary_obj = list(
    condition = cond,
    output = list(),
    result = df
  )

  class(summary_obj) = "pcj_prior_predictive_summary"

  return(summary_obj)
}



get_error.pcj_prior_predictive_summary = get_error_

get_warning.pcj_prior_predictive_summary = get_warning_

get_message.pcj_prior_predictive_summary = get_message_

get_condition.pcj_prior_predictive_summary = get_condition_

get_result.pcj_prior_predictive_summary = get_result_


print.pcj_prior_predictive_summary = function(object, ...) {
  stopifnot(is_of_mono_class(object, "pcj_prior_predictive_summary"))

  if (has_error(object))
    stop(get_error(object)[[1L]])

  if (has_warning(object)) {
    for (w in get_warning(object))
      warning(w)
  }

  print.data.frame(get_result(object), ...)

  return(invisible(object))
}


get_sample.pcj_prior_predictive = function(object, x) {
  stopifnot(exprs = {
    is.pcj_prior_predictive(object)
    vek::is_chr_vec_xb1(x)
    x %in% variable.names(object, "prior_predictive")
  })

  return(get_result(object)$prior_predictive_sample[, x]) # TODO check indexing behavior
}


probability.pcj_prior_predictive = function(object, x, value, stat = NULL) {
  stopifnot(exprs = {
    is.pcj_prior_predictive(object)
    #vek::is_num_vec(value)
    vek::is_chr_vec_xb1(x)
    x %in% variable.names(object, "prior_predictive")
    is_empty(check_stat(stat, "stat"))
  })

  samples = get_sample(object, x)
  stat_res = pcj_safely(stat(samples))

  return(stat_probability(samples, value, stat_res))
}


#mean.pcj_prior_predictive = function(object, x, stat = NULL) {
#  stopifnot(exprs = {
#    is.pcj_prior_predictive(object)
#    vek::is_chr_vec_xb1(x)
#    x %in% variable.names(object, "prior_predictive")
#  })
#
#  if (is.null(stat))
#    stat = default_stats
#
#  stat_check = check_stat(stat, "stat")
#  if (!is_empty(stat_check))
#    stop(stat_check[[1L]])
#
#  samples = get_sample(object, x)
#  stat_res = obtain_stat_result(samples, stat)
#
#  return(stat_mode_("mean", samples, stat_res))
#}


#median.pcj_prior_predictive = function(object, x, stat = NULL) {
#  stopifnot(exprs = {
#    is.pcj_prior_predictive(object)
#    vek::is_chr_vec_xb1(x)
#    x %in% variable.names(object, "prior_predictive")
#  })
#
#  if (is.null(stat))
#    stat = default_stats
#
#  stat_check = check_stat(stat, "stat")
#  if (!is_empty(stat_check))
#    stop(stat_check[[1L]])
#
#  samples = get_sample(object, x)
#  stat_res = obtain_stat_result(samples, stat)
#
#  return(stat_mode_("median", samples, stat_res))
#}


quantile.pcj_prior_predictive = function(object, x, value, stat = NULL) {
  stopifnot(exprs = {
    is.pcj_prior_predictive(object)
    vek::is_chr_vec_xb1(x)
    x %in% variable.names(object, "prior_predictive")
    is_empty(check_stat(stat, "stat"))
  })

  samples = get_sample(object, x)
  stat_res = pcj_safely(stat(samples))

  return(stat_quantile_(value, samples, stat_res))
}


