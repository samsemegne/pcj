

is.pcj_prior_predictive1 = function(x) {
  return(inherits(x, "pcj_prior_predictive1", FALSE))
}


new_prior_predictive = function(
    pci_parameters,
    prior_mu,
    prior_sigma,
    prior_predictive_parameters,
    evaluate = FALSE
  )
{
  stopifnot(exprs = {
    is.pci_parameters1(pci_parameters)
    is_valid__pci_params(pci_parameters)
    is_pcj_prior(prior_mu)
    is_pcj_prior(prior_sigma)
    !(is_pcj_single_point_prior(prior_mu) &&
        is_pcj_single_point_prior(prior_sigma))
    is.prior_predictive_parameters(prior_predictive_parameters)
    is_valid__prior_predictive_parameters(prior_predictive_parameters)
    vek::is_lgl_vec_x1(evaluate)
  })

  if (is_pcj_single_point_prior(prior_sigma))
    stopifnot(prior_sigma$value > 0L)

  seed = prior_predictive_parameters$seed
  rng_kind = prior_predictive_parameters$rng_kind
  rng_version = prior_predictive_parameters$rng_version
  sample_size = prior_predictive_parameters$sample_size

  RNGversion(rng_version)
  set.seed(seed, kind = rng_kind)
  stopifnot(RNGkind()[1L] == rng_kind)

  obj = list(condition = list(), output = list(), result = list())
  content = list(
    pci_parameters = pci_parameters,
    prior_mu = store_prior(prior_mu),
    prior_sigma = store_prior(prior_sigma),
    prior_predictive_parameters = prior_predictive_parameters,
    r_version = R.version$version.string,
    evaluate = evaluate
  )

  if (!evaluate) {
    obj$result = content
    obj = as.environment(obj)
    class(obj) = "pcj_prior_predictive1"
    lockEnvironment(obj, bindings = TRUE)
    return(obj)
  }

  target = pci_parameters$target
  lsl = pci_parameters$lsl
  usl = pci_parameters$usl
  dl = pci_parameters$dl

  f = function() {
    RNGversion(rng_version)
    set.seed(seed, kind = rng_kind)
    stopifnot(RNGkind()[1L] == rng_kind)

    df = new_df(
      mu = pcj_rng(prior_mu, n = sample_size),
      sigma = pcj_rng(prior_sigma, n = sample_size)
    )

    df$data = stats::rnorm(sample_size, df$mu, df$sigma)

    if ("C_p" %in% pci_parameters$capability_indices)
      df$C_p =  pci::C_p(df$sigma, lsl, usl, dl)

    if ("C_pl" %in% pci_parameters$capability_indices)
      df$C_pl = pci::C_pl(df$mu, df$sigma, lsl, dl / 2L)

    if ("C_pu" %in% pci_parameters$capability_indices)
      df$C_pu = pci::C_pu(df$mu, df$sigma, usl, dl / 2L)

    if ("C_pk" %in% pci_parameters$capability_indices)
      df$C_pk = pci::C_pk(df$mu, df$sigma, lsl, usl, dl)

    if ("C_pm" %in% pci_parameters$capability_indices)
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
  class(obj) = "pcj_prior_predictive1"

  lockEnvironment(obj, bindings = TRUE)

  return(obj)
}


#' @export
get_error.pcj_prior_predictive1 = get_error_
#' @export
get_warning.pcj_prior_predictive1 = get_warning_
#' @export
get_message.pcj_prior_predictive1 = get_message_
#' @export
get_condition.pcj_prior_predictive1 = get_condition_
#' @export
get_result.pcj_prior_predictive1 = get_result_
#' @export
get_output.pcj_prior_predictive1 = get_output_


#' @export
variable.names.pcj_prior_predictive1 = function(object, distribution) {
  stopifnot(exprs = {
    is.pcj_prior_predictive1(object)
    is.pci_parameters1(get_result(object)$pci_parameters)
    vek::is_chr_vec_xb1(distribution)
    distribution %in% c("prior", "prior_predictive")
  })

  if (distribution == "prior") {
    return(get_model1_prior_var_name())
  } else if (distribution == "prior_predictive") {
    return(c(
      get_result(object)$pci_parameters$capability_indices,
      get_nonconformance_var_name()
    ))
  } else {
    stop()
  }
}


#' @export
get_prior.pcj_prior_predictive1 = function(object, x) {
  stopifnot(exprs = {
    is.pcj_prior_predictive1(object)
    vek::is_chr_vec_xb1(x)
    x %in% stats::variable.names(object, "prior")
  })

  key = sprintf("prior_%s", x)
  return(get_result(object)[[key]])
}


#' @export
summary.pcj_prior_predictive1 = function(object, stat, statistics) {
  stopifnot(exprs = {
    is.pcj_prior_predictive1(object)
    !has_error(check_stat(stat, "stat"))
    vek::is_chr_vec_xb(statistics)
    is_valid_summary_stats_string(statistics)
  })

  var_name = stats::variable.names(object, "prior_predictive")

  if (has_error(object)) {
    cols = c(c("what", "distribution"), statistics)

    var_name = stats::variable.names(object, "prior_predictive")

    df = matrix(NaN, nrow = length(var_name), ncol = length(cols))
    colnames(df) = cols
    df$what = var_name
    df$distribution = "prior_predictive"

    res = new_pcj_result(df, get_condition(object), get_output(object))
    class(res) = "pcj_prior_predictive1_summary"
    return(res)
  }

  res = lapply(var_name, \(x) {
    samples = get_sample(object, x)
    stat_res = obtain_stat_result(samples, stat)
    at = statistics[statistics != "sd"]

    if (has_error(stat_res)) {
      cond = list(simpleError("Invalid stat result"))
      val = rep_len(NaN, length(at))
      names(val) = at
      at_res = new_pcj_result(val, cond, list())
      sd_res = new_pcj_result(c(sd = NaN), cond, list())
    } else {
      at_res = get_at(at, stat_res)
      sd_res = obtain_stat_sd(stat_res)
    }

    return(list(
      what = x,
      at = at_res,
      sd = sd_res,
      stat = stat_res
    ))
  })

  get_stat_val = \(k, stat_name) {
    if (stat_name == "sd")
      return(get_result(k$sd))
    else
      return(get_result(k$at)[stat_name])
  }

  df_rows = lapply(res, \(k) {
    at = get_result(k$at)

    dfstatcols = lapply(statistics, \(x) {
      val = get_stat_val(k, x)
      df__ = new_df(foo = val)
      colnames(df__) = x
      stopifnot(exprs = {
        ncol(df__) == 1L
        nrow(df__) == 1L
      })
      return(df__)
    })

    df_ = do.call(cbind, dfstatcols)
    df_ = cbind(new_df(what = k$what, distribution = "prior_predictive"), df_)
    return(df_)
  })

  df = do.call(rbind.data.frame, df_rows) # TODO rbind.data.frame set params
  row.names(df) = 1:nrow(df)

  cond_ = lapply(res, \(k) {
    return(c(
      get_condition(k$at),
      get_condition(k$sd),
      get_condition(k$stat)
    ))
  })

  out_ = lapply(res, \(k) {
    return(c(
      get_output(k$at),
      get_output(k$sd),
      get_output(k$stat)
    ))
  })

  cond = do.call(c, cond_)
  cond = c(get_condition(object), cond)
  out = do.call(c, out_)
  out = c(get_output(object), out)

  summary_obj = new_pcj_result(df, cond, out)
  class(summary_obj) = "pcj_prior_predictive1_summary"

  return(summary_obj)
}


#' @export
get_error.pcj_prior_predictive1_summary = get_error_
#' @export
get_warning.pcj_prior_predictive1_summary = get_warning_
#' @export
get_message.pcj_prior_predictive1_summary = get_message_
#' @export
get_condition.pcj_prior_predictive1_summary = get_condition_
#' @export
get_result.pcj_prior_predictive1_summary = get_result_
#' @export
get_output.pcj_prior_predictive1_summary = get_output_


#' @export
print.pcj_prior_predictive1_summary = function(object) {
  stopifnot(is_of_mono_class(object, "pcj_prior_predictive1_summary"))

  throw_first_error(object)
  signal_warnings(object)

  print.data.frame(get_result(object))

  return(invisible(object))
}


#' @export
get_sample.pcj_prior_predictive1 = function(object, x) {
  stopifnot(exprs = {
    is.pcj_prior_predictive1(object)
    vek::is_chr_vec_xb1(x)
    x %in% stats::variable.names(object, "prior_predictive")
  })

  return(get_result(object)$prior_predictive_sample[, x]) # TODO check indexing behavior
}


#' @export
probability.pcj_prior_predictive1 = function(object, what, value, stat, ...) {
  stopifnot(exprs = {
    is.pcj_prior_predictive1(object)
    vek::is_chr_vec_xb1(what)
    vek::is_num_vec_z(value) || vek::is_chr_vec(value)
    what %in% stats::variable.names(object, "prior_predictive")
    is_empty(check_stat(stat, "stat"))
  })

  samples = get_sample(object, what)
  stat_res = obtain_stat_result(samples, stat)
  prob_res = obtain_stat_probability(value, stat_res, ...)

  throw_first_error(prob_res)
  signal_warnings(prob_res)
  return(get_result(prob_res))
}


#' @export
mean.pcj_prior_predictive1 = function(object, what, stat, ...) {
  stopifnot(exprs = {
    is.pcj_prior_predictive1(object)
    vek::is_chr_vec_xb1(what)
    what %in% stats::variable.names(object, "prior_predictive")
    is_empty(check_stat(stat, "stat"))
  })

  samples = get_sample(object, what)
  stat_res = obtain_stat_result(samples, stat)
  mean_res = obtain_stat_mean(stat_res, ...)

  throw_first_error(mean_res)
  signal_warnings(mean_res)
  return(get_result(mean_res))
}


#' @export
median.pcj_prior_predictive1 = function(object, what, stat, ...) {
  stopifnot(exprs = {
    is.pcj_prior_predictive1(object)
    vek::is_chr_vec_xb1(what)
    what %in% stats::variable.names(object, "prior_predictive")
    is_empty(check_stat(stat, "stat"))
  })

  samples = get_sample(object, what)
  stat_res = obtain_stat_result(samples, stat)
  median_res = obtain_stat_median(stat_res, ...)

  throw_first_error(median_res)
  signal_warnings(median_res)
  return(get_result(median_res))
}


#' @export
quantile.pcj_prior_predictive1 = function(object, what, value, stat, ...) {
  stopifnot(exprs = {
    is.pcj_prior_predictive1(object)
    vek::is_chr_vec_xb1(what)
    what %in% stats::variable.names(object, "prior_predictive")
    is_empty(check_stat(stat, "stat"))
  })

  samples = get_sample(object, what)
  stat_res = obtain_stat_result(samples, stat)
  quantile_res = obtain_stat_quantile(stat_res, ...)

  throw_first_error(quantile_res)
  signal_warnings(quantile_res)
  return(get_result(quantile_res))
}


