

is.pcj_model1 = function(x) return(is_of_mono_class(x, "pcj_model1"))


new_pcj_model1 = function(
    data,
    pci_parameters,
    prior_mu,
    prior_sigma,
    sampler_parameters,
    evaluate = FALSE
  )
{
  stopifnot(exprs = {
    vek::is_num_vec_xyz(data)
    length(data) > 1L
    is.pci_parameters1(pci_parameters)
    is_valid__pci_params(pci_parameters)
    is_pcj_prior(prior_mu)
    is_pcj_prior(prior_sigma)
    !(is_pcj_single_point_prior(prior_mu) && is_pcj_single_point_prior(prior_sigma))
    is.rjags_parameters(sampler_parameters)
    is_valid__rjags_params(sampler_parameters)
    all(names(sampler_parameters$initial_value) %in% c("mu", "sigma"),
        na.rm = FALSE)
    vek::is_lgl_vec_x1(evaluate)
  })

  if (is_pcj_single_point_prior(prior_sigma))
    stopifnot(prior_sigma$value > 0L)

  obj = list(condition = list(), output = list(), result = NULL)
  content = list(
    data = data,
    pci_parameters = pci_parameters,
    sampler_parameters = sampler_parameters,
    sample_size = length(data),
    prior_mu = store_prior(prior_mu),
    prior_sigma = store_prior(prior_sigma),
    r_version = R.version$version.string,
    evaluate = evaluate
  )

  if (!evaluate) {
    obj$result = content
    obj = as.environment(obj)
    class(obj) = "pcj_model1"
    lockEnvironment(obj, bindings = TRUE)
    return(obj)
  }

  # Specify the model.
  model_str = get_model1_str(pci_parameters$capability_indices)
  pci_args = lapply(pci_parameters[-1L], as.character)
  prior_args = list(
    prior_mu = prior_to_jags(prior_mu),
    prior_sigma = prior_to_jags(prior_sigma)
  )

  model_str = weld(model_str, c(prior_args, pci_args))
  monitor = get_monitor_var() |> c(pci_parameters$capability_indices)
  inits = list(
    .RNG.name = sampler_parameters$rng_kind,
    .RNG.seed = sampler_parameters$seed
  )

  if ("mu" %in% names(sampler_parameters$initial_value)) {
    if (!is_pcj_single_point_prior(prior_mu))
      inits$mu = sampler_parameters$initial_value$mu
  }

  if ("sigma" %in% names(sampler_parameters$initial_value)) {
    if (!is_pcj_single_point_prior(prior_sigma))
      inits$sigma = sampler_parameters$initial_value$sigma
  }

  f = function() {
    model = rjags::jags.model(
      file = textConnection(model_str),
      data = list(x = data, N = length(data)),
      inits = inits,
      n.adapt = 0L,
      n.chain = sampler_parameters$n_chains,
      quiet = TRUE
    ) # rjags::jags

    stats::update(model, n.iter = sampler_parameters$burnin)
    samples = rjags::coda.samples(
      model = model,
      variable.names = monitor,
      n.iter = sampler_parameters$sample_size,
      thin = sampler_parameters$thin,
      na.rm = FALSE,
      progress.bar = "none"
    ) # coda::mcmc.list

    return(list(
      fit = model,
      samples = samples
    ))
  }

  # Estimate the model.
  obj = pcj_safely(f())

  if (!has_error(obj))
    obj$result = c(get_result(obj), content)
  else
    obj$result = content

  obj = as.environment(obj)

  class(obj) = "pcj_model1"

  lockEnvironment(obj, bindings = TRUE)

  return(obj)
}


#' @export
get_error.pcj_model1 = get_error_
#' @export
get_warning.pcj_model1 = get_warning_
#' @export
get_message.pcj_model1 = get_message_
#' @export
get_condition.pcj_model1 = get_condition_
#' @export
get_result.pcj_model1 = get_result_
#' @export
get_output.pcj_model1 = get_output_


get_model1_str = function(capability_indices) {
  stopifnot(exprs = {
    vek::is_chr_vec_xb(capability_indices)
    length(capability_indices) > 0L
    all(capability_indices %in% get_supported_pci(), na.rm = FALSE)
    length(unique(capability_indices)) == length(capability_indices)
  })

  x = "
  model {
    # Priors
    mu {{prior_mu}}
    sigma {{prior_sigma}}

    # Variables
    target = {{target}}
    lsl = {{lsl}}
    usl = {{usl}}
    dl = {{dl}}
    l = dl / 2
    precision = 1 / (sigma * sigma)

    # Likelihood
    for (i in 1:N) {
      x[i] ~ dnorm(mu, precision)
    }

    # Capability indices
    {{capability_indices}}

    # Nonconformance
    z_value_lsl = (lsl - mu) / sigma
    z_value_usl = (usl - mu) / sigma
    p_nonconformance_below = phi(z_value_lsl)
    p_nonconformance_above = 1 - phi(z_value_usl)
    p_nonconformance = p_nonconformance_below + p_nonconformance_above
  }
  "

  pci_equations = lapply(capability_indices, \(k) {
    return(sprintf("%s = %s", k, pci::pci_info[k, "expr_r"]))
  }) |>
    paste0(collapse = "\n", recycle0 = FALSE)

  x = weld(x, list(capability_indices = pci_equations))

  return(x)
}


get_model1_prior_var_name = function() return(c("mu", "sigma"))


get_monitor_var = function() {
  return(c(get_model1_prior_var_name(), get_nonconformance_var_name()))
}


get_supported_pci = function() {
  return(c("C_p", "C_pl", "C_pu", "C_pk", "C_pm"))
}


get_nonconformance_var_name = function() {
  return(c("p_nonconformance", "p_nonconformance_below",
           "p_nonconformance_above"))
}


#' @export
get_data.pcj_model1 = function(object) {
  stopifnot(is.pcj_model1(object))
  return(get_result(object)$data)
}


#' @export
variable.names.pcj_model1 = function(object, distribution) {
  stopifnot(exprs = {
    is.pcj_model1(object)
    is.pci_parameters1(get_result(object)$pci_parameters)
    vek::is_chr_vec_xb1(distribution)
    distribution %in% c("prior", "posterior")
  })

  if (distribution == "prior") {
    return(get_model1_prior_var_name())
  } else if (distribution == "posterior") {
    return(c(
      get_model1_prior_var_name(),
      get_result(object)$pci_parameters$capability_indices,
      get_nonconformance_var_name()
    ))
  } else {
    stop()
  }
}


#' @export
get_sample.pcj_model1 = function(object, x, chain) {
  stopifnot(exprs = {
    is.pcj_model1(object)
    vek::is_chr_vec_xb1(x)
    vek::is_int_vec_x1(chain) || vek::is_chr_vec_xb1(chain)
  })

  if (vek::is_int_vec_x1(chain)) {
    n_chain = get_result(object)$fit$nchain()

    stopifnot(exprs = {
      all(chain > 0L, na.rm = FALSE)
      all(chain <= n_chain, na.rm = FALSE)
      #length(unique(chain)) == length(chain)
    })

    samples = get_result(object)$samples[[chain]][, x] |> # check indexing behavior
      unclass() # TODO unclass earlier

    attributes(samples) = NULL
    return(samples)
  }
  else if (vek::is_chr_vec_xb1(chain)) {
    stopifnot(chain == "all")
    samples = get_result(object)$samples |>
      lapply(\(k) unclass(k[, x])) |>
      unlist(FALSE, FALSE)

    attributes(samples) = NULL
    return(samples)
  } else {
    stop()
  }
}


#' @export
get_prior.pcj_model1 = function(object, x) {
  stopifnot(exprs = {
    is.pcj_model1(object)
    vek::is_chr_vec_xb1(x)
    x %in% stats::variable.names(object, "prior")
  })

  key = sprintf("prior_%s", x)
  return(get_result(object)[[key]])
}


#' @export
summary.pcj_model1 = function(object, stat, statistics) {
  stopifnot(exprs = {
    is.pcj_model1(object)
    !has_error(check_stat(stat, "stat"))
    vek::is_chr_vec_x(statistics)
    is_valid_summary_stats_string(statistics)
  })

  var_name = stats::variable.names(object, "posterior")

  if (has_error(object)) {
    cols = c(c("what", "distribution"), statistics)

    var_name = stats::variable.names(object, "posterior")

    df = matrix(NaN, nrow = length(var_name), ncol = length(cols))
    colnames(df) = cols
    df$what = var_name
    df$distribution = "posterior"

    res = new_pcj_result(df, get_condition(object), get_output(object))
    class(res) = "pcj_model_summary"
    return(res)
  }

  res = lapply(var_name, \(x) {

    at = statistics[statistics != "sd"]

    prior = NULL
    if (x %in% stats::variable.names(object, "prior"))
      prior = new_pcj_distribution(get_prior(object, x))

    if (!is.null(prior) && is_pcj_single_point_prior(prior)) {
      # Note. Assumes the only supported statistics are: mean, median,
      # quantiles, and sd. It's important to update this piece of code if and
      # when new statistics are added.
      at_val = rep_len(prior$value, length(at))
      names(at_val) = at
      at_res = new_pcj_result(at_val)
      sd_res = new_pcj_result(c(sd = 0L))
      stat_res = new_pcj_result(NULL)

    } else {
      samples = get_sample(object, x, "all")
      stat_res = obtain_stat_result(samples, stat)

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
    df_ = cbind(new_df(what = k$what, distribution = "posterior"), df_)
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
  class(summary_obj) = "pcj_model_summary"
  return(summary_obj)
}


#' @export
get_error.pcj_model_summary = get_error_
#' @export
get_warning.pcj_model_summary = get_warning_
#' @export
get_message.pcj_model_summary = get_message_
#' @export
get_condition.pcj_model_summary = get_condition_
#' @export
get_result.pcj_model_summary = get_result_
#' @export
get_output.pcj_model_summary = get_output_


#' @export
print.pcj_model_summary = function(object) {
  stopifnot(is_of_mono_class(object, "pcj_model_summary"))

  throw_first_error(object)
  signal_warnings(object)

  print.data.frame(get_result(object))

  return(invisible(object))
}


prior_to_jags = function(x) {
  stopifnot(is_of_mono_class(x, "pcj_distribution"))
  if (is.pcj_jags_distribution(x$value))
    return(sprintf("%s %s", "~", pcj_jags_distribution_to_jags(x$value)))
  else if (is_pcj_single_point_prior(x))
    return(sprintf("%s %s", "=", as.character(x$value)))
  else
    stop()
}


store_prior = function(x) {
  stopifnot(is_of_mono_class(x, "pcj_distribution"))
  if (is.pcj_jags_distribution(x$value))
    return(pcj_jags_distribution_to_jags(x$value))
  else if (is_pcj_single_point_prior(x))
    return(x$value)
  else
    stop()
}


#' @export
probability.pcj_model1 = function(object, what, value, ..., stat = NULL) {
  stopifnot(exprs = {
    is.pcj_model1(object)
    vek::is_chr_vec_xb1(what)
    vek::is_num_vec(value) || vek::is_chr_vec(value)
    what %in% stats::variable.names(object, "posterior")
    !has_error(check_stat(stat, "stat"))
  })

  if (what %in% stats::variable.names(object, "prior")) {
    prior = new_pcj_distribution(get_prior(object, what))
    if (is_pcj_single_point_prior(prior)) {
      return(point_prior_probability(prior, value))
    }
  }

  samples = get_sample(object, what, "all")
  stat_res = obtain_stat_result(samples, stat)
  prob_res = obtain_stat_probability(value, stat_res, ...)

  throw_first_error(prob_res)
  signal_warnings(prob_res)
  return(get_result(prob_res))
}


point_prior_probability = function(prior, value) {
  stopifnot(exprs = {
    is_of_mono_class(prior, "pcj_distribution")
    is_pcj_single_point_prior(prior)
    vek::is_num_vec(value) || vek::is_chr_vec_b(value)
  })

  if (vek::is_num_vec(value)) {
    prob = ifelse(value == prior$value, 1., 0.)

    # Preserve NaN values found in "values".
    if (anyNA(value, recursive = FALSE)) {
      prob[is.na(value)] = value[is.na(value)]
    }

    # Inf values in "values" yield NaN values.
    if (any(is.infinite(value), na.rm = FALSE)) {
      prob[is.infinite(value)] = NaN
    }
  } else if (vek::is_chr_vec_b(value)) {
    interval = lapply(value[!is.na(value)], inequality_to_interval)
    is_failed = sapply_(interval, is.null)
    if (any(is_failed, na.rm = FALSE))
      stop("Failed to parse some strings to intervals")

    is_in = sapply_(interval, \(x) {
      return(is_in_interval(x, prior$value))
    })

    stopifnot(vek::is_lgl_vec_x(is_in))
    prob = rep_len(0., length(value))
    prob[is.na(value)] = NA_real_
    prob[!is.na(value)] = ifelse(is_in, 1., 0.)
  }

  stopifnot(exprs = {
    vek::is_dbl_vec_z(prob)
    all(prob >= 0L, na.rm = TRUE)
    all(prob <= 1L, na.rm = TRUE)
  })

  return(prob)
}


#' @export
mean.pcj_model1 = function(object, what, ..., stat = NULL) {
  stopifnot(exprs = {
    is.pcj_model1(object)
    vek::is_chr_vec_xb1(what)
    what %in% stats::variable.names(object, "posterior")
    !has_error(check_stat(stat, "stat"))
  })

  if (what %in% stats::variable.names(object, "prior")) {
    prior = new_pcj_distribution(get_prior(object, what))
    if (is_pcj_single_point_prior(prior)) {
      return(prior$value)
    }
  }

  samples = get_sample(object, what, "all")
  stat_res = obtain_stat_result(samples, stat)
  mean_res = obtain_stat_mean(stat_res, ...)

  throw_first_error(mean_res)
  signal_warnings(mean_res)
  return(get_result(mean_res))
}


#' @export
median.pcj_model1 = function(object, what, ..., stat = NULL) {
  stopifnot(exprs = {
    is.pcj_model1(object)
    vek::is_chr_vec_xb1(what)
    what %in% stats::variable.names(object, "posterior")
    !has_error(check_stat(stat, "stat"))
  })

  if (what %in% stats::variable.names(object, "prior")) {
    prior = new_pcj_distribution(get_prior(object, what))
    if (is_pcj_single_point_prior(prior)) {
      return(prior$value)
    }
  }

  samples = get_sample(object, what, "all")
  stat_res = obtain_stat_result(samples, stat)
  median_res = obtain_stat_median(stat_res, ...)

  throw_first_error(median_res)
  signal_warnings(median_res)
  return(get_result(median_res))
}


#' @export
quantile.pcj_model1 = function(object, what, value, ..., stat = NULL) {
  stopifnot(exprs = {
    is.pcj_model1(object)
    vek::is_chr_vec_xb1(what)
    what %in% stats::variable.names(object, "posterior")
    !has_error(check_stat(stat, "stat"))
    vek::is_num_vec_z(value)
    all(value >= 0L, na.rm = TRUE)
    all(value <= 1L, na.rm = TRUE)
  })

  if (what %in% stats::variable.names(object, "prior")) {
    prior = new_pcj_distribution(get_prior(object, what))
    if (is_pcj_single_point_prior(prior)) {
      stop(paste0("quantile() is currently not supported for variables that",
                  " have a single point distribution as prior"))
      #q = rep_len(prior$value, length(value))
      #if(anyNA(value, recursive = FALSE))
      #  q[is.na(value)] = value[is.na(value)]
      #names(q) = NULL
      #return(q)
    }
  }

  samples = get_sample(object, what, "all")
  stat_res = obtain_stat_result(samples, stat)
  quantile_res = obtain_stat_quantile(value, stat_res, ...)

  throw_first_error(quantile_res)
  signal_warnings(quantile_res)
  return(get_result(quantile_res))
}

