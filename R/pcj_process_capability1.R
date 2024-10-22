


is.pcj_process_capability1 = function(x) {
  is_of_mono_class(x, "pcj_process_capability1")
}


#' @export
new_pcj_process_capability1 = function(
    data,
    pci_params,
    prior_mu,
    prior_sigma,
    prior_predictive_params,
    sampler_params,
    stat = default_stats,
    evaluate = FALSE
  )
{
  stopifnot(exprs = {
    vek::is_num_vec_xyz(data)
    length(data) > 1L
    is.pci_params(pci_params)
    is_valid__pci_params(pci_params)
    vek::is_chr_vec_xb1(prior_mu) || is_pcj_point_prior(prior_mu)
    vek::is_chr_vec_xb1(prior_sigma) || is_pcj_point_prior(prior_sigma)
    !(is_pcj_point_prior(prior_mu) && is_pcj_point_prior(prior_sigma))
    !(is.null(prior_predictive_params) && is.null(sampler_params))
    vek::is_lgl_vec_x1(evaluate)
  })

  if (vek::is_chr_vec_xb1(prior_mu)) {
    prior_mu = parse_jags_dist(prior_mu)
    stopifnot(is.pcj_jags_dist(prior_mu))
  }

  if (vek::is_chr_vec_xb1(prior_sigma)) {
    prior_sigma = parse_jags_dist(prior_sigma)
    stopifnot(is.pcj_jags_dist(prior_sigma))
  }

  if (!is.null(sampler_params)) {
    stopifnot(exprs = {
      is.rjags_params(sampler_params)
      is_valid__rjags_params(sampler_params)
    })
  }

  stat_check = check_stat(stat, "stat")
  if (!is_empty(stat_check)) {
    stop(stat_check[[1L]])
  }

  prior_study_obj = NULL
  if (!is.null(prior_predictive_params)) {
    stopifnot(exprs = {
      is.prior_predictive_params(prior_predictive_params)
      is_valid__prior_predictive_params(prior_predictive_params)
    })

    # Generate prior predictive samples.
    prior_study_obj = new_prior_predictive(
      pci_params,
      prior_mu,
      prior_sigma,
      prior_predictive_params,
      evaluate
    )
  }

  pcj_model = NULL
  if (!is.null(sampler_params)) {
    # Estimate the model.
    pcj_model = new_pcj_model(
      data,
      pci_params,
      prior_mu,
      prior_sigma,
      sampler_params,
      evaluate
    )
  }

  g = \(x, f, default) {
    if (is.null(x))
      return(default)
    else
      return(f(x))
  }

  final_obj = new.env(hash = TRUE, parent = parent.frame(1L), size = NA)
  final_obj$condition = c(
    g(prior_study_obj, get_condition, list()),
    g(pcj_model, get_condition, list())
  )

  final_obj$output = list()
  final_obj$result = list(
    prior_study = prior_study_obj,
    pcj_model = pcj_model,
    stat = stat
  )

  class(final_obj) = "pcj_process_capability1"

  lockEnvironment(final_obj, bindings = TRUE)

  return(final_obj)
}


#' @export
get_error.pcj_process_capability1 = get_error_
#' @export
get_warning.pcj_process_capability1 = get_warning_
#' @export
get_message.pcj_process_capability1 = get_message_
#' @export
get_condition.pcj_process_capability1 = get_condition_
#' @export
get_result.pcj_process_capability1 = get_result_


#' @export
update.pcj_process_capability1 = function(
    object,
    ...
  )
{
  stopifnot(exprs = {
    is.pcj_process_capability1(object)
  })

  if (...length() > 0L) {
    stopifnot(exprs = {
      !is.null(...names())
      all(...names() %in% names(formals(new_pcj_process_capability1)), na.rm = FALSE)
      is_all_unique(...names())
    })
  }

  dots = list(...)

  if (length(dots) == 1L && names(dots) == "stat") {
    # In case "stat" is the only thing being updated, then don't reevaluate the
    # model, but make a copy and only replace "stat" for the new value.
    stat_check = check_stat(dots$stat, "stat")
    if (!is_empty(stat_check))
      stop(stat_check[[1L]])

    obj = as.environment(as.list(object, all.names = TRUE))
    obj$result$stat = dots$stat
    return(obj)
  }

  # TODO case pcj_model is null
  res = get_result(get_result(object)$pcj_model)
  res2 = get_result(get_result(object)$prior_study)

  obj = new_pcj_process_capability1(
    dots$data %||% res$data,
    dots$pci_params %||%res$pci_params,
    dots$prior_mu %||% res$prior_mu,
    dots$prior_sigma %||% res$prior_sigma,
    dots$prior_predictive_params %||% res2$prior_predictive_params,
    dots$sampler_params %||% res$sampler_params,
    dots$stat %||% get_result(object)$stat,
    dots$evaluate %||% res$evaluate
  )

  return(obj)
}


#' @export
variable.names.pcj_process_capability1 = function(object, distribution) {
  stopifnot(exprs = {
    is.pcj_process_capability1(object)
    vek::is_chr_vec_xb1(distribution)
    distribution %in% c("prior", "prior_predictive", "posterior")
  })

  #browser()

  if (distribution == "prior") {
    return(variable.names(get_result(object)$prior_study, distribution))
  } else if (distribution == "prior_predictive") {
    return(variable.names(get_result(object)$prior_study, distribution))
  } else if (distribution == "posterior") {
    return(variable.names(get_result(object)$pcj_model, distribution))
  } else {
    stop()
  }
}


#' @export
get_data.pcj_process_capability1 = function(object) {
  stopifnot(exprs = {
    is.pcj_process_capability1(object)
  })

  return(get_data(get_result(object)$pcj_model))
}


# TODO chain param behavior
#' @export
get_sample.pcj_process_capability1 = function(object, x, distribution, chain) {
  stopifnot(exprs = {
    is.pcj_process_capability1(object)
    vek::is_chr_vec_xb1(x)
    vek::is_chr_vec_xb1(distribution)
    is.null(chain) || vek::is_int_vec_x1(chain) || vek::is_chr_vec_xb1(chain)
    distribution %in% c("prior_predictive", "posterior")
  })

  if (distribution == "prior_predictive") {
    stopifnot(is.null(chain))
    return(get_sample(get_result(object)$prior_study, x))
  } else if (distribution == "posterior") {
    stopifnot(!is.null(chain))
    return(get_sample(get_result(object)$pcj_model, x, chain))
  } else {
    stop()
  }
}



# TODO add data sample size column
#' @export
summary.pcj_process_capability1 = function(object) {
  stopifnot(is.pcj_process_capability1(object))

  res = get_result(object)
  o1 = NULL
  if (is.pcj_prior_predictive(res$prior_study))
    o1 = summary(res$prior_study)

  o2 = NULL
  if (is.pcj_model(get_result(object)$pcj_model))
    o2 = summary(res$pcj_model)

  if (!is.null(o1) && !is.null(o2))
    df = rbind.data.frame(get_result(o1), get_result(o2))
  else if (!is.null(o1))
    df = get_result(o1)
  else if (!is.null(o2))
    df = get_result(o2)
  else
    stop()

  g = \(x, f, default) {
    if (is.null(x)) return(default)
    else return(f(x))
  }

  res = list(
    condition = c(g(o1, get_condition, list()), g(o2, get_condition, list())),
    output = list(),
    result = df
  )

  class(res) = "pcj_process_capability1_summary"
  return(res)
}


#' @export
get_error.pcj_process_capability1_summary = get_error_
#' @export
get_warning.pcj_process_capability1_summary = get_warning_
#' @export
get_message.pcj_process_capability1_summary = get_message_
#' @export
get_condition.pcj_process_capability1_summary = get_condition_
#' @export
get_result.pcj_process_capability1_summary = get_result_


#' @export
print.pcj_process_capability1_summary = function(object, ...) {
  stopifnot(is_of_mono_class(object, "pcj_process_capability1_summary"))

  if (has_error(object))
    stop(get_error(object)[[1L]])

  if (has_warning(object)) {
    for (w in get_warning(object))
      warning(w)
  }

  print.data.frame(get_result(object), ...)

  return(invisible(object))
}


#' @export
probability.pcj_process_capability1 = function(object, x, distribution, value) {
  stopifnot(exprs = {
    is.pcj_process_capability1(object)
    vek::is_chr_vec_xb1(x)
    vek::is_chr_vec_xb1(distribution)
    distribution %in% c("prior", "prior_predictive", "posterior")
    x %in% variable.names(object, distribution)
  })

  stat = get_result(object)$stat

  if (distribution == "prior") {
    stop("probability() is currently not supported for the prior distribution")
  }
  else if (distribution == "prior_predictive") {
    return(probability(get_result(object)$prior_study, x, value, stat))
  }
  else if (distribution == "posterior") {
    return(probability(get_result(object)$pcj_model, x, value, stat))
  } else {
    stop()
  }
}


#mean.pcj_process_capability1 = function(object, x, distribution, stat = NULL) {
#  stopifnot(exprs = {
#    is.pcj_process_capability1(object)
#    vek::is_chr_vec_xb1(x)
#    vek::is_chr_vec_xb1(distribution)
#    distribution %in% c("prior", "prior_predictive", "posterior")
#    x %in% variable.names(object, distribution)
#  })
#
#  if (is.null(stat))
#    stat = default_stats
#
#  stat_check = check_stat(stat, "stat")
#  if (!is_empty(stat_check))
#    stop(stat_check[[1L]])
#
#  if (distribution == "prior") {
#    stop("mean() is currently not supported for the prior distribution")
#  }
#  else if (distribution == "prior_predictive") {
#    return(mean(get_result(object)$prior_study, x, stat))
#  }
#  else if (distribution == "posterior") {
#    return(mean(get_result(object)$pcj_model, x, stat))
#  } else {
#    stop()
#  }
#}


#median.pcj_process_capability1 = function(object, x, distribution, stat = NULL) {
#  stopifnot(exprs = {
#    is.pcj_process_capability1(object)
#    vek::is_chr_vec_xb1(x)
#    vek::is_chr_vec_xb1(distribution)
#    distribution %in% c("prior", "prior_predictive", "posterior")
#    x %in% variable.names(object, distribution)
#  })
#
#  if (is.null(stat))
#    stat = default_stats
#
#  stat_check = check_stat(stat, "stat")
#  if (!is_empty(stat_check))
#    stop(stat_check[[1L]])
#
#  if (distribution == "prior") {
#    stop("median() is currently not supported for the prior distribution")
#  }
#  else if (distribution == "prior_predictive") {
#    return(median(get_result(object)$prior_study, x, stat))
#  }
#  else if (distribution == "posterior") {
#    return(median(get_result(object)$pcj_model, x, stat))
#  } else {
#    stop()
#  }
#}


#' @export
quantile.pcj_process_capability1 = function(object, x, distribution, value) {
  stopifnot(exprs = {
    is.pcj_process_capability1(object)
    vek::is_chr_vec_xb1(x)
    vek::is_chr_vec_xb1(distribution)
    distribution %in% c("prior", "prior_predictive", "posterior")
    x %in% variable.names(object, distribution)
  })

  stat = get_result(object)$stat

  if (distribution == "prior") {
    stop("quantile() is currently not supported for the prior distribution")
  }
  else if (distribution == "prior_predictive") {
    return(quantile(get_result(object)$prior_study, x, value, stat))
  }
  else if (distribution == "posterior") {
    return(quantile(get_result(object)$pcj_model, x, value, stat))
  } else {
    stop()
  }
}

