


is.pcj_process_capability_model1 = function(x) {
  return(is_of_mono_class(x, "pcj_process_capability_model1"))
}


#' @export
new_pcj_process_capability_model1 = function(
    data,
    pci_parameters,
    prior_mu,
    prior_sigma,
    prior_predictive_parameters,
    sampler_parameters,
    stat = default_stats,
    evaluate = FALSE
  )
{
  stopifnot(exprs = {
    vek::is_num_vec_xyz(data)
    length(data) > 1L
    is.pci_parameters1(pci_parameters)
    is_valid__pci_params(pci_parameters)
    vek::is_chr_vec_xb1(prior_mu) || vek::is_num_vec_xyz1(prior_mu)
    vek::is_chr_vec_xb1(prior_sigma) || vek::is_num_vec_xyz1(prior_sigma)
    !(is.null(prior_predictive_parameters) && is.null(sampler_parameters))
    vek::is_lgl_vec_x1(evaluate)
  })

  prior_mu = new_pcj_distribution(prior_mu)
  prior_sigma = new_pcj_distribution(prior_sigma)
  stopifnot(exprs = {
    !(is_pcj_single_point_prior(prior_mu) && is_pcj_single_point_prior(prior_sigma))
  })

  if (!is.null(sampler_parameters)) {
    stopifnot(exprs = {
      is.rjags_parameters(sampler_parameters)
      is_valid__rjags_params(sampler_parameters)
    })
  }

  stat_check = check_stat(stat, "stat")
  throw_first_error(stat_check)

  prior_study_obj = NULL
  if (!is.null(prior_predictive_parameters)) {
    stopifnot(exprs = {
      is.prior_predictive_parameters(prior_predictive_parameters)
      is_valid__prior_predictive_parameters(prior_predictive_parameters)
    })

    # Generate prior predictive samples.
    prior_study_obj = new_prior_predictive(
      pci_parameters,
      prior_mu,
      prior_sigma,
      prior_predictive_parameters,
      evaluate
    )
  }

  pcj_model1 = NULL
  if (!is.null(sampler_parameters)) {
    # Estimate the model.
    pcj_model1 = new_pcj_model1(
      data,
      pci_parameters,
      prior_mu,
      prior_sigma,
      sampler_parameters,
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
    g(pcj_model1, get_condition, list())
  )

  final_obj$output = list()
  final_obj$result = list(
    prior_predictive = prior_study_obj,
    pcj_model1 = pcj_model1,
    stat = stat
  )

  class(final_obj) = "pcj_process_capability_model1"

  lockEnvironment(final_obj, bindings = TRUE)

  return(final_obj)
}


#' @export
get_error.pcj_process_capability_model1 = get_error_
#' @export
get_warning.pcj_process_capability_model1 = get_warning_
#' @export
get_message.pcj_process_capability_model1 = get_message_
#' @export
get_condition.pcj_process_capability_model1 = get_condition_
#' @export
get_result.pcj_process_capability_model1 = get_result_


#' @export
update.pcj_process_capability_model1 = function(object, ...) {
  stopifnot(exprs = {
    is.pcj_process_capability_model1(object)
  })

  if (...length() > 0L) {
    stopifnot(exprs = {
      !is.null(...names())
      all(...names() %in% names(formals(new_pcj_process_capability_model1)), na.rm = FALSE)
      is_all_unique(...names())
    })
  }

  dots = list(...)

  #if (length(dots) == 1L && names(dots) == "stat") {
  #  # In case "stat" is the only thing being updated, then don't reevaluate the
  #  # model, but make a copy and only replace "stat" for the new value.
  #  stat_check = check_stat(dots$stat, "stat")
  #  throw_first_error(stat_check)
  #
  #  obj = list2env(as.list(object, all.names = TRUE))
  #  obj$result$stat = dots$stat
  #  return(obj)
  #}

  # TODO case pcj_model1 is null
  res = get_result(get_result(object)$pcj_model1)
  res2 = get_result(get_result(object)$prior_predictive)

  obj = new_pcj_process_capability_model1(
    dots$data %||% res$data,
    dots$pci_parameters %||%res$pci_parameters,
    dots$prior_mu %||% res$prior_mu,
    dots$prior_sigma %||% res$prior_sigma,
    dots$prior_predictive_parameters %||% res2$prior_predictive_parameters,
    dots$sampler_parameters %||% res$sampler_parameters,
    dots$stat %||% get_result(object)$stat,
    dots$evaluate %||% FALSE
  )

  return(obj)
}


#' @export
variable.names.pcj_process_capability_model1 = function(object, distribution) {
  stopifnot(exprs = {
    is.pcj_process_capability_model1(object)
    vek::is_chr_vec_x1(distribution)
    distribution %in% c("prior", "prior_predictive", "posterior")
  })

  if (distribution == "prior") {
    return(stats::variable.names(
      get_result(object)$prior_predictive, distribution
    ))
  } else if (distribution == "prior_predictive") {
    return(stats::variable.names(
      get_result(object)$prior_predictive, distribution
    ))
  } else if (distribution == "posterior") {
    return(stats::variable.names(get_result(object)$pcj_model1, distribution))
  } else {
    stop()
  }
}


#' @export
get_data.pcj_process_capability_model1 = function(object) {
  stopifnot(exprs = {
    is.pcj_process_capability_model1(object)
  })

  return(get_data(get_result(object)$pcj_model1))
}


# TODO chain param behavior
# TODO reorder x/distribution
#' @export
get_sample.pcj_process_capability_model1 = function(
    object,
    x,
    distribution,
    chain
  )
{
  stopifnot(exprs = {
    is.pcj_process_capability_model1(object)
    vek::is_chr_vec_x1(x)
    vek::is_chr_vec_x1(distribution)
    is.null(chain) || vek::is_int_vec_x1(chain) || vek::is_chr_vec_x1(chain)
    distribution %in% c("prior_predictive", "posterior")
  })

  if (distribution == "prior_predictive") {
    stopifnot(is.null(chain))
    return(get_sample(get_result(object)$prior_predictive, x))
  } else if (distribution == "posterior") {
    stopifnot(!is.null(chain))
    return(get_sample(get_result(object)$pcj_model1, x, chain))
  } else {
    stop()
  }
}


#' @export
get_prior.pcj_process_capability_model1 = function(object, x) {
  stopifnot(exprs = {
    is.pcj_process_capability_model1(object)
    vek::is_chr_vec_x1(x)
    x %in% stats::variable.names(object, "prior")
  })

  if (!is.null(get_result(object)$prior_predictive))
    return(get_prior(get_result(object)$prior_predictive, x))
  else if (!is.null(get_result(object)$pcj_model1))
    return(get_prior(get_result(object)$pcj_model1, x))
  else
    stop()
}


# TODO add data sample size column
#' @export
summary.pcj_process_capability_model1 = function(
    object,
    statistics = c("mean", "median", "sd", "q.025", "q.25", "q.5", "q.75",
                   "q.975")
  )
{
  stopifnot(exprs = {
    is.pcj_process_capability_model1(object)
    vek::is_chr_vec_xb(statistics)
    is_valid_summary_stats_string(statistics)
  })

  res = get_result(object)
  stat = res$stat

  o1 = NULL
  if (is.pcj_prior_predictive1(res$prior_predictive))
    o1 = summary(res$prior_predictive, stat, statistics)

  o2 = NULL
  if (is.pcj_model1(get_result(object)$pcj_model1))
    o2 = summary(res$pcj_model1, stat, statistics)

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

  res = new_pcj_result(
    df,
    c(g(o1, get_condition, list()), g(o2, get_condition, list())),
    c(g(o1, get_output, list()), g(o2, get_output, list()))
  )

  class(res) = "pcj_process_capability_model1_summary"
  return(res)
}


#' @export
get_error.pcj_process_capability_model1_summary = get_error_
#' @export
get_warning.pcj_process_capability_model1_summary = get_warning_
#' @export
get_message.pcj_process_capability_model1_summary = get_message_
#' @export
get_condition.pcj_process_capability_model1_summary = get_condition_
#' @export
get_result.pcj_process_capability_model1_summary = get_result_


#' @export
print.pcj_process_capability_model1_summary = function(object) {
  stopifnot(is_of_mono_class(object, "pcj_process_capability_model1_summary"))

  throw_first_error(object)
  signal_warnings(object)

  print.data.frame(get_result(object))

  return(invisible(object))
}


#' @export
probability.pcj_process_capability_model1 = function(
    object,
    distribution,
    what,
    value,
    ...
  )
{
  stopifnot(exprs = {
    is.pcj_process_capability_model1(object)
    vek::is_chr_vec_xb1(distribution)
    vek::is_chr_vec_xb1(what)
    vek::is_num_vec_z(value) || vek::is_chr_vec(value)
    distribution %in% c("prior", "prior_predictive", "posterior")
    what %in% stats::variable.names(object, distribution)
  })

  stat = get_result(object)$stat

  if (distribution == "prior") {
    stop("probability() is currently not supported for the prior distribution")
  }
  else if (distribution == "prior_predictive") {
    return(probability(
      get_result(object)$prior_predictive, what, value, ..., stat = stat))
  }
  else if (distribution == "posterior") {
    return(probability(
      get_result(object)$pcj_model1, what, value, ..., stat = stat))
  } else {
    stop()
  }
}


#' @export
mean.pcj_process_capability_model1 = function(object, distribution, what, ...) {
  stopifnot(exprs = {
    is.pcj_process_capability_model1(object)
    vek::is_chr_vec_x1(distribution)
    vek::is_chr_vec_x1(what)
    distribution %in% c("prior", "prior_predictive", "posterior")
    what %in% stats::variable.names(object, distribution)
  })

  stat = get_result(object)$stat

  if (distribution == "prior") {
    stop("mean() is currently not supported for the prior distribution")
  }
  else if (distribution == "prior_predictive") {
    return(mean(get_result(object)$prior_predictive, what, ..., stat = stat))
  }
  else if (distribution == "posterior") {
    return(mean(get_result(object)$pcj_model1, what, ..., stat = stat))
  } else {
    stop()
  }
}


#' @export
median.pcj_process_capability_model1 = function(
    object,
    distribution,
    what,
    ...
  )
{
  stopifnot(exprs = {
    is.pcj_process_capability_model1(object)
    vek::is_chr_vec_x1(distribution)
    vek::is_chr_vec_x1(what)
    distribution %in% c("prior", "prior_predictive", "posterior")
    what %in% stats::variable.names(object, distribution)
  })

  stat = get_result(object)$stat

  if (distribution == "prior") {
    stop("median() is currently not supported for the prior distribution")
  }
  else if (distribution == "prior_predictive") {
    return(stats::median(
      get_result(object)$prior_predictive, what, ..., stat = stat))
  }
  else if (distribution == "posterior") {
    return(stats::median(get_result(object)$pcj_model1, what, ..., stat = stat))
  } else {
    stop()
  }
}


#' @export
quantile.pcj_process_capability_model1 = function(
    object,
    distribution,
    what,
    value,
    ...
  )
{
  stopifnot(exprs = {
    is.pcj_process_capability_model1(object)
    vek::is_chr_vec_x1(distribution)
    vek::is_chr_vec_x1(what)
    distribution %in% c("prior", "prior_predictive", "posterior")
    what %in% stats::variable.names(object, distribution)
  })

  stat = get_result(object)$stat

  if (distribution == "prior") {
    stop("quantile() is currently not supported for the prior distribution")
  }
  else if (distribution == "prior_predictive") {
    return(stats::quantile(
      get_result(object)$prior_predictive, what, value, ..., stat = stat))
  }
  else if (distribution == "posterior") {
    return(stats::quantile(
      get_result(object)$pcj_model1, what, value, ..., stat = stat))
  } else {
    stop()
  }
}


#' @export
has_probability_density.pcj_process_capability_model1 = function(
    object,
    distribution,
    what
  )
{
  stopifnot(exprs = {
    is.pcj_process_capability_model1(object)
    vek::is_chr_vec_x1(distribution)
    distribution %in% c("prior", "prior_predictive", "posterior")
    vek::is_chr_vec_x1(what)
    what %in% stats::variable.names(object, distribution)
  })

  if (distribution %in% c("prior", "posterior")) {
    if (what %in% stats::variable.names(object, "prior")) {
      prior = get_prior(object, what) |>
        new_pcj_distribution()

      return(!is_pcj_single_point_prior(prior))
    } else {
      return(TRUE)
    }
  } else {
    return(TRUE)
  }
}


#' @export
has_probability_mass.pcj_process_capability_model1 = function(
    object,
    distribution,
    what
  )
{
  stopifnot(exprs = {
    is.pcj_process_capability_model1(object)
    vek::is_chr_vec_x1(distribution)
    distribution %in% c("prior", "prior_predictive", "posterior")
    vek::is_chr_vec_x1(what)
    what %in% stats::variable.names(object, distribution)
  })

  return(!has_probability_density(object, distribution, what))
}
