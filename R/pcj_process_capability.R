


is.pcj_process_capability = function(x) {
  is_of_mono_class(x, "pcj_process_capability")
}


#' @export
estimate_capability = function(
    data,
    pci_params,
    prior_mu,
    prior_sigma,
    prior_predictive_params,
    sampler_params,
    sequential_params
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
    is.rjags_params(sampler_params)
    is_valid__rjags_params(sampler_params)
    is.sequential_params(sequential_params)
    is_valid__sequential_params(sequential_params)
  })

  if (vek::is_chr_vec_xb1(prior_mu)) {
    prior_mu = parse_jags_dist(prior_mu)
    stopifnot(is.pcj_jags_dist(prior_mu))
  }

  if (vek::is_chr_vec_xb1(prior_sigma)) {
    prior_sigma = parse_jags_dist(prior_sigma)
    stopifnot(is.pcj_jags_dist(prior_sigma))
  }

  # Generate prior predictive samples.
  prior_study_obj = new_prior_predictive1(
    pci_params,
    prior_mu,
    prior_sigma,
    prior_predictive_params
  )

  pcj_model = NULL
  sequential = NULL
  if (is.null(sequential_params)) {
    # Estimate a single model using all data.
    pcj_model = new_pcj_model(
      data,
      pci_params,
      prior_mu,
      prior_sigma,
      sampler_params
    )
  } else {
    # Run a sequential analysis.
    sequential = sequential_analysis(
      data,
      prior_mu,
      prior_sigma,
      pci_params,
      sampler_params,
      sequential_params
    )
  }

  final_obj = new.env(hash = TRUE, parent = parent.frame(1L), size = NA)
  final_obj$prior_study = prior_study_obj
  final_obj$pcj_model = pcj_model
  final_obj$sequential_analysis = sequential

  class(final_obj) = "pcj_process_capability"

  lockEnvironment(final_obj, bindings = TRUE)

  return(final_obj)
}


#' @export
variable.names.pcj_process_capability = function(object, distribution) {
  stopifnot(exprs = {
    is.pcj_process_capability(object)
    vek::is_chr_vec_xb1(distribution)
    distribution %in% c("prior", "prior_predictive", "posterior")
  })

  if (distribution %in% c("prior", "prior_predictive")) {
    stopifnot(is.pcj_prior_predictive(object$prior_study))
    v = variable.names.pcj_prior_predictive(object$prior_study, distribution)
    return(v)
  } else if (distribution == "posterior") {
    if (is.pcj_model(object$pcj_model)) {
      v = variable.names.pcj_model(object$pcj_model, distribution)
      return(v)
    } else if (is.pcj_sequential_analysis1(object$sequential_analysis)) {
      last_i = length(object$sequential_analysis$fit)
      last_model = object$sequential_analysis$fit[[last_i]]
      stopifnot(is.pcj_model(last_model))
      v = variable.names.pcj_model(last_model, distribution)
      return(v)
    } else {
      stop()
    }
  } else {
    stop()
  }
}


# TODO add data sample size column
#' @export
summary.pcj_process_capability = function(object) {
  stopifnot(is.pcj_process_capability(object))

  o1 = summary(object$prior_study)

  if (is.pcj_model(object$pcj_model)) {
    o2 = summary(object$pcj_model)
  }
  else if (is.pcj_sequential_analysis1(object$sequential_analysis)) {
    o2 = summary(object$sequential_analysis)
  } else if (is.null(object$pcj_model) &&
             is.null(object$sequential_analysis))
  {
    o2 = NULL
  }
  else {
    stop()
  }

  if (is.null(o2))
    df = o1$result
  else
    df = rbind.data.frame(o1$result, o2$result)

  cond_ = get_condition(o1)
  if (!is.null(o2))
    cond_ = c(cond_, get_condition(o2))

  res = list(
    condition = cond_,
    output = list(),
    result = df
  )

  class(res) = "pcj_process_capability_summary"
  return(res)
}


#' @export
get_error.pcj_process_capability_summary = get_error_
#' @export
get_warning.pcj_process_capability_summary = get_warning_
#' @export
get_message.pcj_process_capability_summary = get_message_
#' @export
get_condition.pcj_process_capability_summary = get_condition_
#' @export
get_result.pcj_process_capability_summary = get_result_


#' @export
print.pcj_process_capability_summary = function(object, ...) {
  stopifnot(is_of_mono_class(object, "pcj_process_capability_summary"))

  if (has_error(object))
    stop(get_error(object)[[1L]])

  if (has_warning(object)) {
    for (w in get_warning(object))
      warning(w)
  }

  print.data.frame(object$result, ...)

  return(invisible(object))
}


#' @export
probability.pcj_process_capability = function(
    object, q, x, distribution, i = NULL, stat = NULL
  )
{
  stopifnot(exprs = {
    is.pcj_process_capability(object)
    vek::is_chr_vec_xb1(x)
    vek::is_chr_vec_xb1(distribution)
    distribution %in% c("prior", "prior_predictive", "posterior")
    x %in% variable.names(object, distribution)
  })

  if (is.null(stat))
    stat = default_stats

  stopifnot(exprs = {
    !is.object(stat)
    is.function(stat)
    length(formals(stat)) > 0L
  })

  if (is.pcj_sequential_analysis1(object$sequential_analysis)) {
    stopifnot(exprs = {
      vek::is_int_vec_x1(i)
      i >= -1L
      i != 0L
      i <= length(object$sequential_analysis$fit)
    })

  } else {
    stopifnot(is.null(i))
  }

  if (distribution == "prior") {
    stop("Prior distribution currently does not support probability()")
  }
  else if (distribution == "prior_predictive") {
    p = probability(object$prior_study, q, x, stat)
    return(p)
  }
  else if (distribution == "posterior") {
    if (is.pcj_model(object$pcj_model)) {
      p = probability(object$pcj_model, q, x, stat)
      return(p)
    }
    else if (is.pcj_sequential_analysis1(object$sequential_analysis)) {
      p = probability(object$sequential_analysis, q, x, i, stat)
      return(p)
    } else {
      stop()
    }
  } else {
    stop()
  }
}

