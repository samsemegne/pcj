


is.pcj_process_capability_jags1 = function(x) {
  is_of_mono_class(x, "pcj_process_capability_jags1")
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

  pcj_model1 = NULL
  sequential = NULL
  if (is.null(sequential_params)) {
    # Estimate a single model using all data.
    pcj_model1 = new_pcj_model1(
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
  final_obj$pcj_model1 = pcj_model1
  final_obj$sequential_analysis = sequential

  class(final_obj) = "pcj_process_capability_jags1"

  lockEnvironment(final_obj, bindings = TRUE)

  return(final_obj)
}


#' @export
variable.names.pcj_process_capability_jags1 = function(object, distribution) {
  stopifnot(exprs = {
    is.pcj_process_capability_jags1(object)
    vek::is_chr_vec_xb1(distribution)
    distribution %in% c("prior", "prior_predictive", "posterior")
  })

  if (distribution %in% c("prior", "prior_predictive")) {
    stopifnot(is.pcj_prior_predictive1(object$prior_study))
    v = variable.names.pcj_prior_predictive1(object$prior_study, distribution)
    return(v)
  } else if (distribution == "posterior") {
    if (is.pcj_model1(object$pcj_model)) {
      v = variable.names.pcj_model1(object$pcj_model, distribution)
      return(v)
    } else if (is.pcj_sequential_analysis1(object$sequential_analysis)) {
      last_i = length(object$sequential_analysis$fit)
      last_model = object$sequential_analysis$fit[[last_i]]
      stopifnot(is.pcj_model1(last_model))
      v = variable.names.pcj_model1(last_model, distribution)
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
summary.pcj_process_capability_jags1 = function(object) {
  stopifnot(is.pcj_process_capability_jags1(object))

  o1 = summary(object$prior_study)

  if (is.pcj_model1(object$pcj_model1)) {
    o2 = summary(object$pcj_model1)
  }
  else if (is.pcj_sequential_analysis1(object$sequential_analysis)) {
    o2 = summary(object$sequential_analysis)
  } else if (is.null(object$pcj_model1) &&
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

  class(res) = "pcj_process_capability_jags1_summary"
  return(res)
}


#' @export
get_error.pcj_process_capability_jags1_summary = get_error_
#' @export
get_warning.pcj_process_capability_jags1_summary = get_warning_
#' @export
get_message.pcj_process_capability_jags1_summary = get_message_
#' @export
get_condition.pcj_process_capability_jags1_summary = get_condition_


#' @export
print.pcj_process_capability_jags1_summary = function(object, ...) {
  stopifnot(is_of_mono_class(object, "pcj_process_capability_jags1_summary"))

  if (has_error(object))
    stop(get_error(object)[[1L]])

  if (has_warning(object)) {
    for (w in get_warning(object))
      warning(w)
  }

  print.data.frame(object$result, ...)

  return(invisible(object))
}

