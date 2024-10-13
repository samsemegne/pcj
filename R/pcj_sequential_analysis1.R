

is.pcj_sequential_analysis1 = function(object, ...) {
  is_of_mono_class(object, "pcj_sequential_analysis1")
}


sequential_analysis = function(
    data,
    prior_mu,
    prior_sigma,
    pci_params,
    sampler_params,
    sequential_params
  )
{
  stopifnot(exprs = {
    vek::is_num_vec_xyz(data)
    length(data) > 1L
    is.pci_params(pci_params)
    is_valid__pci_params(pci_params)
    is_pcj_prior(prior_mu)
    is_pcj_prior(prior_sigma)
    !(is_pcj_point_prior(prior_mu) && is_pcj_point_prior(prior_sigma))
    is.rjags_params(sampler_params)
    is_valid__rjags_params(sampler_params)
    all(names(sampler_params$initial_values) %in% c("mu", "sigma"),
        na.rm = FALSE)
    is.sequential_params(sequential_params)
    is_valid__sequential_params(sequential_params)
  })

  if (is_pcj_point_prior(prior_sigma))
    stopifnot(prior_sigma > 0L)

  at = sequential_params$at

  result = list()
  for (i in 1:length(at)) {
    n = at[i]
    dat = utils::head(data, n)
    obj = new_pcj_model1(
      dat,
      pci_params,
      prior_mu,
      prior_sigma,
      sampler_params
    )

    result[[length(result) + 1L]] = obj
  }

  obj = new.env(TRUE, parent.frame(1L), NA)
  obj$fit = result
  obj$sequential_params = sequential_params

  class(obj) = "pcj_sequential_analysis1"

  lockEnvironment(obj, bindings = TRUE)

  return(obj)
}


#' @export
summary.pcj_sequential_analysis1 = function(object, ...) {
  stopifnot(is.pcj_sequential_analysis1(object))

  results = lapply(object$fit, summary)

  dfs = lapply(results, \(k) return(k$result))
  df = do.call(rbind.data.frame, dfs)
  row.names(df) = 1:nrow(df)

  cond_ = lapply(results, get_condition)
  cond = list()
  for (k in cond_)
    cond = c(cond, k)

  res = list(
    condition = cond,
    output = list(),
    result = df
  )

  class(res) = "pcj_sequential_analysis1_summary"

  return(res)
}


#' @export
get_error.pcj_sequential_analysis1_summary = get_error_
#' @export
get_warning.pcj_sequential_analysis1_summary = get_warning_
#' @export
get_message.pcj_sequential_analysis1_summary = get_message_
#' @export
get_condition.pcj_sequential_analysis1_summary = get_condition_


#' @export
print.pcj_sequential_analysis1_summary = function(object, ...) {
  stopifnot(is_of_mono_class(object, "pcj_sequential_analysis1_summary"))

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
probability.pcj_sequential_analysis1 = function(object, q, x, i, stat = NULL) {
  stopifnot(exprs = {
    is.pcj_sequential_analysis1(object)
    length(object$fit) > 0L
    vek::is_num_vec(q)
    vek::is_chr_vec_xb1(x)
    vek::is_int_vec_x1(i)
    #x %in% variable.names(object, "posterior")
    i >= -1L
    i != 0L
    i <= length(object$fit)
  })

  if (is.null(stat))
    stat = default_stats

  stopifnot(exprs = {
    !is.object(stat)
    is.function(stat)
    length(formals(stat)) > 0L
  })

  if (i == -1L)
    i = length(object$fit)

  obj = object$fit[[i]]
  return(probability.pcj_model1(obj, q, x, stat))
}



# TODO implement get_error etc
