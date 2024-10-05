

is.pcj_model1 = function(x) is_of_mono_class(x, "pcj_model1")


new_pcj_model1 = function(
    data,
    pci_params,
    prior_mu,
    prior_sigma,
    sampler_params
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
  })

  if (is_pcj_point_prior(prior_sigma))
    stopifnot(prior_sigma > 0L)

  # Specify the model.
  model_str = get_model1_str(pci_params$capability_indices)
  pci_args = lapply(pci_params[-1L], as.character)
  prior_args = list(
    prior_mu = prior_to_jags(prior_mu),
    prior_sigma = prior_to_jags(prior_sigma)
  )

  model_str = weld(model_str, c(prior_args, pci_args))
  monitor = get_monitor_var() |> c(pci_params$capability_indices)
  inits = list(
    .RNG.name = sampler_params$rng_kind,
    .RNG.seed = sampler_params$seed
  )

  if ("mu" %in% names(sampler_params$initial_values)) {
    if (!is_pcj_point_prior(prior_mu))
      inits$mu = sampler_params$initial_values$mu
  }

  if ("sigma" %in% names(sampler_params$initial_values)) {
    if (!is_pcj_point_prior(prior_sigma))
      inits$sigma = sampler_params$initial_values$sigma
  }

  f = function() {
    model = rjags::jags.model(
      file = textConnection(model_str),
      data = list(x = data, N = length(data)),
      inits = inits,
      n.adapt = 0L,
      n.chain = sampler_params$n_chains,
      quiet = TRUE
    ) # rjags::jags

    stats::update(model, n.iter = sampler_params$burnin)
    samples = rjags::coda.samples(
      model = model,
      variable.names = monitor,
      n.iter = sampler_params$sample_size,
      thin = sampler_params$thin,
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

  content = list(
    pci_params = pci_params,
    sampler_params = sampler_params,
    sample_size = length(data),
    prior_mu = store_prior(prior_mu),
    prior_sigma = store_prior(prior_sigma),
    r_version = R.version$version.string
  )

  if (!has_error(obj))
    obj$result = c(obj$result, content)
  else
    obj$result = c(list(data = data), content) # TODO

  class(obj) = c("pcj_model1")

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


get_model1_prior_var_name = function() c("mu", "sigma")


get_monitor_var = function() {
  return(c(get_model1_prior_var_name(), get_nonconformance_var_name()))
}


get_supported_pci = function() {
  return(c("C_p", "C_pl", "C_pu", "C_pk", "C_pm"))
}


get_nonconformance_var_name = function() {
  c("p_nonconformance", "p_nonconformance_below", "p_nonconformance_above")
}


#' @export
variable.names.pcj_model1 = function(object, distribution) {
  stopifnot(exprs = {
    is.pcj_model1(object)
    is.pci_params(object$result$pci_params)
    vek::is_chr_vec_xb1(distribution)
    distribution %in% c("prior", "posterior")
  })

  if (distribution == "prior") {
    return(get_model1_prior_var_name())
  } else if (distribution == "posterior") {
    return(c(
      get_model1_prior_var_name(),
      object$result$pci_params$capability_indices,
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
    n_chain = object$result$fit$nchain()

    stopifnot(exprs = {
      all(chain > 0L, na.rm = FALSE)
      all(chain <= n_chain, na.rm = FALSE)
      #length(unique(chain)) == length(chain)
    })

    samples = object$result$samples[[chain]][, x] |> # check indexing behavior
      unclass() # TODO unclass earlier

    attributes(samples) = NULL
    return(samples)
  }
  else if (vek::is_chr_vec_xb1(chain)) {
    stopifnot(chain == "all")
    samples = object$result$samples |>
      lapply(\(k) unclass(k[, x])) |>
      unlist(FALSE, FALSE)

    attributes(samples) = NULL
    return(samples)
  } else {
    stop()
  }
}


#' @export
summary.pcj_model1 = function(object) {
  stopifnot(is.pcj_model1(object))

  x = summary(object$result$samples)

  stats_df = as.data.frame(x$statistics) # TODO defaults
  stats_df = cbind(x = row.names(stats_df), stats_df)

  q_df = as.data.frame(x$quantiles)
  colnames(q_df) = gsub("%", "", colnames(q_df))
  q = as.numeric(colnames(q_df)) * .01
  q = as.character(q)
  q = sub("0\\.", "\\.", q)
  colnames(q_df) = paste0("q", q, collapse = NULL, recycle0 = FALSE)


  df = cbind(stats_df, q_df)
  colnames(df) = gsub("-|\\s+|^$", "_", colnames(df)) |> tolower()
  df = subset.data.frame(df, select = -c(naive_se, time_series_se))

  return(df)
}


prior_to_jags = function(x) {
  if (is.pcj_jags_dist(x))
    return(sprintf("%s %s", "~", pcj_jags_dist_to_jags(x)))
  else if (vek::is_num_vec_xyz1(x))
    return(sprintf("%s %s", "=", as.character(x))) # Point prior
  else
    stop()
}


store_prior = function(x) {
  if (is.pcj_jags_dist(x))
    return(pcj_jags_dist_to_jags(x))
  else if (is_pcj_point_prior(x))
    return(x)
  else
    stop()
}


## TODO
##' @export
#mean.pcj_model1 = function(object, ..., x = NULL, stat = NULL) {
#  stopifnot(exprs = {
#    is.pcj_model1(object)
#  })
#}


## TODO
##' @export
#median.pcj_model1 = function(object, ..., x = NULL, stat = NULL) {
#  stopifnot(exprs = {
#    is.pcj_model1(object)
#  })
#}


## TODO
##' @export
#quantile.pcj_model1 = function(object, ..., x = NULL, stat = NULL) {
#  stopifnot(exprs = {
#    is.pcj_model1(object)
#  })
#}

