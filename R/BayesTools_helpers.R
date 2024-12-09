

BayesTools_prior_to_jags = function(x) {
  # Check the structure of the object.
  stopifnot(exprs = {
    BayesTools::is.prior(x)
    is_uniquely_named_list(unclass(x))
    length(x) == 4L
    "distribution" %in% names(x)
    "parameters" %in% names(x)
    "prior_weights" %in% names(x)
    "truncation" %in% names(x)
    # Check distribution.
    vek::is_chr_vec_xb1(x$distribution)
    # Check parameters.
    is_uniquely_named_list(x$parameters)
    all(sapply_(x$parameters, vek::is_num_vec_xyz1), na.rm = FALSE)
    # Check prior_weights.
    vek::is_num_vec_xy1(x$prior_weights)
    # Check truncation.
    is_uniquely_named_list(x$truncation)
    length(x$truncation) == 2L
    c("lower", "upper") %in% names(x$truncation)
    all(sapply_(x$truncation, vek::is_num_vec_xy1), na.rm = FALSE)
  })

  map = get_BayesTools_prior_jags_map()

  name = x$distribution

  if (x$prior_weights != 1L) {
    msg = paste0('A BayesTools "prior_weights" value ',
                 'other than 1 is currently not supported')
    stop(msg)
  }

  if (!(name %in% names(map))) {
    msg = sprintf('BayesTools distribution "%s" is not supported', name)
    stop(msg)
  }

  map = map[[name]]

  params = x$parameters

  stopifnot(exprs = {
    length(params) == length(map$params)
    all(names(params) %in% names(map$params), na.rm = FALSE)
  })

  # Check the validity of the prior object by trying to reproduce it, and seeing
  # it doesn't throw errors.
  args = list(x$distribution, x$parameters, x$truncation, x$prior_weights)
  replica = pcj_safely({ do.call(BayesTools::prior, args) })

  if (has_error(replica)) {
    e = get_error(replica)[[1L]]
    msg = sprintf("Failed to recreate BayesTools prior: %s", e$message)
    stop(msg)
  }

  if (!identical(x, get_result(replica))) {
    msg = "Failed to replicate BayesTools prior"
    stop(msg)
  }

  # Rename BayesTools parameters to JAGS corresponding parameters.
  names(params) = map$params[names(params)]

  # Reorder parameters in the order expected by JAGS.
  jags_dist_info = get_jags_dist_info()
  stopifnot(map$name %in% names(jags_dist_info))
  jags_dist_info = jags_dist_info[[map$name]]
  stopifnot(exprs = {
    vek::is_chr_vec_nxb(jags_dist_info$params)
    length(jags_dist_info$params) == length(params)
    all(names(params) %in% jags_dist_info$params)
  })

  params = params[jags_dist_info$params]

  # Create the JAGS string.
  params_str = sapply_(params, as.character) |>
    paste0(collapse = ", ", recycle0 = FALSE)

  trunc_str = ""
  lwr = x$truncation$lower
  upr = x$truncation$upper
  if (lwr == -Inf && upr == Inf)
    trunc_str = ""
  else if (lwr == -Inf && upr != Inf)
    trunc_str = sprintf("T(, %s)", upr)
  else if (lwr != -Inf && upr == Inf)
    trunc_str = sprintf("T(%s, )", lwr)
  else
    trunc_str = sprintf("T(%s, %s)", lwr, upr)

  jags_str = sprintf("%s(%s)%s", map$name, params_str, trunc_str)
  return(jags_str)
}


get_BayesTools_prior_jags_map = function() {
  g = \(name, params) return(list(name = name, params = params))

  return(list(
    normal =  g(c(normal = "dnorm"), c(mean = "mu", sd = "sigma")),
    beta =    g(c(beta = "dbeta"), c(alpha = "a", beta = "b")),
    exp =     g(c(exp = "dexp"), c(rate = "lambda")),
    gamma =   g(c(gamma = "dgamma"), c(shape = "lambda", rate = "r")), # TODO check
    t =       g(c(t = "dt"), c(location = "mu", scale = "tau", df = "k")), # TODO check
    uniform = g(c(uniform = "dunif"), c(a = "a", b = "b"))
  ))
}





