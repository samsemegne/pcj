

is.pcj_jags_dist = function(object) is_of_mono_class(object, "pcj_jags_dist")


precision_to_sd = function(precision) {
  stopifnot(exprs = {
    vek::is_num_vec_z(precision)
    all(precision > 0L, na.rm = TRUE)
  })

  val = 1L / sqrt(precision) # TODO 1/var or 1/std?
  val[precision == 0L] = NaN # TODO NA indexing...
  return(val)
}


is_pcj_point_prior = vek::is_num_vec_xyz1


is_pcj_prior = function(x) is.pcj_jags_dist(x) || is_pcj_point_prior(x)


pcj_jags_dist_to_jags = function(object) {
  stopifnot(is.pcj_jags_dist(object))

  args_str = object$args |>
    as.character() |>
    paste0(collapse = ", ", recycle0 = FALSE)

  dist_str = sprintf("%s(%s)", object$name, args_str)

  if (!is.null(object$truncation)) {
    trunc_a = (object$truncation[[1L]] %||% "") |> as.character()
    trunc_b = (object$truncation[[2L]] %||% "") |> as.character()
    trunc_str = sprintf("T(%s, %s)", trunc_a, trunc_b)
    dist_str = sprintf("%s%s", dist_str, trunc_str)
  }

  return(dist_str)
}


pcj_jags_dist_to_jags2 = function(object) {
  stopifnot(is.pcj_jags_dist(object))

  args_str = object$args |>
    as.character() |>
    paste0(collapse = ", ", recycle0 = FALSE)

  dist_str = sprintf("%s(x, %s)", object$name, args_str)
  return(dist_str)
}


pcj_rng = function(object, n) {
  stopifnot(exprs = {
    vek::is_int_vec_x1(n)
    n >= 0
  })

  if (is.pcj_jags_dist(object))
    return(pcj_jags_dist_rng(object, n))
  else if (is_pcj_point_prior(object))
    return(rep_len(object, n))
  else
    stop()
}


pcj_jags_dist_rng = function(
    object,
    n,
    seed = 1L,
    rng_kind = "base::Wichmann-Hill"
  )
{
  stopifnot(exprs = {
    is.pcj_jags_dist(object)
    vek::is_int_vec_x1(n)
    vek::is_int_vec_x1(seed)
    n >= 0
    rng_kind %in% get_rjags_rng_kinds()
  })

  if (n == 0L)
    return(numeric(0L)) # TODO return type depending on dist

  jags = pcj_jags_dist_to_jags(object)

  model_str <- sprintf("
  model {
    for (i in 1:N) {
      y[i] ~ %s
    }
  }
  ", jags)

  model = rjags::jags.model(
    textConnection(model_str),
    data = list(N = n),
    n.chains = 1L,
    inits = list(.RNG.seed = seed, .RNG.name = rng_kind),
    quiet = TRUE
  )

  samples = rjags::coda.samples(
    model,
    variable.names = "y",
    n.iter = 1L,
    progress.bar = "none"
  )

  x = unclass(samples)[[1L]] |> as.numeric()
  return(x)
}


pcj_jags_dist_untruncated_pdf = function(object, x) {
  stopifnot(exprs = {
    is.pcj_jags_dist(object)
    vek::is_num_vec(x)
  })

  if (length(x) == 0L)
    return(double(0L))

  dist_jags = pcj_jags_dist_to_jags2(object)

  jags_str = sprintf("
  data {
    y = %s
  }

  model {
    foo = -1
  }
  ", dist_jags)

  model = rjags::jags.model(
    textConnection(jags_str),
    data = list(x = x),
    n.chains = 1L,
    quiet = TRUE
  )

  samples = rjags::coda.samples(
    model,
    variable.names = "y",
    n.iter = 1L,
    progress.bar = "none"
  )

  x = unclass(samples)[[1L]] |> as.numeric()
  return(x)
}


pcj_jags_dist_untruncated_cdf = function(object, x) {
  stopifnot(exprs = {
    is.pcj_jags_dist(object)
    vek::is_num_vec(x)
  })

  if (length(x) == 0L)
    return(double(0L))

  dist_jags = pcj_jags_dist_to_jags2(object)
  dist_jags = sprintf("p%s", substr(dist_jags, 2L, nchar(dist_jags))) # TODO nchar defaults

  jags_str = sprintf("
  data {
    y = %s
  }

  model {
    foo = -1
  }
  ", dist_jags)

  model = rjags::jags.model(
    textConnection(jags_str),
    data = list(x = x),
    n.chains = 1L,
    quiet = TRUE
  )

  samples = rjags::coda.samples(
    model,
    variable.names = "y",
    n.iter = 1L,
    progress.bar = "none"
  )

  x = unclass(samples)[[1L]] |> as.numeric()
  return(x)
}


# Normalizing constant.
pcj_jags_dist_nc = function(object) {
  stopifnot(is.pcj_jags_dist(object))

  if (is.null(object$truncation)) {
    return(1L)
  }
  else {
    trunc_a = object$truncation[[1L]]
    trunc_b = object$truncation[[2L]]
    mass_a = 0L
    mass_b = 1L
    if (!is.null(trunc_a))
      mass_a = pcj_jags_dist_untruncated_cdf(object, trunc_a)
    if (!is.null(trunc_b))
      mass_b = pcj_jags_dist_untruncated_cdf(object, trunc_b)

    stopifnot(mass_a < mass_b)
    return(mass_b - mass_a)
  }
}


pcj_jags_dist_pdf = function(object, x) {
  stopifnot(exprs = {
    is.pcj_jags_dist(object)
    vek::is_num_vec(x)
  })

  if (is.null(object$truncation)) {
    return(pcj_jags_dist_untruncated_pdf(object, x))
  } else {
    nc = pcj_jags_dist_nc(object)
    dens = pcj_jags_dist_untruncated_pdf(object, x) / nc
    trunc_a = object$truncation[[1L]]
    trunc_b = object$truncation[[2L]]
    if (!is.null(trunc_a))
      dens[x < trunc_a] = 0L
    if (!is.null(trunc_b))
      dens[x > trunc_b] = 0L

    return(dens)
  }
}

