

is.rjags_params = function(x) is_of_mono_class(x, "rjags_params")


#' @export
new_rjags_params = function(
    burnin,
    sample_size,
    thin,
    n_chains,
    seed,
    rng_kind,
    initial_values = list()
  )
{
  stopifnot(exprs = {
    vek::is_int_vec_x1(burnin)
    vek::is_int_vec_x1(sample_size)
    vek::is_int_vec_x1(thin)
    vek::is_int_vec_x1(n_chains)
    vek::is_int_vec_x1(seed)
    vek::is_chr_vec_xb1(rng_kind)
    is_uniquely_named_list(initial_values)
    burnin >= 50L # TODO
    sample_size >= 100L # TODO
    thin > 0L
    n_chains > 0L
    rng_kind %in% get_rjags_rng_kinds()
  })

  return(structure(
    list(
      burnin = burnin,
      sample_size = sample_size,
      thin = thin,
      n_chains = n_chains,
      seed = seed,
      rng_kind = rng_kind,
      initial_values = initial_values
    ),
    class = "rjags_params"
  ))
}


is_valid__rjags_params = function(x) {
  stopifnot(is.rjags_params(x))
  return(all(
    is_uniquely_named_list(unclass(x)),
    identical(names(x), get_rjags_params_params()),
    vek::is_int_vec_x1(x$burnin),
    vek::is_int_vec_x1(x$sample_size),
    vek::is_int_vec_x1(x$thin),
    vek::is_int_vec_x1(x$n_chains),
    vek::is_int_vec_x1(x$seed),
    vek::is_chr_vec_xb1(x$rng_kind),
    is_uniquely_named_list(x$initial_values),
    x$burnin >= 50L, # TODO
    x$sample_size >= 100L, # TODO
    x$thin > 0L,
    x$n_chains > 0L,
    x$rng_kind %in% get_rjags_rng_kinds(),
    na.rm = FALSE
  ))
}


get_rjags_params_params = function() {
  return(c("burnin", "sample_size", "thin", "n_chains", "seed",
           "rng_kind", "initial_values"))
}
