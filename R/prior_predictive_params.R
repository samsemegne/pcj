

is.prior_predictive_params = function(x) {
  is_of_mono_class(x, "prior_predictive_params")
}


#' @export
new_prior_predictive_params = function(
    sample_size,
    seed,
    rng_kind = "Wichmann-Hill",
    rng_version = "4.4.0"
  )
{
  stopifnot(exprs = {
    vek::is_int_vec_x1(sample_size)
    vek::is_int_vec_x1(seed)
    vek::is_chr_vec_xb1(rng_kind)
    vek::is_chr_vec_xb1(rng_version)
    sample_size >= 100L
    rng_kind %in% get_r_rng_kinds()
    is_valid_r_version_format(rng_version)
  })

  return(structure(
    list(
      sample_size = sample_size,
      seed = seed,
      rng_kind = rng_kind,
      rng_version = rng_version
    ),
    class = "prior_predictive_params"
  ))
}


is_valid__prior_predictive_params = function(x) {
  stopifnot(is.prior_predictive_params(x))
  return(all(
    is_uniquely_named_list(unclass(x)),
    identical(names(x), get_prior_predictive_params_names()),
    vek::is_int_vec_x1(x$sample_size),
    vek::is_int_vec_x1(x$seed),
    vek::is_chr_vec_xb1(x$rng_kind),
    vek::is_chr_vec_xb1(x$rng_version),
    x$sample_size >= 100L,
    x$rng_kind %in% get_r_rng_kinds(),
    is_valid_r_version_format(x$rng_version),
    na.rm = FALSE
  ))
}


get_prior_predictive_params_names = function() {
  return(c("sample_size", "seed", "rng_kind", "rng_version"))
}

