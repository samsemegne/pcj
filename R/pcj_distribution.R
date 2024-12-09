

new_pcj_distribution = function(x) {
  stopifnot(exprs = {
    vek::is_num_vec_xyz1(x) || vek::is_chr_vec_xb1(x)
  })

  if (vek::is_chr_vec_xb1(x)) {
    obj = parse_jags_dist(x)
    stopifnot(is.pcj_jags_distribution(obj))
  } else if (vek::is_num_vec_xyz1(x)) {
    obj = x
  } else {
    stop()
  }

  return(structure(
    list(value = obj),
    class = "pcj_distribution"
  ))
}


is_pcj_single_point_prior = function(x) {
  stopifnot(is_of_mono_class(x, "pcj_distribution"))
  return(vek::is_num_vec_xyz1(x$value))
}


is_pcj_prior = function(x) return(is_of_mono_class(x, "pcj_distribution"))

