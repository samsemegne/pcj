

is.interval = function(object) {
  return(is_of_mono_class(object, "interval"))
}


#' @export
new_interval = function(a, b, is_a_inclusive, is_b_inclusive) {
  stopifnot(exprs = {
    vek::is_num_vec_xy1(a)
    vek::is_num_vec_xy1(b)
    vek::is_lgl_vec_x1(is_a_inclusive)
    vek::is_lgl_vec_x1(is_b_inclusive)
  })

  if (is_a_inclusive)
    stopifnot(!is.infinite(a))
  if (is_b_inclusive)
    stopifnot(!is.infinite(b))

  if (is_a_inclusive && is_b_inclusive)
    stopifnot(a <= b)
  else
    stopifnot(a < b)

  return(structure(
    list(
      a = a,
      b = b,
      is_a_inclusive = is_a_inclusive,
      is_b_inclusive = is_b_inclusive
    ),
    class = "interval"
  ))
}

toS


#' @export
new_lower_tail = function(b, is_b_inclusive) {
  stopifnot(exprs = {
    vek::is_num_vec_xy1(b)
    vek::is_lgl_vec_x1(is_b_inclusive)
    stopifnot(!is.infinite(b))
  })

  # TODO consider if is int and .Machine$int.min?

  return(structure(
    list(b = b, is_b_inclusive = is_b_inclusive),
    class = "lower_tail"
  ))
}


#' @export
new_upper_tail = function(a, is_a_inclusive) {
  stopifnot(exprs = {
    vek::is_num_vec_xy1(a)
    vek::is_lgl_vec_x1(is_a_inclusive)
    stopifnot(!is.infinite(a))
  })

  # TODO consider if is int and .Machine$int.max?

  return(structure(
    list(a = a, is_a_inclusive = is_a_inclusive),
    class = "upper_tail"
  ))
}


