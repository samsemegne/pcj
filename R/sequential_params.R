

is.sequential_params = function(x) is_of_mono_class(x, "sequential_params")


#' @export
new_sequential_params = function(at) {
  stopifnot(exprs = {
    vek::is_int_vec_x(at)
    length(at) > 1L
    !is.unsorted(at, na.rm = FALSE, strictly = TRUE)
    all(at > 0L, na.rm = FALSE) # TODO min sample size?
  })

  return(structure(
    list(at = at),
    class = "sequential_params"
  ))
}


is_valid__sequential_params = function(x) {
  stopifnot(is.sequential_params(x))
  return(all(
    is_uniquely_named_list(unclass(x)),
    identical(names(x), get_sequential_params_names()),
    vek::is_int_vec_x(x$at),
    length(x$at) > 1L,
    !is.unsorted(x$at, na.rm = FALSE, strictly = TRUE),
    all(x$at > 0L, na.rm = FALSE), # TODO min sample size?
    na.rm = FALSE
  ))
}


get_sequential_params_names = function() {
  return("at")
}
