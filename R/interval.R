


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


new_lower_tail = function(b, is_b_inclusive) {
  stopifnot(exprs = {
    vek::is_num_vec_xy1(b)
    vek::is_lgl_vec_x1(is_b_inclusive)
    !is.infinite(b)
  })

  # TODO consider if is int and .Machine$int.min?

  return(structure(
    list(b = b, is_b_inclusive = is_b_inclusive),
    class = "lower_tail"
  ))
}


new_upper_tail = function(a, is_a_inclusive) {
  stopifnot(exprs = {
    vek::is_num_vec_xy1(a)
    vek::is_lgl_vec_x1(is_a_inclusive)
    !is.infinite(a)
  })

  # TODO consider if is int and .Machine$int.max?

  return(structure(
    list(a = a, is_a_inclusive = is_a_inclusive),
    class = "upper_tail"
  ))
}


# Parses strings expressing inequalities to interval objects, e.g. "> 3" becomes
# a new_upper_tail(3, FALSE) object, and ">= 1 & < 3" becomes a
# new_interval(1, 3, TRUE, FALSE) object. Invalid string return NULL. A string
# holding two inequalities like in the latter example must hold inequalities of
# the opposite kind, i.e. "> 3 & > 5" is invalid.
inequality_to_interval = function(x) {
  stopifnot(vek::is_chr_vec_x1(x))

  x = trimws(x)
  if (x == "")
    return(NULL)

  y = strsplit_(x, "&")
  if (length(y) == 1L)
    return(inequality_to_interval1(x))
  else if (length(y) == 2L) {
    a = inequality_to_interval1(y[1L])
    b = inequality_to_interval1(y[2L])
    if (is.null(a) || is.null(b))
      return(NULL)
    else if (class(a) == class(b))
      return(NULL)
    else
      return(intersect_lower_and_upper_tail(a, b))
  } else {
    return(NULL)
  }
}


# TODO make logic more concise
inequality_to_interval1 = function(x) {
  stopifnot(vek::is_chr_vec_x1(x))

  x = trimws(x)
  if (x == "")
    return(NULL)

  if (startsWith(x, "<=")) {
    if (nchar(x) == 2L)
      return(NULL)

    rhs = substr(x, 3L, nchar(x)) |>
      trimws()

    res = pcj_safely(as.numeric(rhs))
    val = get_result(res)
    if (!is_empty(get_condition(res)))
      return(NULL)
    else if (!vek::is_num_vec_xyz1(val))
      return(NULL)
    else
      return(new_lower_tail(val, TRUE))

  } else if (startsWith(x, ">=")) {
    if (nchar(x) == 2L)
      return(NULL)

    rhs = substr(x, 3L, nchar(x)) |>
      trimws()

    res = pcj_safely(as.numeric(rhs))
    val = get_result(res)
    if (!is_empty(get_condition(res)))
      return(NULL)
    else if (!vek::is_num_vec_xyz1(val))
      return(NULL)
    else
      return(new_upper_tail(val, TRUE))

  } else if (startsWith(x, ">")) {
    if (nchar(x) == 1L)
      return(NULL)

    rhs = substr(x, 2L, nchar(x)) |>
      trimws()

    res = pcj_safely(as.numeric(rhs))
    val = get_result(res)
    if (!is_empty(get_condition(res)))
      return(NULL)
    else if (!vek::is_num_vec_xyz1(val))
      return(NULL)
    else
      return(new_upper_tail(val, FALSE))

  } else if (startsWith(x, "<")) {
    if (nchar(x) == 1L)
      return(NULL)

    rhs = substr(x, 2L, nchar(x)) |>
      trimws()

    res = pcj_safely(as.numeric(rhs))
    val = get_result(res)
    if (!is_empty(get_condition(res)))
      return(NULL)
    else if (!vek::is_num_vec_xyz1(val))
      return(NULL)
    else
      return(new_lower_tail(val, FALSE))

  } else {
    return(NULL)
  }
}


intersect_lower_and_upper_tail = function(x, y) {
  stopifnot(exprs = {
    is_of_mono_class(x, "upper_tail") || is_of_mono_class(x, "lower_tail")
    is_of_mono_class(y, "upper_tail") || is_of_mono_class(y, "lower_tail")
    class(x) != class(y)
  })

  xx = x
  yy = y
  if (is_of_mono_class(x, "upper_tail")) {
    xx = y
    yy = x
  }

  res = pcj_safely({
    new_interval(yy$a, xx$b, yy$is_a_inclusive, xx$is_b_inclusive)
  })

  val = get_result(res)

  if (!is_empty(get_condition(res)))
    return(NULL)

  return(val)
}


