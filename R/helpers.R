

is_sorted = function(x) {
  stopifnot(vek::is_num_vec(x) || is_list_of_num_1(x))
  if (is_list(x))
    return(!is.unsorted(as.numeric(x)))
  else
    return(!is.unsorted(x))
}


is_list_of_num_1 = function(x) {
  is_num_1 =  \(k) return(vek::is_num_vec(k) && length(k) == 1L)
  return(is_list(x) && all(sapply_(x, is_num_1), na.rm = FALSE))
}


chr_starts_with_inequality = function(x) {
  stopifnot(vek::is_chr_vec(x))
  return(startsWith(x, ">") | startsWith(x, ">=") |
           startsWith(x, "<") | startsWith(x, "<="))
}


strip_attrs = function(x) {
  if (is.environment(x))
    return(x)
  attributes(x) = NULL
  return(x)
}


chr_is_num = function(x) {
  stopifnot(vek::is_chr_vec(x))
  if (is_empty(x))
    return(logical(0L))

  is_num = sapply_(x, \(k) {
    return(tryCatch(
      is.numeric(as.numeric(k)),
      condition = \(cond) return(FALSE)
    ))
  })

  return(is_num)
}


chr_is_quantile_code = function(x) {
  stopifnot(vek::is_chr_vec(x))

  if (is_empty(x))
    return(logical(0L))

  is_q = startsWith(x, "q")
  if (!any(is_q, na.rm = TRUE))
    return(is_q)

  x_ = x[is_q]
  x_ = substr(x_, 2L, nchar(x_))
  is_num = chr_is_num(x_)
  num = as.numeric(x_[is_num]) # TODO handle ints
  is_q[is_q] = is_num
  is_fin_num = is.finite(num)
  is_q[is_q] = is_fin_num
  num = num[is_fin_num]
  is_q[is_q] = num >= 0L & num <= 1L
  return(is_q)
}


chr_parse_quantile_code = function(x) {
  stopifnot(vek::is_chr_vec(x))
  if (is_empty(x))
    return(numeric(0L))

  is_q = chr_is_quantile_code(x)
  q = rep_len(NaN, length(x))
  if (!any(is_q, na.rm = FALSE))
    return(q)

  q[is_q] = as.numeric(substr(x[is_q], 2L, nchar(x[is_q])))
  return(q)
}


gate_apply = function(x, f, g, flag_func, output_type) {
  stopifnot(exprs = {
    # TODO
    #vek::is_num_vec(x) || vek::is_chr_vec(x) || vek::is_lgl_vec(x)
    is.function(f)
    length(formals(f)) > 0L
    is.function(g)
    length(formals(g)) > 0L
    is.function(flag_func)
    length(formals(flag_func)) > 0L
    vek::is_chr_vec_xb1(output_type)
    output_type %in% c("logical", "character", "double", "integer", "numeric")
  })

  flags = flag_func(x)
  stopifnot(exprs = {
    vek::is_lgl_vec_x(flags)
    length(flags) == length(x)
  })

  output_type_f = switch(
    output_type,
    "logical" = logical,
    "character" = character,
    "double" = double,
    "integer" = integer,
    "numeric" = numeric,
    stop()
  )

  if (length(x) == 0L) {
    return(output_type_f(0L))
  }

  x_ = output_type_f(length(x))
  f_output = f(x[flags])
  g_output = g(x[!flags])
  x_[flags] = f_output
  x_[!flags] = g_output

  return(x_)
}


is_num_vec_prop = function(x) {
  return(vek::is_num_vec(x) &&
    all(x[is.finite(x)] >= 0L & x[is.finite(x)] <= 1L, na.rm = TRUE))
}


is_cond = function(object) {
  return(length(class(object)) > 0L &&
    inherits(object, "condition", TRUE) == length(class(object)) &&
    is_all_unique(class(object))
  )
}


has_error = function(object, ...) {
  e = get_error(object, ...)
  stopifnot(is_list(e))
  return(!is_empty(e))
}


has_warning = function(object, ...) {
  w = get_warning(object, ...)
  stopifnot(is_list(w))
  return(!is_empty(w))
}


throw_first_error = function(object) {
  if (has_error(object)) {
    e = get_error(object)[[1L]]
    stop(e)
  }
}


signal_warnings = function(object) {
  if (has_warning(object)) {
    for(w in get_warning(object)) {
      warning(w)
    }
  }
}


is_valid_summary_stats_string = function(x) {
  stopifnot(exprs = {
    vek::is_chr_vec_xb(x)
    is_all_unique(x)
  })

  other_str = x[!startsWith(x, "q")]

  valid_stats = c("mean", "median", "sd", "var", "mean.default",
                  "median.default")

  if (!all(other_str %in% valid_stats, na.rm = FALSE))
    return(FALSE)

  q_str = x[startsWith(x, "q")]
  if (length(q_str) > 0L) {
    max_str_len = max(nchar(q_str), na.rm = FALSE)
    q_str_rhs = substr(q_str, 2L, max_str_len)
    res = pcj_safely({ as.numeric(q_str_rhs) })
    if (has_warning(res) || has_error(res))
      return(FALSE)

    q_val = get_result(res)
    if (!vek::is_num_vec_xyz(q_val))
      return(FALSE)

    if (!all(q_val >= 0L & q_val <= 1L, na.rm = FALSE))
      return(FALSE)

    if (!is_all_unique(q_val))
      return(FALSE)
  }


  return(TRUE)
}


sapply_ = function(x, f, ...) {
  sapply(x, f, ..., simplify = TRUE, USE.NAMES = FALSE)
}


is_empty = function(x) {
  stopifnot(!is.null(x))
  return(length(x) == 0L)
}


valueError = function(message) {
  errorCondition(message, class = "valueError", call = NULL)
}


typeError = function(message) {
  errorCondition(message, class = "typeError", call = NULL)
}


runtimeError = function(message) {
  errorCondition(message, class = "runtimeError", call = NULL)
}


recursive_unclass = function(x, depth = 1000L) {
  stopifnot(exprs = {
    !is.environment(x) # TODO
    vek::is_int_vec_x1(depth)
    depth >= 0L
  })

  if (depth == 0L)
    return(x)

  if (is_list(unclass(x))) {
    depth = depth - 1L
    if (depth == 0L)
      return(unclass(x))

    a = attributes(unclass(x))
    obj = lapply(x, \(k) Tailcall(recursive_unclass, k, depth - 1L))
    attributes(obj) = a
    return(obj)
  }
  else {
    return(unclass(x))
  }
}


dots_names = function(...) {
  if (...length() == 0L)
    return(character(0L))
  if (is.null(...names()))
    return(rep_len("", ...length()))
  else
    return(...names())
}


is_valid_r_version_format = function(x) {
  stopifnot(vek::is_chr_vec(x))
  return(grepl("^\\d+\\.\\d+\\.\\d+$", x)) # e.g. "4.4.0"
}


is_of_mono_class = function(x, cls) {
  stopifnot(vek::is_chr_vec_xb1(cls))
  return(inherits(x, cls, FALSE) && length(class(x)) == 1L && class(x) == cls)
}


is_list = function(x) {
  return(!is.object(x) &&
    is.list(x) &&
    identical(class(x), "list"))
}


is_pcj_safely_obj = function(x) {
  is_uniquely_named_list(x) &&
    all(names(x) %in% c("condition", "output", "result"),
        na.rm = FALSE)
}


is_named_list = function(x) {
  if (!is_list(x))
    return(FALSE)
  if (length(x) == 0L)
    return(TRUE)
  else
    return(vek::is_chr_vec_xb(names(x)))
}


is_uniquely_named_list = function(x) {
  if (!is_named_list(x))
    return(FALSE)
  if (length(x) == 0L)
    return(TRUE)
  else
    return(is_all_unique(names(x)))
}


is_all_unique = function(x) {
  return(length(unique(x)) == length(x))
}


new_df = function(...) {
  data.frame(
    ...,
    row.names = NULL,
    check.rows = FALSE,
    check.names = TRUE,
    fix.empty.names = TRUE,
    stringsAsFactors = FALSE
  )
}


get_r_rng_kinds = function() {
  return(c(
    "Wichmann-Hill",
    "Marsaglia-Multicarry",
    "Super-Duper",
    "Mersenne-Twister",
    "Knuth-TAOCP-2002",
    "Knuth-TAOCP",
    "L'Ecuyer-CMRG"
  ))
}


get_rjags_rng_kinds = function() {
  return(c(
    "base::Wichmann-Hill",
    "base::Marsaglia-Multicarry",
    "base::Super-Duper",
    "base::Mersenne-Twister"
  ))
}


get_error_ = function(object, ...) {
  Filter(\(x) inherits(x, "error", FALSE), object$condition)
}


get_warning_ = function(object, ...) {
  Filter(\(x) inherits(x, "warning", FALSE), object$condition)
}


get_message_ = function(object, ...) {
  Filter(\(x) inherits(x, "message", FALSE), object$condition)
}


get_condition_ = function(object, ...) {
  return(object$condition)
}


get_output_ = function(object, ...) {
  return(object$output)
}


get_result_ = function(object, ...) {
  object$result
}


strsplit_ = function(x, split) {
  stopifnot(exprs = {
    vek::is_chr_vec_x1(x)
    vek::is_chr_vec_x1(split)
  })

  y = strsplit(x, split, fixed = FALSE, perl = FALSE, useBytes = FALSE)[[1L]]
  if (split != "" && endsWith(x, split))
    y = c(y, "")

  return(y)
}


str_count = function(x, target) {
  stopifnot(exprs = {
    vek::is_chr_vec_x1(x)
    vek::is_chr_vec_x1(target)
  })

  lengths(regmatches(x, gregexpr(target, x)))
}




# ------------------------------------------------------------------------------

# TODO rename
#bqc_is_ok_vec <- function(...) {
#  len <- bqc_length_vector(...)
#  len <- len[!(len %in% 0:1L)]
#  return(length(unique(len)) <= 1L)
#}


# TODO
weld <- function(template, data) {
  stopifnot(exprs = {
    vek::is_chr_vec_x1(template)
    #bqc_is_named_list(data)
    length(data) > 0L
    #all(bqc_map_lgl(data, vek::is_chr_vec_x1))
  })

  data_keys <- names(data)
  result <- template
  for (i in 1:length(data)) {
    key <- data_keys[[i]]
    val <- data[[i]]
    patt <- sprintf("\\{\\{%s\\}\\}", key)
    result <- gsub(pattern = patt, replacement = val, x = result)
  }

  return(result)
}


#
bqc_map_by_name <- function(x, y, func, simplify = FALSE) {
  stopifnot(exprs = {
    #bqc_is_list(x)
    #bqc_is_list(y)
    is.function(func)
  })

  if (length(x) == 0L && length(y) == 0L)
    return(NULL)

  common_name_set <- intersect(names(x), names(y))
  common_name_set <- common_name_set[common_name_set != ""]

  if (length(common_name_set) == 0L)
    return(NULL)

  result <- sapply(common_name_set, \(name) {
    return(func(x[[name]], y[[name]], name))
  }, simplify = simplify, USE.NAMES = TRUE)

  return(result)
}


# TODO check length(formals(func)) == 2L and no dots
#bqc_map_df <- function(x, func) {
#  stopifnot(exprs = {
#    #bqc_is_list(x)
#    is.function(func)
#    length(formals(func)) == 1L
#  })
#
#  Map(f = func, x) |>
#    bqc__df__list_row_bind()
#}


#bqc_map2_df <- function(x, y, func) {
#  stopifnot(exprs = {
#    #bqc_is_list(x)
#    #bqc_is_list(y)
#    bqc_is_eq_length(x, y)
#    is.function(func)
#    length(formals(func)) == 2L
#  })
#
#  Map(f = func, x, y) |>
#    bqc__df__list_row_bind()
#}


#bqc__df__list_row_bind <- function(x) {
#  #stopifnot(bqc_is_list(x))
#  if (length(x) == 0L)
#    return(NULL)
#
#  if (length(x) == 1L) {
#    stopifnot(is.data.frame(x[[1L]]))
#    return(x[[1L]])
#  }
#
#  stopifnot(exprs = {
#    all(bqc_map_lgl(x, \(k) is.data.frame(k)))
#    all(bqc_map_lgl(x, \(k) {
#      isTRUE(all( colnames(k) == colnames(x[[1L]]) ))
#    })) #colnames of each data.frame must match exactly
#  })
#
#  df <- do.call(rbind, x)
#  stopifnot(is.data.frame(df))
#  return(df)
#}



#' Evaluates whether `x` is a bare list
#'
#' @param x Any.
#' @returns `logical`.
#' @keywords internal
#bqc_is_list <- function(x) {
#  !is.object(x) && is.list(x)
#}


#' bqc__norm__prob()
#'
#' @description
#' Calculates the probability mass of a normal random variable over an interval.
#'
#' @param mu `double`. The mean(s).
#' @param sigma `double`. The standard deviation(s).
#' @param a `numeric`. The lower bound(s).
#' @param b `numeric`. The upper bound(s).
#' @returns `double`. Probability value(s).
bqc__norm__prob <- function(mu, sigma, a, b) {
  #stopifnot(exprs = {
    #bqc_is_real_vec(mu)
    #bqc_is_real_vec(sigma)
    #vek::is_num_vec(a)
    #vek::is_num_vec(b)
    #all(sigma >= 0L, na.rm = FALSE)
    #all(b >= a, na.rm = FALSE)
    #bqc_is_ok_vec(mu, sigma, a, b)
  #})

  mass_a <- stats::pnorm(a, mu, sigma, TRUE, FALSE)
  mass_b <- stats::pnorm(b, mu, sigma, TRUE, FALSE)
  prob <- mass_b - mass_a

  #stopifnot(exprs = {
    #bqc_is_prob_vec_x(mass_a)
    #bqc_is_prob_vec_x(mass_b)
    #all(mass_b >= mass_a)
    #bqc_is_prob_vec_x(prob)
    #bqc_is_eq_length(mass_a, mass_b, prob)
  #})

  return(prob)
}
