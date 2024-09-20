

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
    all(names(x) %in% c("error", "warnings", "conditions", "outputs", "result"),
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


pcj_safely = function(expr) {
  warnings_ = list()
  error_ = NULL
  other = list()
  conditions = list()
  result = NULL

  result = withCallingHandlers({
    tryCatch({
      expr
    }, error = \(e) {
      error_ <<- e
    })
  }, condition = \(cond) {
    conditions[[length(conditions) + 1L]] <<- cond
  })

  out = list()
  #out = utils::capture.output({
  #}, file = NULL, append = FALSE, type = "output", split = FALSE)

  #out = trimws(out, which = "both", whitespace = "[ \t\r\n]")
  #out = out[out != ""] |>
  #  as.list()

  warnings_ = Filter(\(k) inherits(k, "warning", FALSE), conditions)
  other = Filter(\(k) !inherits(k, "warning", FALSE), conditions)

  if (length(warnings_) == 0L)
    warnings_ = NULL

  if (!is.null(error_))
    result = NULL

  if (length(other) == 0L)
    other = NULL

  return(list(
    error = error_,
    warnings = warnings_,
    conditions = other,
    outputs = out,
    result = result
  ))
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


new_runtime_error = function(message) {
  return(errorCondition(message = message, class = "runtimeError", call = NULL))
}
