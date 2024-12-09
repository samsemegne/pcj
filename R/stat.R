

obtain_stat_result = function(samples, stat) {
  stopifnot(exprs = {
    vek::is_num_vec(samples) # TODO
    !has_error(check_stat(stat, "stat"))
  })

  res = pcj_safely(stat(samples))
  res_check = check_stat_result(get_result(res), "stat")
  return(new_pcj_result(
    get_result(res),
    c(get_condition(res), get_condition(res_check)),
    get_output(res)
  ))
}


check_stat = function(x, label) {
  stopifnot(vek::is_chr_vec_xb1(label))

  if (is.object(x)) {
    msg = sprintf('"%s" must not extend a class', label)
    return(new_pcj_check(list(typeError(msg))))
  }

  if (!is.function(x)) {
    msg = sprintf('"%s" must be a function', label)
    return(new_pcj_check(list(typeError(msg))))
  }

  if (length(formals(x)) < 1L) {
    msg = sprintf('"%s" must have at least one parameter')
    return(new_pcj_check(list(valueError(msg))))
  }

  return(new_pcj_check(list()))
}


check_stat_result = function(x, label) {
  stopifnot(exprs = {
    vek::is_chr_vec_xb1(label)
  })

  paste0_ = \(...) paste0(..., collapse = NULL, recycle0 = FALSE)
  stat_names = c("density", "quantile", "probability", "mean", "median", "sd",
                 "var")

  stat_names_ = sprintf('"%s"', stat_names) |>
    paste0(collapse = ", ", recycle0 = FALSE)

  bag = list()

  if (!is_list(x)) {
    msg = sprintf('"%s" must be a list', label)
    bag = c(bag, list(typeError(msg)))
    return(new_pcj_check(bag))
  }

  if (!is_uniquely_named_list(x)) {
    msg = sprintf('All names of "%s" must be unique', label)
    bag = c(bag, list(valueError(msg)))
    return(new_pcj_check(bag))
  }

  if (!all(names(x) %in% stat_names, na.rm = FALSE)) {
    msg = paste0_(sprintf('All names of "%s" must be in: ', label), stat_names_)
    bag = c(bag, list(valueError(msg)))
    return(new_pcj_check(bag))
  }

  check_functions = list(
    density = check_stat_density_result,
    quantile = check_stat_quantile_result,
    probability = check_stat_probability_result,
    mean = check_stat_mean_result,
    median = check_stat_median_result,
    sd = check_stat_sd_result,
    var = check_stat_var_result
  )

  checks = lapply(stat_names, \(k) {
    if (k %in% names(x)) {
      label_ = sprintf("%s$%s", label, k) # e.g. "stat$density"
      check_thing = check_functions[[k]]
      stopifnot(is.function(check_thing))
      return(check_thing(x[[k]], label_))
    }
    else {
      return(new_pcj_check(list()))
    }
  })

  bag = c(bag, do.call(c, lapply(checks, get_condition)))

  return(new_pcj_check(bag))
}


# Also used by check_stat_probability_result().
check_stat_quantile_result = function(x, label) {
  stopifnot(vek::is_chr_vec_xb1(label))

  if (!is.function(x)) {
    msg = sprintf('"%s" must be a function', label)
    return(new_pcj_check(list(typeError(msg))))
  }

  if (length(formals(x)) < 1L) {
    msg = sprintf('"%s" must have one or more parameters"', label)
    return(new_pcj_check(list(valueError(msg))))
  }

  return(new_pcj_check(list()))
}


check_stat_probability_result = function(x, label) {
  return(check_stat_quantile_result(x, label))
}


check_stat_density_result = function(x, label) {
  stopifnot(vek::is_chr_vec_xb1(label))

  if (is.function(x)) {
    if (length(formals(x)) < 1L) {
      msg = sprintf('"%s" must have one or more parameters', label)
      return(new_pcj_check(list(valueError(msg))))
    }

    return(new_pcj_check(list()))
  } else {
    msg = sprintf('"%s" must be a function', label)
    return(new_pcj_check(typeError(msg)))
  }
}


# Also used by check_stat_median_result(), check_stat_sd_result(),
# check_stat_var_result().
check_stat_mean_result = function(x, label) {
  stopifnot(vek::is_chr_vec_xb1(label))

  if (is.function(x)) {
    if (length(formals(x)) < 1L) {
      msg = sprintf('"%s" must have one or more parameters"', label)
      return(new_pcj_check(list(typeError(msg))))
    }

    return(new_pcj_check(list()))
  } else {
    msg = sprintf('"%s must be a function"', label)
    return(new_pcj_check(list(typeError(msg))))
  }
}


check_stat_median_result = function(x, label) {
  return(check_stat_mean_result(x, label))
}


check_stat_sd_result = function(x, label) {
  return(check_stat_mean_result(x, label))
}


check_stat_var_result = function(x, label) {
  return(check_stat_mean_result(x, label))
}


check_xy_density = function(x, label) {
  stopifnot(vek::is_chr_vec_xb1(label))

  bag = list()

  if (!is_list(x)) {
    msg = sprintf('"%s" must be a list', label)
    bag = c(bag, list(typeError(msg)))
    return(new_pcj_check(bag))
  }

  if (!is_xy_density(x)) {
    msg = sprintf('"%s" must contain elements "x" and "y"', label)
    bag = c(bag, list(typeError(msg)))
    return(new_pcj_check(bag))
  }

  if (!vek::is_num_vec(x$x)) {
    msg = sprintf('"%s$x" must be a base-R numeric vector', label)
    bag = c(bag, list(typeError(msg)))
  }
  else {
    if (!vek::is_num_vec_xyz(x$x)) {
      msg = sprintf('"%s$x" must be finite', label)
      bag = c(bag, list(valueError(msg)))
    }

    if (!(length(x$x) > 1L)) { # TODO based on graphics used, e.g. lines/points
      msg = sprintf('"%s$x" must have length > 1', label)
      bag = c(bag, list(valueError(msg)))
    }
  }

  if (!vek::is_num_vec(x$y)) {
    msg = sprintf('"%s$y" must be a base-R numeric vector', label)
    bag = c(bag, list(typeError(msg)))
  }
  else {
    if (!vek::is_num_vec_xyz(x$y)) {
      msg = sprintf('"%s$y" must be finite', label)
      bag = c(bag, list(valueError(msg)))
    }

    if (!(length(x$y) > 1L)) {
      msg = sprintf('"%s$y" must have length > 1', label)
      bag = c(bag, list(valueError(msg)))
    }
  }

  if (vek::is_num_vec(x$x) && vek::is_num_vec(x$y)) {
    if (length(x$x) != length(x$y)) {
      msg = sprintf('"%s$x" and "%s$y" must be of equal length', label, label)
      bag = c(bag, list(valueError(msg)))
    }
  }

  # TODO check the order of x

  return(new_pcj_check(bag))
}


check_mass = function(x, label) {

}


check_density = function(x, label) {

}


check_probability = function(x, label) {
  new_pcj_check(list()) # TODO
}


# Also used by check_median().
check_mean = function(x, label) {
  stopifnot(vek::is_chr_vec_xb1(label))

  if (!vek::is_num_vec(x)) {
    msg = sprintf('"%s" must be a base-R numeric vector', label)
    return(new_pcj_check( list(typeError(msg)) ))
  }

  if (!vek::is_num_vec_xyz1(x)) {
    msg = sprintf('"%s" must be finite and be of length 1', label)
    return(new_pcj_check( list(valueError(msg)) ))
  }

  return(new_pcj_check(list()))
}


check_median = function(x, label) {
  return(check_mean(x, label))
}


# Also used by check_var().
check_sd = function(x, label) {
  stopifnot(vek::is_chr_vec_xb1(label))

  if (!vek::is_num_vec(x)) {
    msg = sprintf('"%s" must be a base-R numeric vector', label)
    return(new_pcj_check( list(typeError(msg)) ))
  }

  if (!vek::is_num_vec_xyz1(x)) {
    msg = sprintf('"%s" must be finite and be of length 1', label)
    return(new_pcj_check( list(valueError(msg)) ))
  }

  if (x < 0L) {
    msg = sprintf('"%s >= 0" is not TRUE', label)
    return(new_pcj_check( list(valueError(msg)) ))
  }

  return(new_pcj_check(list()))
}


check_var = function(x, label) {
  return(check_sd(x, label))
}


check_quantile = function(q, p, label) {
  stopifnot(exprs = {
    vek::is_num_vec(p)
    is_num_vec_prop(p)
    vek::is_chr_vec_xb1(label)
  })

  if (!vek::is_num_vec(q)) {
    msg = sprintf('"%s" must be a base-R numeric vector', label)
    return(new_pcj_check( list(typeError(msg)) ))
  }

  if (length(q) == 0L && length(p) == 0L)
    return(new_pcj_check(list()))

  if (length(q) != length(p)) {
    msg = sprintf('"%s" length must match input length', label)
    return(new_pcj_check( list(valueError(msg)) ))
  }

  cond = list()

  q_ = q[is.finite(p)]
  if (!is_empty(q_)) {
    if (!vek::is_num_vec_xy(q_)) {
      msg = sprintf('"%s" must not be NA or NaN', label)
      cond = c(cond, list(valueError(msg)))
    }
  }

  q_ = q[is.finite(p) & p != 0L & p != 1L]
  if (!is_empty(q_)) {
    if (!vek::is_num_vec_z(q_)) {
      msg = sprintf('"%s" must be finite where "0 < p < 1"', label)
      cond = c(cond, list(valueError(msg)))
    }
  }

  q_ = q[is.finite(p) & p == 0L]
  if (!is_empty(q_)) {
    if (any(q_ == Inf, na.rm = FALSE)) {
      msg = sprintf('"%s" cannot be Inf when "p == 0"', label)
      cond = c(cond, list(valueError(msg)))
    }
  }

  q_ = q[is.finite(p) & p == 1L]
  if (!is_empty(q_)) {
    if (any(q_ == -Inf, na.rm = FALSE)) {
      msg = sprintf('"%s" cannot be -Inf when "p == 1"', label)
      cond = c(cond, list(valueError(msg)))
    }
  }

  q_ = q[is.finite(p)]
  p_ = p[is.finite(p)]
  if (!is_empty(q_)) {
    r1 = rank(p_, TRUE, "average")
    r2 = rank(q_, TRUE, "average")
    names(r1) = NULL
    names(r2) = NULL
    if (!identical(r1, r2)) {
      msg = sprintf('"%s" rank must match rank of p', label)
      cond = c(cond, list(valueError(msg)))
    }

    rm(r1, r2)
  }

  q_ = q[is.finite(p)]
  p_ = p[is.finite(p)]
  if (!is_empty(q_)) {
    if (any(q_ == p_, na.rm = FALSE)) {
      msg = sprintf('"%s" cannot equal p', label)
      cond = c(cond, list(valueError(msg)))
    } # TODO check
  }

  return(new_pcj_check(cond))
}


get_stat_density = function(x) { return(x$density) }


# TODO dots
gaussian_density = function(x, ...) {
  # Intentionally left missing: width, from, to
  args = alist(
    x, bw = "nrd0", adjust = 1, kernel = "gaussian", weights = NULL,
    window = kernel, width =, give.Rkern = FALSE, subdensity = FALSE,
    warnWbw = var(weights) > 0, n = 512, from =, cut = 3, ext = 4,
    old.coords = FALSE, na.rm = FALSE
  )

  dens_obj = do.call(stats::density.default, args)

  attr(dens_obj, "is_left_tail_zero") = TRUE
  attr(dens_obj, "is_right_tail_zero") = TRUE
  return(dens_obj)
}


# TODO use quantile() instead.
# TODO consider how to handle if value is numeric Inf
sample_proportion = function(samples, value) {
  # This function assumes the variable to be a continuous random variable.
  stopifnot(exprs = {
    vek::is_num_vec_xyz(samples)
    vek::is_num_vec_z(value) ||
      is_of_mono_class(value, "interval") ||
      is_of_mono_class(value, "lower_tail") ||
      is_of_mono_class(value, "upper_tail")
  })

  prop_lowertail = \(b) {
    stopifnot(vek::is_num_vec_z(b) && length(b) == 1L)
    if (is.na(b))
      return(b)
    else
      return(sum(samples <= b, na.rm = FALSE) / length(samples))
  }

  if (vek::is_num_vec(value)) {
    # The values represent point values, so return probabilities of zero.
    if (length(value) == 0L)
      return(double(0L))

    names(value) = NULL
    prop = rep_len(0., length(value))
    prop[is.na(value)] = value[is.na(value)]
  } else if (is_of_mono_class(value, "interval")) {
    prop = prop_lowertail(value$b) - prop_lowertail(value$a)
  } else if (is_of_mono_class(value, "lower_tail")) {
    prop = prop_lowertail(value$b)
  } else if (is_of_mono_class(value, "upper_tail")) {
    prop = 1L - prop_lowertail(value$a)
  } else {
    stop()
  }

  stopifnot(exprs = {
    vek::is_num_vec_z(prop)
    all(prop >= 0L, na.rm = TRUE)
    all(prop <= 1L, na.rm = TRUE)
  })

  return(prop)
}


default_stats = function(x, ...) {
  return(list(
    density = \(...) gaussian_density(x, ...),
    mean = \(...) mean.default(x, ...), # trim = 0, na.rm = FALSE
    median = \(...) stats::median.default(x, ...), #na.rm = FALSE
    sd = \(...) stats::sd(x, ...), #na.rm = FALSE
    var = \(...) stats::var(x, ...), # y = NULL, na.rm = FALSE, use = "everything"
    quantile = \(k, ...) stats::quantile(x, k, ...),
    probability = \(k, ...) sample_proportion(x, k, ...)
  ))
}


obtain_stats = function(stat_codes, stat_result, use_names) {
  stopifnot(exprs = {
    vek::is_chr_vec(stat_codes)
    is.pcj_result(stat_result)
    vek::is_lgl_vec_x1(use_names)
    is_valid_summary_stats_string(stat_codes) # TODO
    is_all_unique(stat_codes)
  })

  if (is_empty(stat_codes)) {
    return(list())
  }

  results = list()

  is_q_str = chr_is_quantile_code(stat_codes)
  if (any(is_q_str, na.rm = FALSE)) {
    q_codes = stat_codes[is_q_str]
    p = chr_parse_quantile_code(q_codes)
    stopifnot(vek::is_num_vec_xyz(p))
    quantile_result = obtain_stat_quantile(p, stat_result)
    if (use_names) {
      stopifnot(length(quantile_result$result) == length(q_codes))
      names(quantile_result$result) = q_codes
    }

    results = c(results, list(quantile = quantile_result))
  }

  func_map = list(
    mean = obtain_stat_mean,
    median = obtain_stat_median,
    sd = obtain_stat_sd,
    var = obtain_stat_var
    # TODO mean.default/median.default
  )

  res = lapply(stat_codes[!is_q_str], \(k) {
    f = func_map[[k]]
    res_ = f(stat_result)
    if (use_names) {
      stopifnot(length(get_result(res_)) == 1L)
      names(res_$result) = k
    }

    return(res_)
  })

  names(res) = stat_codes[!is_q_str]
  results = c(results, res)
  return(results)
}


# TODO allow numerics
get_at2 = function(at, stat_result) {
  stopifnot(exprs = {
    vek::is_chr_vec_x(strip_attributes_if_valuetype(at))
    is.pcj_result(stat_result)
    !has_error(stat_result)
  })

  # Check the object carries no "names" attribute. This requirement may change
  # in the future.
  if (is.numeric(at) || is.character(at)) {
    stopifnot(is.null(names(at)))
  }
  else if (is.list(at)) {
    is_names_null = \(k) return(is.null(names(k)))
    stopifnot(exprs = {
      is.null(names(at))
      all(sapply_(at, is_names_null), na.rm = FALSE)
    })
    rm(is_names_null)
  } else {
    stop()
  }

  seq_args = get_by_and_n(at, "at")

  # TODO?
  #where = get_where(at)
  #if (!is.null(where))
  #  where = parse_stat_inequality_str(where, stat_result)

  parse_literal_ = \(x) {
    stopifnot(vek::is_chr_vec_x(x))
    f = \() {
      y = as.numeric(x)
      names(y) = x
      return(y)
    }

    res = pcj_safely(f())
    if (has_error(res) || has_warning(res)) {
      val = rep_len(NaN, length(x))
      names(val) = x
      res$result = val
    }

    return(res)
  }

  parse_inequality_ = \(x) {
    stopifnot(vek::is_chr_vec_x(x))
    return(pcj_safely(parse_stat_inequality_str(x, stat_result)))
  }

  stat_map = get_stat_map(stat_result)[c("mean", "median", "quantile")]
  map = list(
    literal = list(chr_is_num, parse_literal_),
    inequality = list(chr_starts_with_inequality, parse_inequality_)
    # TODO mean/median.default
  )

  map = c(map, stat_map)
  rm(stat_map)

  results = gate_apply2(strip_attributes_if_valuetype(at), map)

  # Combine each result that is a vector into a single vector.
  has_vector_as_result = !(names(results) %in% "inequality")
  res_values = lapply(results[has_vector_as_result], get_result)
  names(res_values) = NULL
  values = do.call(c, res_values)
  rm(res_values)

  #
  k = at[at %in% names(values)]
  values = values[k]
  rm(k)

  # Create sequences from inequality strings representing intervals,
  # e.g. ">= 1 & <= 10" effectively translates into
  # seq(from = 1, to = 10, by = attr(at, "by")) or
  # seq(from = 1, to = 10, length.out = attr(at, "n")).
  extra_points = list()
  if (has_error(results$inequality)) {
    # TODO ...
    browser()
  } else {
    ineq_res = get_result(results$inequality) # list of pcj_result
    extra_points = lapply(ineq_res, \(res) {
      if (has_error(res))
        return(list())

      interval = get_result(res)
      k = create_sequence_from_interval(interval, seq_args$n, seq_args$by)
      names(k) = as.character(k)
      # TODO names of endpoints
      return(k)
    })
  }

  names(extra_points) = NULL
  extra_points = do.call(c, extra_points)

  # Splice in the extra points.
  values = c(values, extra_points)

  # Sort the values.
  values_order = order(as.numeric(values))
  values = values[values_order]

  # Handle duplicate values.
  # Note. This requirement might change in the future, but is enforced currently
  # for simplicity.
  unique_check_condition = list()
  if (!is_all_unique(values)) {
    browser()
    error_msg = '"x" must resolve to unique values, currently'
    unique_check_condition = list(simpleError(error_msg))
    rm(error_msg)
  }

  # Construct the final result object.
  cond = do.call(c, lapply(results, get_condition))
  ineq_cond = do.call(c, lapply(get_result(results$inequality), get_condition))
  cond = c(cond, ineq_cond, unique_check_condition)
  rm(ineq_cond)
  out = do.call(c, lapply(results, get_output))
  ineq_out = do.call(c, lapply(get_result(results$inequality), get_output))
  out = c(out, ineq_out)
  rm(ineq_out)

  return(new_pcj_result(values, cond, out))
}


get_stat_map = function(stat_result) {
  stopifnot(exprs = {
    is.pcj_result(stat_result)
    !has_error(stat_result)
  })

  parse_quantile_ = \(x) {
    stopifnot(vek::is_chr_vec_x(x))
    p = chr_parse_quantile_code(x)
    stopifnot(vek::is_num_vec_xyz(p))
    res = obtain_stat_quantile(p, stat_result)
    names(res$result) = x
    return(res)
  }

  parse_mean_ = \(x) {
    stopifnot(vek::is_chr_vec_x(x))
    res = obtain_stat_mean(stat_result)
    names(res$result) = "mean"
    return(res)
  }

  parse_median_ = \(x) {
    stopifnot(vek::is_chr_vec_x(x))
    res = obtain_stat_median(stat_result)
    names(res$result) = "median"
    return(res)
  }

  is_eq = \(x) {
    stopifnot(vek::is_chr_vec_xb1(x))
    return(\(k) {
      stopifnot(vek::is_chr_vec_x(k))
      return(k == x)
    })
  }

  return(list(
    quantile = list(chr_is_quantile_code, parse_quantile_),
    mean = list(is_eq("mean"), parse_mean_),
    median = list(is_eq("median"), parse_median_)
    # TODO sd, var
  ))
}


gate_apply2 = function(code_str, map) {
  is_valid_map_entry = \(k) {
    return(
      is_list(k) &&
      length(k) == 2L &&
      is.function(k[[1L]]) &&
      is.function(k[[2L]]) &&
      length(formals(k[[1L]])) > 0L &&
      length(formals(k[[2L]])) > 0L
    )
  }

  stopifnot(exprs = {
    vek::is_chr_vec_x(code_str)
    is_uniquely_named_list(map)
    all(sapply_(map, is_valid_map_entry), na.rm = FALSE)
  })

  if (is_empty(code_str))
    return(list())

  results = lapply(names(map), \(k) {
    entry = map[[k]]
    flag_func = entry[[1L]]
    func = entry[[2L]]
    flags = flag_func(code_str)
    stopifnot(exprs = {
      vek::is_lgl_vec_x(flags)
      length(flags) == length(code_str)
    })

    if (!any(flags, na.rm = FALSE))
      return(new_pcj_result(list()))

    args = code_str[flags]
    res = func(args)
    stopifnot(is.pcj_result(res))
    return(res)
  })

  names(results) = names(map)
  return(results)
}


get_at = function(at, stat_result) {
  stopifnot(exprs = {
    vek::is_chr_vec_xb(at)
    is.pcj_result(stat_result)
    !has_error(stat_result)
  })

  if (is_empty(at)) {
    return(new_pcj_result(numeric(0L)))
  }

  value = rep_len(NA_real_, length(at))
  names(value) = as.character(at)

  is_lit_str = chr_is_num(at)
  if (any(is_lit_str, na.rm = FALSE)) {
    lit = as.numeric(at[is_lit_str])
    value[is_lit_str] = lit
  }

  stat_codes = unique(at[!is_lit_str])
  results = obtain_stats(stat_codes, stat_result, TRUE) # Named list of results
  # List of result values (as lists).
  res_val = lapply(results, \(k) as.list(get_result(k)))
  names(res_val) = NULL
  # List of result values, e.g. 'list(mean = 1.5, sd = 2.3)'.
  res_val = do.call(c, res_val)

  for (k in stat_codes) {
    value[names(value) == k] = res_val[[k]]
  }

  if (is_empty(results)) {
    cond = list()
    out = list()
  } else {
    cond = do.call(c, lapply(results, get_condition))
    out = do.call(c, lapply(results, get_output))
  }

  return(new_pcj_result(value, cond, out))
}


obtain_stat_sd = function(stat_result, ...) {
  stopifnot(is.pcj_result(stat_result))

  if (has_error(stat_result)) {
    return(new_pcj_result(
      NaN, get_condition(stat_result), get_output(stat_result)
    ))
  }

  if (!("sd" %in% names(get_result(stat_result)))) {
    e = simpleError('"stat$sd" is missing')
    return(new_pcj_result(
      NA_real_,
      c(get_condition(stat_result), list(e)),
      get_output(stat_result)
    ))
  }

  f = get_result(stat_result)$sd
  sd_result = pcj_safely({ f(...) })
  sd_check = check_sd(get_result(sd_result), "stat$sd")
  sd_val = get_result(sd_result)
  if (has_error(sd_check))
    sd_val = NaN

  return(new_pcj_result(
    sd_val,
    c(get_condition(stat_result),
      get_condition(sd_result), get_condition(sd_check)),
    c(get_output(stat_result), get_output(sd_result))
  ))
}


obtain_stat_var = function(stat_result, ...) {
  stopifnot(is.pcj_result(stat_result))

  if (has_error(stat_result)) {
    return(new_pcj_result(
      NaN, get_condition(stat_result), get_output(stat_result)
    ))
  }

  if (!("var" %in% names(get_result(stat_result)))) {
    e = simpleError('"stat$var" is missing')
    return(new_pcj_result(
      NA_real_,
      c(get_condition(stat_result), list(e)),
      get_output(stat_result)
    ))
  }

  f = get_result(stat_result)$var
  var_result = pcj_safely({ f(...) })
  var_check = check_var(get_result(var_result), "stat$var")
  var_val = get_result(var_result)
  if (has_error(var_check))
    var_val = NaN

  return(new_pcj_result(
    var_val,
    c(get_condition(stat_result),
      get_condition(var_result), get_condition(var_check)),
    c(get_output(stat_result), get_output(var_result))
  ))
}


obtain_stat_probability = function(value, stat_result, ...) {
  stopifnot(exprs = {
    vek::is_num_vec(value) || vek::is_chr_vec(value)
    is.pcj_result(stat_result)
  })

  if (has_error(stat_result)) {
    return(new_pcj_result(
      rep_len(NaN, length(value)),
      get_condition(stat_result),
      get_output(stat_result)
    ))
  }

  if (!("probability" %in% names(get_result(stat_result)))) {
    e = simpleError('"stat$probability" is missing')
    return(new_pcj_result(
      rep_len(NA_real_, length(value)),
      c(get_condition(stat_result), list(e)),
      get_output(stat_result)
    ))
  }

  if (length(value) == 0L) {
    return(new_pcj_result(
      double(0L),
      get_condition(stat_result),
      get_output(stat_result)
    ))
  }

  f = get_result(stat_result)$probability
  # TODO Find a better solution than using f_() and associated code below in
  # gate_apply().
  g = \(k, ...) return(f(k, ...))
  f_ = Vectorize(g, "k", TRUE, FALSE)

  if (vek::is_num_vec(value)) {
    probability_result = pcj_safely({
      gate_apply(
        x = value,
        f = \(k) f(k, ...),
        g = \(k) ifelse(is.infinite(k), NaN, k),
        flag_func = \(k) is.finite(k),
        output_type = "double"
      )
    })

    # TODO add inputs to check?
    probability_check = check_probability(
      get_result(probability_result), "stat$probability")

    probability_val = get_result(probability_result)
    if (has_error(probability_check))
      probability_val = rep_len(NA_real_, length(value))

    return(new_pcj_result(
      probability_val,
      c(get_condition(stat_result), get_condition(probability_result),
        get_condition(probability_check)),
      c(get_output(stat_result), get_output(probability_result))
    ))
  }
  else if (vek::is_chr_vec(value)) {
    # Parse strings like "> 3 & <= 4" to interval objects, and any other values
    # to regular numbers.
    is_num = chr_is_num(value)
    interval = lapply(value[!is_num], inequality_to_interval)
    failed_to_parse_interval = sapply_(interval, is.null)
    if (any(failed_to_parse_interval, na.rm = FALSE)) {
      stop("Failed to parse some characters to intervals")
    }

    value_ = as.list(value)
    value_[is_num] = as.numeric(value[is_num])
    value_[!is_num] = interval

    is_finnum_or_interval = \(k) (is.numeric(k) && is.finite(k)) || is.object(k)

    probability_result = pcj_safely({
      gate_apply(
        x = value_,
        f = \(k) return(f_(k, ...)),
        g = \(k) return(ifelse(is.infinite(as.numeric(k)), NaN, k)),
        flag_func = \(k) sapply_(k, is_finnum_or_interval),
        output_type = "double"
      )
    })

    # TODO add inputs to check?
    probability_check = check_probability(
      get_result(probability_result), "stat$probability")

    probability_val = get_result(probability_result)
    if (has_error(probability_check))
      probability_val = rep_len(NA_real_, length(value))

    return(new_pcj_result(
      probability_val,
      c(get_condition(stat_result), get_condition(probability_result),
        get_condition(probability_check)),
      c(get_output(stat_result), get_output(probability_result))
    ))
  } else {
    stop()
  }
}


obtain_stat_quantile = function(value, stat_result, ...) {
  stopifnot(exprs = {
    vek::is_num_vec(value)
    is.pcj_result(stat_result)
  })

  # TODO do validation here too
  if (has_error(stat_result)) {
    return(new_pcj_result(
      rep_len(NaN, length(value)),
      get_condition(stat_result),
      get_output(stat_result)
    ))
  }

  if (!("quantile" %in% names(get_result(stat_result)))) {
    e = simpleError('"stat$quantile" is missing')
    return(new_pcj_result(
      rep_len(NA_real_, length(value)),
      c(get_condition(stat_result), list(e)),
      get_output(stat_result)
    ))
  }

  if (length(value) == 0L) {
    return(new_pcj_result(
      double(0L),
      get_condition(stat_result),
      get_output(stat_result)
    ))
  }

  f = get_result(stat_result)$quantile

  quantile_result = pcj_safely({
    gate_apply(
      x = value,
      f = \(k) f(k, ...),
      g = identity,
      flag_func = \(k) !is.na(k),
      output_type = "double"
    )
  })

  quantile_check = check_quantile(get_result(quantile_result), value, "stat$quantile")
  quantile_val = get_result(quantile_result)
  if (has_error(quantile_check))
    quantile_val = NaN

  return(new_pcj_result(
    quantile_val,
    c(get_condition(stat_result),
      get_condition(quantile_result), get_condition(quantile_check)),
    c(get_output(stat_result), get_output(quantile_result))
  ))
}


obtain_stat_mean = function(stat_result, ...) {
  stopifnot(is.pcj_result(stat_result))

  if (has_error(stat_result)) {
    return(new_pcj_result(
      NaN, get_condition(stat_result), get_output(stat_result)
    ))
  }

  if (!("mean" %in% names(get_result(stat_result)))) {
    e = simpleError('"stat$mean" is missing')
    return(new_pcj_result(
      NA_real_,
      c(get_condition(stat_result), list(e)),
      get_output(stat_result)
    ))
  }

  f = get_result(stat_result)$mean
  mean_result = pcj_safely({ f(...) })
  mean_check = check_mean(get_result(mean_result), "stat$mean")
  mean_val = get_result(mean_result)
  if (has_error(mean_check))
    mean_val = NaN

  return(new_pcj_result(
    mean_val,
    c(get_condition(stat_result),
      get_condition(mean_result), get_condition(mean_check)),
    c(get_output(stat_result), get_output(mean_result))
  ))
}


obtain_stat_median = function(stat_result, ...) {
  stopifnot(is.pcj_result(stat_result))

  if (has_error(stat_result)) {
    return(new_pcj_result(
      NaN, get_condition(stat_result), get_output(stat_result)
    ))
  }

  if (!("median" %in% names(get_result(stat_result)))) {
    e = simpleError('"stat$median" is missing')
    return(new_pcj_result(
      NA_real_,
      c(get_condition(stat_result), list(e)),
      get_output(stat_result)
    ))
  }

  f = get_result(stat_result)$median
  median_result = pcj_safely({ f(...) })
  median_check = check_median(get_result(median_result), "stat$median")
  median_val = get_result(median_result)
  if (has_error(median_check))
    median_val = NaN

  return(new_pcj_result(
    median_val,
    c(get_condition(stat_result),
      get_condition(median_result), get_condition(median_check)),
    c(get_output(stat_result), get_output(median_result))
  ))
}


parse_stat_expr_str = function(x, stat_result) {
  stopifnot(exprs = {
    vek::is_chr_vec_x1(x)
    is.pcj_result(stat_result)
    !has_error(stat_result)
  })

  y = trimws(x)

  # case, a numeric: "1"
  # case, a numeric: "NA"
  if (chr_is_num(y)) {
    res = pcj_safely({ as.numeric(y) })
    stopifnot(vek::is_num_vec(get_result(res)) && length(get_result(res)) == 1L)
    names(res$result) = y
    return(res)
  }

  # TODO check logic, i.e. length of y
  # case, a stat code: "mean"
  if (is_valid_summary_stats_string(y)) {
    results = obtain_stats(y, stat_result, TRUE)
    return(results[[1L]])
  }

  # case, a call: min(1, 2)
  # case, a call with stat codes: min(mean, 2)
  if (startsWith(y, "min(") && endsWith(y, ")")) {
    z = substr(y, 5L, nchar(y) - 1L) |>
      strsplit_(",") |>
      trimws()

    res = get_at(z, stat_result)
    if (has_error(res)) {
      return(new_pcj_result(NULL, get_condition(res), get_output(res)))
    }

    val = min(get_result(res), na.rm = TRUE)
    return(new_pcj_result(val, get_condition(res), get_output(res)))
  } else if (startsWith(y, "max(") && endsWith(y, ")")) {
    z = substr(y, 5L, nchar(y) - 1L) |>
      strsplit_(",") |>
      trimws()

    res = get_at(z, stat_result)
    if (has_error(res)) {
      return(new_pcj_result(NULL, get_condition(res), get_output(res)))
    }

    val = max(get_result(res), na.rm = TRUE)
    return(new_pcj_result(val, get_condition(res), get_output(res)))
  }

  return(new_pcj_result(NULL, list(simpleError('Failed to parse expression'))))
}


parse_stat_inequality_str1 = function(x, stat_result) {
  stopifnot(exprs = {
    vek::is_chr_vec_x1(x)
    is.pcj_result(stat_result)
    !has_error(stat_result)
  })

  x = trimws(x)
  trim_start = NULL
  if (startsWith(x, ">") || startsWith(x, "<")) {
    trim_start = 1L
  }
  else if (startsWith(x, ">=") || startsWith(x, "<=")) {
    trim_start = 2L
  }
  else {
    cond = list(simpleError('Failed to parse inequality string'))
    return(new_pcj_result(NULL, cond))
  }

  lhs = substr(x, 1L, 1L + trim_start)
  rhs = substr(x, 2L + trim_start, nchar(x)) |>
    trimws()

  rhs_res = parse_stat_expr_str(rhs, stat_result)
  if (has_error(rhs_res)) {
    rhs_res$result = NULL
    return(rhs_res)
  }

  # TODO case xyz

  inequality_str = sprintf("%s %s", lhs, as.character(get_result(rhs_res)))
  interval = inequality_to_interval1(inequality_str)
  stopifnot(!is.null(interval))

  val = list(
    interval = interval,
    value_name = names(get_result(rhs_res)) # TODO case NULL
  )

  return(new_pcj_result(val, get_condition(rhs_res), get_output(rhs_res)))
}


parse_stat_inequality_str_ = function(x, stat_result) {
  stopifnot(exprs = {
    vek::is_chr_vec_x1(x)
    is.pcj_result(stat_result)
    !has_error(stat_result)
  })

  x = strsplit_(x, "&") |>
    trimws()

  if (length(x) > 2L) {
    # TODO return error result
  }

  if (length(x) == 1L) {
    return(parse_stat_inequality_str1(x, stat_result))
  } else if (length(x) == 2L) {
    ineq1 = parse_stat_inequality_str1(x[1L], stat_result)
    ineq2 = parse_stat_inequality_str1(x[2L], stat_result)

    # TODO handle errors
    # TODO handle invalid case, e.g. > 3 & > 4

    tail1 = get_result(ineq1)$interval
    tail2 = get_result(ineq2)$interval
    interval = intersect_lower_and_upper_tail(tail1, tail2)
    return(new_pcj_result(
      interval,
      c(get_condition(ineq1), get_condition(ineq2)),
      c(get_output(ineq1), get_output(ineq2))
    ))
  }
}


parse_stat_inequality_str = function(x, stat_result) {
  f = Vectorize(
    parse_stat_inequality_str_,
    vectorize.args = "x",
    SIMPLIFY = FALSE,
    USE.NAMES = TRUE
  )

  return(f(x, stat_result))
}


get_where = function(object) {
  where = attr(object, "where", TRUE)
  stopifnot(exprs = {
    vek::is_chr_vec_x1(where)
    chr_starts_with_inequality(where)
  })

  return(where)
}


get_by_and_n = function(object, label) {
  stopifnot(exprs = {
    vek::is_chr_vec_xb1(label)
  })

  by = NULL
  n = NULL

  attr_names = names(attributes(object))
  if ("by" %in% attr_names && "n" %in% attr_names)
    stop(sprintf('"%s" may only carry one of "by" or "n" as attributes', label))

  if ("by" %in% attr_names) {
    by = attr(object, "by", TRUE)
    stopifnot(exprs = {
      vek::is_num_vec_xyz1(by)
      by > 0
    })
  }

  if ("n" %in% attr_names) {
    n = attr(object, "n", TRUE)
    stopifnot(exprs = {
      vek::is_int_vec_x1(n)
      n > 1L
    })
  }

  return(list(n = n, by = by))
}
