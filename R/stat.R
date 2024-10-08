
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
  bag_append = \(..., par_env = parent.frame(1L)) {
    stopifnot(exprs = {
      exists("bag")
      ...length() > 0L
    })

    par_env$bag = c(par_env$bag, list(...))
  }

  if (is.function(x)) {
    # Here, "x" is assumed to be a density function.
    if (length(formals(x)) < 1L) {
      msg = '"%s" must have one or more parameters' |>
        sprintf(label)

      bag_append(typeError(msg))
    }

    return(bag)
  }
  else if (is_xy_density(x)) {
    # Here, "x" is assumed to be a density object, e.g. from stats::density.
    return(check_xy_density(x, label))
  }
  else if (is_list(x))  {
    # TODO add length check

    if (!is_uniquely_named_list(x)) {
      msg = sprintf('All names of "%s" must be unique', label)
      bag_append(typeError(msg))
    }

    if (!all(names(x) %in% stat_names, na.rm = FALSE)) {
      msg = sprintf('All names of "%s" must be in: ', label) |>
        paste0_(stat_names_)

      bag_append(typeError(msg))
    }

    if (name_count(x, "density") == 1L) {
      if (is.function(x$density)) {
        if (length(formals(x$density)) < 1L) {
          msg = '"%s$density" must have one or more parameters' |>
            sprintf(label)

          bag_append(typeError(msg))
        }
      } else if (is_xy_density(x$density)) { # TODO unclass first?
        tmp = check_xy_density(x$density, sprintf("%s$density", label))
        for (cond in tmp)
          bag_append(cond)
      } else {
        msg = paste0_('"%s$density" must be a function or list containing ',
                      'elements "x" and "y"') |>
          sprintf(label)

        bag_append(typeError(msg))
      }
    }

    if (name_count(x, "quantile") == 1L) {
      if (!is.function(x$quantile)) {
        msg = sprintf('"%s$quantile" must be a function', label)
        bag_append(typeError(msg))
      } else if (length(formals(x$quantile)) < 1L) {
        msg = '"%s$quantile" function must have one or more parameters"' |>
          sprintf(label)

        bag_append(typeError(msg))
      }
    }

    if (name_count(x, "probability") == 1L) {
      if (!is.function(x$probability)) {
        msg = sprintf('"%s$probability" must be a function', label)
        bag_append(typeError(msg))
      } else if (length(formals(x$probability)) < 1L) {
        msg = '"%s$probability" must have one or more parameters"' |>
          sprintf(label)

        bag_append(typeError(msg))
      }
    }

    if (name_count(x, "mean") == 1L) {
      for (cond in check_scalar_stat(x$mean, sprintf("%s$mean", label)))
        bag_append(cond)
    }

    if (name_count(x, "median") == 1L) {
      for (cond in check_scalar_stat(x$median, sprintf("%s$median", label)))
        bag_append(cond)
    }

    if (name_count(x, "sd") == 1L) {
      for (cond in check_scalar_stat(x$sd, sprintf("%s$sd", label)))
        bag_append(cond)

      if (vek::is_num_vec_xyz1(x$sd) && x$sd < 0L) {
        msg = sprintf('"%s$sd >= 0" is not TRUE', label)
        bag_append(valueError(msg))
      }
    }

    if (name_count(x, "var") == 1L) {
      for (cond in check_scalar_stat(x$sd, sprintf("%s$var", label)))
        bag_append(cond)

      if (vek::is_num_vec_xyz1(x$var) && x$var < 0L) {
        msg = sprintf('"%s$var >= 0" is not TRUE', label)
        bag_append(valueError(msg))
      }
    }

    return(bag)
  } else {
    msg = sprintf('"%s" must be a (density) function or a named list', label)
    bag_append(typeError(msg))
    return(bag)
  }

  stop()
}


check_scalar_stat = function(x, label) {
  if (vek::is_num_vec(x)) {
    if (!vek::is_num_vec_xyz1(x)) {
      msg = sprintf('"%s" must be finite and be of length 1', label)
      return(list(valueError(msg)))
    } else {
      return(list())
    }
  } else if (is.function(x)) {
    if (length(formals(x)) < 1L) {
      msg = sprintf('"%s" must have one or more parameters"', label)
      return(list(typeError(msg)))
    } else {
      return(list())
    }
  } else {
    msg = sprintf('"%s must be a base-R numeric vector or a function"', label)
    return(list(typeError(msg)))
  }
}

check_xy_density = function(x, label) {
  stopifnot(vek::is_chr_vec_xb1(label))

  bag = list()
  bag_append = \(..., par_env = parent.frame(1L)) {
    stopifnot(exists("bag"))
    par_env$bag = c(par_env$bag, list(...))
  }

  if (!is_list(x)) {
    msg = sprintf('"%s" must be a list', label)
    bag_append(typeError(msg))
    return(bag)
  }

  if (!is_xy_density(x)) {
    msg = sprintf('"%s" must contain elements "x" and "y"', label)
    bag_append(typeError(msg))
    return(bag)
  }

  if (!vek::is_num_vec(x$x)) {
    msg = sprintf('"%s$x" must be a base-R numeric vector', label)
    bag_append(typeError(msg))
  }
  else {
    if (!vek::is_num_vec_xyz(x$x)) {
      msg = sprintf('"%s$x" must be finite', label)
      bag_append(valueError(msg))
    }

    if (!(length(x$x) > 1L)) { # TODO based on graphics used, e.g. lines/points
      msg = sprintf('"%s$x" must have length > 1', label)
      bag_append(valueError(msg))
    }
  }

  if (!vek::is_num_vec(x$y)) {
    msg = sprintf('"%s$y" must be a base-R numeric vector', label)
    bag_append(typeError(msg))
  }
  else {
    if (!vek::is_num_vec_xyz(x$y)) {
      msg = sprintf('"%s$y" must be finite', label)
      bag_append(valueError(msg))
    }

    if (!(length(x$y) > 1L)) {
      msg = sprintf('"%s$y" must have length > 1', label)
      bag_append(valueError(msg))
    }
  }

  if (vek::is_num_vec(x$x) && vek::is_num_vec(x$y)) {
    if (length(x$x) != length(x$y)) {
      msg = sprintf('"%s$x" and "%s$y" must be of equal length', label, label)
      bag_append(valueError(msg))
    }
  }

  # TODO check the order of x

  return(bag)
}


get_stat_density = function(x) {
  if (is_xy_density(x))
    return(x)
  else if (is.function(x))
    return(x)
  else if (is_list(x) && is_xy_density(x$density))
    return(x$density)
  else if (is_list(x) && is.function(x$density))
    return(x$density)
  else
    stop()
}


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


# TODO case length(x) == 0
sample_proportion = function(x, q, ...) {
  stopifnot(exprs = {
    vek::is_num_vec_xyz(x)
    vek::is_num_vec(q)
  })

  prop = sapply_(q, \(k) {
    return(sum(k <= x, na.rm = FALSE) / length(x))
  })

  stopifnot(exprs = {
    vek::is_num_vec_xyz(prop)
    all(prop >= 0L, na.rm = FALSE)
    all(prop <= 1L, na.rm = FALSE)
  })

  return(prop)
}


default_stats = function(x, ...) {
  dens_obj = gaussian_density(x, ...)

  list(
    density = dens_obj,
    mean = \(k) mean.default(k, trim = 0, na.rm = FALSE),
    median = \(k) stats::median.default(k, na.rm = FALSE),
    sd = \(k) stats::sd(k, na.rm = FALSE),
    var = \(k) stats::var(k, y = NULL, na.rm = FALSE, use = "everything"),
    quantile = stats::quantile,
    probability = sample_proportion
  )
}


get_at = function(at, samples, stat) {
  stopifnot(exprs = {
    vek::is_num_vec_xyz(samples)
    is.null(at) || vek::is_num_vec_xyz(at) || is.function(at) ||
      vek::is_chr_vec_xb(at)
  })

  tmp = check_stat_result(stat, "stat")
  if (!is_empty(tmp))
    stop(tmp[[1L]])
  else
    rm(tmp)

  new_pcj_safe_obj = \(k) {
    return(structure(list(
      condition = list(),
      output = list(), # TODO is list type?
      result = k
    ), class = "pcj_result"))
  }

  if (is.null(at)) {
    return(new_pcj_safe_obj(at))
  }
  else if (vek::is_num_vec_xyz(at)) {
    return(new_pcj_safe_obj(at))
  }
  else if (is.function(at)) {
    stopifnot(exprs = {
      length(formals(at)) > 0L
    })

    at_obj = pcj_safely(at(samples))
    return(at_obj)
  } else if (vek::is_chr_vec_xb(at)) {
    if (length(at) == 0L)
      return(new_pcj_safe_obj(numeric(0L)))

    supported_modes = c("mean", "median", "mean.default", "median.default")

    is_q = startsWith(at, "q")
    is_mode = at %in% supported_modes
    q_str = at[is_q]
    mode_str = at[is_mode]
    lit_str = at[!(is_q | is_mode)]
    rm(is_q, is_mode)

    stopifnot(exprs = {
      is_all_unique(at)
      # TODO don't allow case both "q.5" and "median" %in% at
    })

    results = list()

    lit = numeric(0L)
    if (length(lit_str) > 0L) {
      # Obtain literal values.
      # TODO, do for each value individually, and don't throw error
      lit = suppressWarnings(as.numeric(lit_str))
      stopifnot(exprs = {
        vek::is_num_vec_xyz(lit)
        # TODO check for unique? else indexing at end may fail?
      })

      names(lit) = lit_str
    }

    q = numeric(0L)
    if (length(q_str) > 0L) {
      # Obtain quantiles.
      q_val = sub(".", "", q_str)
      q_val = suppressWarnings(as.numeric(q_val))
      stopifnot(exprs = {
        vek::is_num_vec_xyz(q_val)
        all(q_val >= 0L, na.rm = FALSE)
        all(q_val <= 1L, na.rm = FALSE)
      })

      q_obj = NULL
      if (is_list(stat) && "quantile" %in% names(stat))
      {
        q_obj = pcj_safely(stat$quantile(samples, q_val))
        q = q_obj$result

      } else {
        q_obj = pcj_safely(stats::quantile(samples, q_val))
        q = q_obj$result
      }

      if (has_error(q_obj)) {
        q = rep_len(NaN, length(q_str))
        names(q) = q_str
        results = c(results, list(quantile = q_obj))
      } else {
        e = NULL
        if (!vek::is_num_vec(q))
          e = e %||%
            typeError('"quantile" must return a base-R numeric vector')

        if (!vek::is_num_vec_xyz(q))
          e = e %||% valueError('"quantile" result must be finite')

        if (length(q) != length(q_val))
          e = e %||%
            valueError('"quantile" result length must equal input length')

        r1 = rank(q, TRUE, "average")
        r2 = rank(q_val, TRUE, "average")
        names(r1) = NULL
        names(r2) = NULL
        if (!identical(r1, r2))
          e = e %||%
          valueError('"quantile" result rank must equal input rank')

        rm(r1, r2)

        if (any(q == q_val, na.rm = FALSE)) # TODO check
          e = e %||% valueError('"quantile" results can\'t equal input')

        if (!is.null(e))
          q = rep_len(NaN, length(q_str))

        tmp = new_pcj_safe_obj(NULL)
        if (!is.null(e))
          tmp$condition = list(e)

        names(q) = q_str
        results = c(results, list(quantile = q_obj, quantile_check = tmp))
        rm(e)
      }

      #stopifnot(exprs = {
      #  vek::is_num_vec_xyz(q)
      #  length(q) == length(q_val)
      #  identical(r1, r2)
      #  all(q != q_val, na.rm = FALSE)
      #})
      #rm(r1, r2)


      #names(q_obj$result) = q_str
      #names(q) = q_str
      #results = c(results, list(quantile = q_obj))
    }

    modes = numeric(0L)
    if (length(mode_str) > 0L) {
      # Obtain modes.
      stopifnot(exprs = {
        all(mode_str %in% supported_modes, na.rm = FALSE) # TODO "mode"
      })

      get_default_f = \(k) {
        switch(
          k,
          "mean.default" = \(x) mean.default(x, trim = 0, na.rm = FALSE),
          "median.default" = \(x) stats::median.default(x, na.rm = FALSE),
          stop()
        )
      }

      default_ops = c("mean.default", "median.default")
      is_valid_mode = vek::is_num_vec_xyz1

      obtain_mode_obj = \(m) {
        if (m %in% default_ops) {
          f = get_default_f(m)
          m_obj = pcj_safely(f(samples))
          return(m_obj)
        }
        else if (is_list(stat) && m %in% names(stat)) {
          m_val = stat[[m]]
          if (is.function(m_val)) {
            f = m_val
            m_obj = pcj_safely(f(samples))
            return(m_obj)
          } else if (is_valid_mode(m_val)) {
            return(m_val)
          } else {
            stop()
          }
        } else {
          stop()
        }
      }

      mode_objects = lapply(mode_str, obtain_mode_obj)
      names(mode_objects) = mode_str

      get_mode = \(k) {
        if (is.pcj_result(k)) {
          if (is_valid_mode(k$result))
            return(k$result)
          else
            return(NaN)
        }
        else if (is_valid_mode(k))
          return(k)
        else
          stop()
      }

      modes = sapply(mode_objects, get_mode, simplify = TRUE, USE.NAMES = FALSE)
      names(modes) = mode_str

      results = c(results, Filter(is.pcj_result, mode_objects))
    }

    values = c(lit, q, modes)
    # Put the values in the order of "at".
    values = values[at]

    return(structure(list(
      condition = do.call(c, lapply(results, get_condition)),
      output = list(), # TODO
      result = values # Named numeric vector
    ), class = "pcj_result"))
  } else {
    stop()
  }
}


obtain_stat_sd = function(samples, stat_result) {
  stopifnot(vek::is_num_vec_xyz(samples))

  stat_check = check_stat_result(stat_result, "stat")
  if (!is_empty(stat_check)) {
    stop(stat_check[[1L]])
  }

  check_sd = \(k) {
    if (!vek::is_num_vec(k)) {
      msg = 'sd must be a base-R numeric vector'
      return(list(typeError(msg)))
    } else if (!vek::is_num_vec_xyz1(k)) {
      msg = 'sd must be finite and be of length 1'
      return(list(valueError(msg)))
    } else if (k < 0L) {
      msg = 'sd >= 0 is not TRUE'
      return(list(valueError(msg)))
    } else {
      return(list())
    }
  }

  if (is.function(stat_result)) {
    stop()
  } else if (is_xy_density(stat_result)) {
    stop()
  } else if (is_list(stat_result)) {
    if (!("sd" %in% names(stat_result)))
      stop()

    if (is.function(stat_result$sd)) {
      f = stat_result$sd
      sd_result = pcj_safely(f(samples))
      sd_check = check_sd(sd_result$result)
      if (is_empty(sd_check))
        return(sd_result) # TODO should include stat_obj_ conditions

      sd_result$result = NaN
      sd_result$condition = c(sd_result$condition, sd_check)
      return(sd_result)
    } else if (vek::is_num_vec(stat_result$sd)) {
      # The stat_result$sd value is not checked here, since it's a value, it has
      # already been checked by check_stat_result().
      sd_result = structure(list(
          condition = list(), # TODO should include stat_obj_ conditions
          output = list(),
          result = stat_result$sd
      ), class = "pcj_result")

      return(sd_result)
    } else {
      stop()
    }
  } else {
    stop()
  }
}

