
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
