
check_stat_result = function(x, label) {
  stopifnot(exprs = {
    vek::is_chr_vec_xb1(label)
  })

  paste0_ = \(...) paste0(..., collapse = NULL, recycle0 = FALSE)
  stat_names = c("density", "quantile", "probability", "mean", "median", "sd",
                 "var")

  stat_names_ = sprintf('"%s"', stat_names) |>
    paste0(collapse = ", ", recycle0 = FALSE)

  if (is.function(x)) {
    # Here, "x" is assumed to be a density function.
    if (length(formals(x)) < 1L) {
      msg = '"%s" must have one or more parameters' |>
        sprintf(label)

      return(typeError(msg))
    }

    return(invisible(NULL)) # All ok
  }
  else if (is_xy_density(x)) {
    # Here, "x" is assumed to be a density object, e.g. from stats::density.
    return(check_xy_density(x, label))
  }
  else if (is_list(x))  {
    if (!is_uniquely_named_list(x)) {
      msg = sprintf('All names of "%s" must be unique', label)
      return(typeError(msg))
    }

    if (!all(names(x) %in% stat_names, na.rm = FALSE)) {
      msg = sprintf('All names of "%s" must be in: ', label) |>
        paste0_(stat_names_)

      return(typeError(msg))
    }

    if ("density" %in% names(x)) {
      if (is.function(x$density)) {
        if (length(formals(x$density)) < 1L) {
          msg = '"%s$density" must have one or more parameters' |>
            sprintf(label)

          return(typeError(msg))
        }
      } else if (is_xy_density(x$density)) {
        tmp = check_xy_density(x$density, sprintf("%s$density", label))
        if (!is.null(tmp))
          return(tmp)
      } else {
        msg = paste0_('"%s$density" must be a function or list containing ',
                      'elements "x" and "y"') |>
          sprintf(label)

        return(typeError(msg))
      }
    }

    if ("quantile" %in% names(x)) {
      if (!is.function(x$quantile)) {
        msg = sprintf('"%s$quantile" must be a function', label)
        return(typeError(msg))
      } else if (length(formals(x$quantile)) < 1L) {
        msg = '"%s$quantile" function must have one or more parameters"' |>
          sprintf(label)

        return(typeError(msg))
      }
    }

    if ("probability" %in% names(x)) {
      if (!is.function(x$probability)) {
        msg = sprintf('"%s$probability" must be a function', label)
        return(typeError(msg))
      } else if (length(formals(x$probability)) < 1L) {
        msg = '"%s$probability" must have one or more parameters"' |>
          sprintf(label)

        return(typeError(msg))
      }
    }

    if ("mean" %in% names(x)) {
      if (!vek::is_num_vec(x$mean)) {
        msg = sprintf('"%s$mean" must be a base-R numeric vector', label)
        return(typeError(msg))
      }
      else if (!vek::is_num_vec_xyz1(x$mean)) {
        msg = sprintf('"%s$mean" must be finite and be of length 1', label)
        return(valueError(msg))
      }
    }

    if ("median" %in% names(x)) {
      if (!vek::is_num_vec(x$median)) {
        msg = sprintf('"%s$median" must be a base-R numeric vector', label)
        return(typeError(msg))
      }
      else if (!vek::is_num_vec_xyz1(x$median)) {
        msg = sprintf('"%s$median" must be finite and be of length 1', label)
        return(valueError(msg))
      }
    }

    if ("sd" %in% names(x)) {
      if (!vek::is_num_vec(x$sd)) {
        msg = sprintf('"%s$sd" must be a base-R numeric vector', label)
        return(typeError(msg))
      }
      else if (!vek::is_num_vec_xyz1(x$sd)) {
        msg = sprintf('"%s$sd" must be finite and be of length 1', label)
        return(valueError(msg))
      }
    }

    if ("var" %in% names(x)) {
      if (!vek::is_num_vec(x$var)) {
        msg = sprintf('"%s$var" must be a base-R numeric vector', label)
        return(typeError(msg))
      }
      else if (!vek::is_num_vec_xyz1(x$var)) {
        msg = sprintf('"%s$var" must be finite and be of length 1', label)
        return(valueError(msg))
      }
    }

    return(invisible(NULL)) # All ok
  } else {
    msg = sprintf('"%s" must be a (density) function or a named list', label)
    return(typeError(msg))
  }
}


check_xy_density = function(x, label) {
  if (!is_xy_density(x)) {
    msg = sprintf('"%s" must be a list containing elements "x" and "y"', label)
    return(typeError(msg))
  }

  if (!is_uniquely_named_list(x)) {
    msg = sprintf('All names of "%s" must be unique', label)
    return(typeError(msg))
  }

  if (!vek::is_num_vec(x$x)) {
    msg = sprintf('"%s$x" must be a base-R numeric vector', label)
    return(typeError(msg))
  }
  else {
    if (!vek::is_num_vec_xyz(x$x)) {
      msg = sprintf('"%s$x" must be finite', label)
      return(valueError(msg))
    } else if (!(length(x$x) > 1L)) {
      msg = sprintf('"%s$x" must have length > 1', label)
      return(valueError(msg))
    }
  }

  if (!vek::is_num_vec(x$y)) {
    msg = sprintf('"%s$y" must be a base-R numeric vector', label)
    return(typeError(msg))
  }
  else {
    if (!vek::is_num_vec_xyz(x$y)) {
      msg = sprintf('"%s$y" must be finite', label)
      return(valueError(msg))
    } else if (!(length(x$y) > 1L)) {
      msg = sprintf('"%s$y" must have length > 1', label)
      return(valueError(msg))
    }
  }

  if (vek::is_num_vec(x$x) && vek::is_num_vec(x$y)) {
    if (length(x$x) != length(x$y)) {
      msg = sprintf('"%s$x" and "%s$y" must be of equal length', label, label)
      return(valueError(msg))
    }
  }

  # TODO check the order of x

  return(invisible(NULL)) # All ok
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
