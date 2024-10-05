

is.pcj_plot_object = function(x) is_of_mono_class(x, "pcj_plot_object")
is.pcj_plot_object_list = function(x) {
  is_of_mono_class(x, "pcj_plot_object_list")
}


#' @export
plot_prior = function(
    object,
    ...,
    x = NULL,
    transform = list()
  )
{
  stopifnot(exprs = {
    is.pcj_process_capability_jags1(object) || is.pcj_model1(object)
    vek::is_chr_vec_xb1(x)
    x %in% c("mu", "sigma")
  })

  graphics = "lines"
  if (...length() > 0L)
    if (dots_names(...)[1L] == "")
      graphics = ...elt(1L)

  stopifnot(exprs = {
    vek::is_chr_vec_xb1(graphics)
    graphics %in% c("lines", "points", "area", "arrows")
  })

  prior_key = sprintf("%s%s", "prior_", x)
  if (is.pcj_process_capability_jags1(object))
    prior_obj = object$prior_study$result[[prior_key]]
  else if (is.pcj_model1(object))
    prior_obj = object$result[[prior_key]]
  else
    stop()

  if (is_pcj_point_prior(prior_obj)) {
    return(plot_point_prior(object, ..., x = x, transform = transform))
  }
  else if (graphics %in% c("lines", "points")) {
    return(plot_prior_(object, ..., x = x, transform = transform))
  }
  else if (graphics == "area") {
    dots = list(...)
    return(do.call(plot_prior_area, c(list(object), dots[-1L], list(x = x, transform = transform))))
    #return(plot_prior_area(object, ..., x = x, transform = transform))
  } else {
    stop()
  }
}


#' @export
plot_prior_predictive = function(
    object,
    ...,
    x = NULL,
    stat = NULL,
    transform = list()
  )
{
  graphics = "lines"
  if (...length() > 0L)
    if (dots_names(...)[1L] == "")
      graphics = ...elt(1L)

  stopifnot(exprs = {
    vek::is_chr_vec_xb1(graphics)
    graphics %in% c("lines", "area", "points")
  })

  if (graphics %in% c("lines", "points")) {
    ###browser()
    dots = list(...)
    return(do.call(plot_prior_predictive_, c(list(object), dots, list(x = x, stat = stat, transform = transform))))
    #return(plot_prior_predictive_(
    #  object,
    #  ...,
    #  x = x,
    #  stat = stat,
    #  transform = transform
    #))
  }
  else if (graphics == "area") {
    dots = list(...)
    args = c(list(object), dots[-1L], list(x = x, stat = stat, transform = transform))
    return(do.call(plot_prior_predictive_area, args))
  } else {
    stop()
  }
}


#' @export
plot_posterior = function(
    object,
    ...,
    x = NULL,
    stat = NULL,
    transform = list()
  )
{
  stopifnot(exprs = {
    is.pcj_process_capability_jags1(object) || is.pcj_model1(object)
  })

  graphics = "lines"
  if (...length() > 0L)
    if (dots_names(...)[1L] == "")
      graphics = ...elt(1L)

  stopifnot(exprs = {
    vek::is_chr_vec_xb1(graphics)
    graphics %in% c("lines", "points", "area")
  })

  if (graphics %in% c("lines", "points")) {
    #browser()
    return(plot_posterior_(object, ..., x = x, stat = stat, transform = transform))
  }
  else if (graphics == "area") {
    dots = list(...)[-1L]
    return(do.call(plot_posterior_area, c(list(object), dots, list(x = x, stat = stat, transform = transform))))
    #return(plot_posterior_area(object, ..., x = x, stat = stat, transform = transform))
  } else {
    stop()
  }
}


plot_point_prior = function(
    object,
    ...,
    x = NULL,
    transform = list()
  )
{
  stopifnot(exprs = {
    is.pcj_process_capability_jags1(object)
    is.pcj_prior_predictive1(object$prior_study)
    vek::is_chr_vec_xb1(x)
    #vek::is_num_vec_xyz(transform) # TODO
    x %in% c("mu", "sigma")
  })

  graphics = "lines"
  if (...length() > 0L)
    if (dots_names(...)[1L] == "")
      graphics = ...elt(1L)

  stopifnot(exprs = {
    vek::is_chr_vec_xb1(graphics)
    graphics %in% c("lines", "arrows")
  })

  dots = list(...)

  if (...length() > 0L)
    if (dots_names(...)[1L] == "")
      dots = dots[-1L]

  stopifnot(is_uniquely_named_list(dots))

  add = FALSE
  if ("add" %in% names(dots)) {
    add = dots$add
    stopifnot(vek::is_lgl_vec_x1(add))
    dots$add = NULL
  }

  var_name = x
  rm(x)
  var_info = get_var_info()

  prior_key = sprintf("prior_%s", var_name)
  prior_obj = object$prior_study$result[[prior_key]]
  stopifnot(is_pcj_point_prior(prior_obj))

  xlim = c(-.5, .5) + prior_obj
  ylab = "Mass"
  xlab = get_var_lab(var_name)
  #legend = xlab
  ylim = c(0L, 1L)

  if (graphics == "lines") {
    if ("type" %in% names(dots)) {
      stopifnot(exprs = {
        vek::is_chr_vec_xb1(dots$type)
        dots$type %in% c("l", "h")
      })

      dots$type = NULL
    }

    xy = list(x = c(prior_obj, prior_obj), y = c(0L, 1L))
    data = xy |> c(list(x_ = var_name, transform = transform, add = add))

    args = list(type = "l") |>
      smth(dots) |>
      smth(use_if_no_theme(
        type = "l",
        lend = "butt"
      )) |>
      smth(get_theme_args(
        func_name = graphics,
        func_namespace = "graphics",
        #class = class,
        data = data
      )) |>
      keep(get_supported_lines_params())

    data = named_list_rm(data, c("x", "y"))
    args = c(xy, args)
    graphics_obj = new_pcj_plot_object("lines.default", args, data)
  }
  else if (graphics == "arrows") {
    x01y01 = list(x0 = prior_obj, y0 = 0L, x1 = prior_obj, y1 = 1L)
    data = x01y01 |> c(list(x_ = var_name, transform = transform, add = add))

    args = dots |>
      smth(get_theme_args(
        func_name = graphics,
        func_namespace = "graphics",
        #class = class,
        data = data
      )) |>
      keep(get_supported_arrows_params())

    data = named_list_rm(data, c("x0", "y0", "x1", "y1"))
    args = c(x01y01, args)
    graphics_obj = new_pcj_plot_object("arrows", args, data)
  } else {
    stop()
  }

  if (add) {
    return(graphics_obj)
  } else {
    data = list(x = NULL, y = NULL, content = list(graphics_obj))

    args = list(type = "n") |>
      smth(dots) |>
      smth(use_if_no_theme(
        xlab = xlab,
        ylab = ylab,
        xlim = xlim,
        ylim = ylim,
        main = "Prior"
      )) |>
      smth(get_theme_args(
        func_name = "plot.default",
        func_namespace = "graphics",
        #class = class,
        data = data
      )) |>
      keep(get_supported_plot_default_params())

    if ("ylim" %in% names(args) && !is.null(args$ylim)) {
      tmp = check_lim(args$ylim, "y")
      if (!is.null(tmp)) stop(tmp)
      else rm(tmp)
    }

    if ("xlim" %in% names(args) && !is.null(args$xlim)) {
      tmp = check_lim(args$xlim, "x")
      if (!is.null(tmp)) stop(tmp)
      else rm(tmp)
    }

    data = named_list_rm(data, c("x", "y", "content"))
    args = c(list(x = NULL, y = NULL), args)
    plot_default_obj = new_pcj_plot_object("plot.default", args, data)

    plot_obj = list(plot_default_obj, graphics_obj)
    class(plot_obj) = "pcj_plot_object_list"
    return(plot_obj)
  }
}


plot_prior_ = function(
    object,
    ...,
    x = NULL,
    transform = list()
  )
{
  stopifnot(exprs = {
    is.pcj_process_capability_jags1(object) # TODO or is.pcj_model
    #vek::is_chr_vec_xb1(x) # TODO

    #vek::is_num_vec_xyz(transform) # TODO
    x %in% c("mu", "sigma")
  })

  graphics = "lines"
  if (...length() > 0L)
    if (dots_names(...)[1L] == "")
      graphics = ...elt(1L)

  stopifnot(exprs = {
    vek::is_chr_vec_xb1(graphics)
    graphics %in% c("lines", "points")
  })

  dots = list(...)
  if (...length() > 0L)
    if (dots_names(...)[1L] == "")
      dots = dots[-1L]

  stopifnot(is_uniquely_named_list(dots))

  var_name = x
  rm(x)
  var_info = get_var_info()
  var_bounds = get_var_bounds(var_name, var_info)

  at = NULL
  if ("at" %in% names(dots)) {
    at = dots$at
    dots$at = NULL
  }

  if (!is.null(at)) {
    stopifnot(exprs = {
      vek::is_num_vec_xyz(at)
      length(at) > 1L
      !is.unsorted(at, na.rm = FALSE, strictly = TRUE)
      #is_within_var_bounds(at, var_bounds) # TODO
    })
  }

  add = FALSE
  if ("add" %in% names(dots)) {
    add = dots$add
    dots$add = NULL
  }

  stopifnot(vek::is_lgl_vec_x1(add))

  # Get the prior.
  prior_key = sprintf("prior_%s", var_name)
  prior_obj = parse_jags_dist(object$prior_study$result[[prior_key]])
  stopifnot(is.pcj_jags_dist(prior_obj))

  # Determine x.
  if ("xlim" %in% names(dots) && !is.null(dots$xlim))
    xlim = dots$xlim
  else if (!is.null(at))
    xlim = range(at, na.rm = FALSE)
  else
    xlim = auto_xlim_pcj_jags_dist(prior_obj)

  tmp = check_lim(xlim, "x")
  if (!is.null(tmp))
    stop(tmp)
  else
    rm(tmp)

  if (is.null(at))
    x = seq(xlim[1L], xlim[2L], length.out = 501L)
  else
    x = at

  type = switch(graphics, "lines" = "l", "points" = "p", stop())
  if ("type" %in% names(dots)) {
    type = dots$type
    dots$type = NULL
  }

  stopifnot(vek::is_chr_vec_xb1(type))
  if (graphics == "lines") {
    stopifnot(type %in% c("l", "h"))
    if (type == "h")
      stop('Setting "type" to "h" is currently not supported')
  }
  else if (graphics == "points")
    stopifnot(type == "p")
  else
    stop()


  # Calculate the densities.
  y = pcj_jags_dist_pdf(prior_obj, x)

  # Cut x values outside of the supported range for the given prior.
  # (Hard coded).
  if (var_name == "sigma" && x[1L] < 0L) {
    ya = pcj_jags_dist_pdf(prior_obj, 0L)
    y = c(ya, y[x > 0L])
    x = c(0L, x[x > 0L])
  }

  # Insert truncation points.
  if (!is.null(prior_obj$truncation) && graphics == "lines") {
    trunc_a = prior_obj$truncation[[1L]]
    trunc_b = prior_obj$truncation[[2L]]
    min_x = min(x, na.rm = TRUE)
    max_x = max(x, na.rm = TRUE)
    if (!is.null(trunc_a) && trunc_a >= min_x) {
      ya = pcj_jags_dist_pdf(prior_obj, trunc_a)
      xy = splice_truncation_xy(x, y, trunc_a, NULL, ya, NULL)
      x = xy$x
      y = xy$y
    }
    if (!is.null(trunc_b) && trunc_b <= max_x) {
      yb = pcj_jags_dist_pdf(prior_obj, trunc_b)
      xy = splice_truncation_xy(x, y, NULL, trunc_b, NULL, yb)
      x = xy$x
      y = xy$y
    }
  }

  ylab = "Density"
  xlab = get_var_lab(var_name)
  legend = xlab
  ylim = range(y, na.rm = TRUE)
  data = list(x = x, y = y, x_ = var_name, transform = transform, add = add)

  args = list(type = type) |>
    smth(dots) |>
    smth(get_theme_args(
      func_name = graphics,
      func_namespace = "graphics",
      #class = class,
      data = data
    ))

  if (graphics == "lines")
    args = args |> keep(get_supported_lines_params())
  else if (graphics == "points")
    args = args |> keep(get_supported_points_params())
  else
    stop()

  data = named_list_rm(data, c("x", "y"))
  args = c(list(x = x, y = y), args)
  func = switch(
    graphics, "lines" = "lines.default", "points" = "points.default", stop())

  graphics_obj = new_pcj_plot_object(func, args, data)

  if (add) {
    return(graphics_obj)
  } else {
    data = list(x = NULL, y = NULL, content = list(graphics_obj))

    args = list(type = "n") |>
      smth(dots) |>
      smth(use_if_no_theme(
        ylab = ylab,
        xlab = xlab,
        ylim = ylim,
        xlim = xlim,
        main = "Prior"
      )) |>
      smth(get_theme_args(
        func_name = "plot.default",
        func_namespace = "graphics",
        #class = class,
        data = data
    )) |>
      keep(get_supported_plot_default_params())

    if ("ylim" %in% names(args) && !is.null(args$ylim)) {
      tmp = check_lim(args$ylim, "y")
      if (!is.null(tmp)) stop(tmp)
      else rm(tmp)
    }

    if ("xlim" %in% names(args) && !is.null(args$xlim)) {
      tmp = check_lim(args$xlim, "x")
      if (!is.null(tmp)) stop(tmp)
      else rm(tmp)
    }

    data = named_list_rm(data, c("x", "y", "content"))
    args = c(list(x = NULL, y = NULL), args)
    plot_default_obj = new_pcj_plot_object("plot.default", args, data)

    plot_obj = list(plot_default_obj, graphics_obj)
    class(plot_obj) = "pcj_plot_object_list"
    return(plot_obj)
  }
}


plot_prior_predictive_ = function(
    object,
    ...,
    x = NULL,
    stat = NULL,
    transform = list()
  )
{
  stopifnot(exprs = {
    is.pcj_process_capability_jags1(object)
    is.pcj_prior_predictive1(object$prior_study)
    !has_error(object$prior_study)
    vek::is_chr_vec_xb1(x)

    #vek::is_num_vec_xyz(transform) # TODO
    # TODO check x %in% c(...)
  })

  if (is.null(stat)) {
    stat = gaussian_density
  } else {
    stopifnot(exprs = {
      !is.object(stat)
      is.function(stat)
      length(formals(stat)) > 0L
    })
  }

  graphics = "lines"
  if (...length() > 0L)
    if (dots_names(...)[1L] == "")
      graphics = ...elt(1L)

  stopifnot(exprs = {
    vek::is_chr_vec_xb1(graphics)
    graphics %in% c("lines", "points")
  })

  dots = list(...)
  if (...length() > 0L)
    if (dots_names(...)[1L] == "")
      dots = dots[-1L]

  ##browser()
  stopifnot(is_uniquely_named_list(dots))

  var_name = x
  rm(x)
  var_info = get_var_info()
  var_bounds = get_var_bounds(var_name, var_info)

  samples = get_sample.pcj_prior_predictive1(object$prior_study, var_name)

  add = FALSE
  if ("add" %in% names(dots)) {
    add = dots$add
    dots$add = NULL
  }

  stopifnot(vek::is_lgl_vec_x1(add))

  stat_args = list(samples)
  if ("..." %in% names(formals(stat))) {
    # Provide metadata about the variable.
    #stat_args = c(stat_args, list(
    #  var_name = var_name,
    #  lower_bound = var_bounds$lower,
    #  upper_bound = var_bounds$upper
    #))
  }

  stat_obj_ = pcj_safely(do.call(stat, stat_args))

  if (has_error(stat_obj_)) {
    meta = list(result = list(stat = stat_obj_))
    e = get_error(stat_obj_)
    w = get_warning(stat_obj_)
    return(new_pcj_plot_object(NULL, NULL, meta, e, w))
  }

  stat_obj = recursive_unclass(stat_obj_$result, 5L) # TODO adjust depth

  stat_check = check_stat_result(stat_obj, "stat")
  if (!is_empty(stat_check)) {
    meta = list(result = list(stat = stat_obj_, stat_check = stat_check))
    e = stat_check
    w = get_warning(stat_obj_)
    return(new_pcj_plot_object(NULL, NULL, meta, e, w))
  }

  dens_obj = get_stat_density(stat_obj)

  at = NULL
  if ("at" %in% names(dots)) {
    at = dots$at
    dots$at = NULL
  }

  at_obj = get_at(at, samples, stat)

  if (has_error(at_obj)) {
    meta = list(result = list(
      stat = stat_obj_, stat_check = check_stat, at = at_obj
    ))

    e = get_error(at_obj)
    w = c(get_warning(stat_obj_), get_warning(at_obj))
    return(new_pcj_plot_object(NULL, NULL, meta, e, w))
  }

  at = at_obj$result
  stopifnot({
    vek::is_num_vec_xyz(at) || is.null(at)
  })

  if (!is.null(at)) {
    if (graphics == "lines")
      stopifnot(length(at) > 1L)
    else if (graphics == "points")
      stopifnot(length(at) > 0L)
    else
      stop()

    # Store the original order of "at", so other vectorized arguments may be
    # re-ordered in the new order.
    at_order = order(at, na.last = TRUE, decreasing = FALSE, method = "auto")

    at = at[at_order]
    stopifnot(exprs = {
      !is.unsorted(at, na.rm = FALSE, strictly = TRUE)
    })


    # TODO
    #is_col_vec = \(k) {
    #  vek::is_num_vec(k) || vek::is_chr_vec(k) || vek::is_lgl_vec(k)
    #}
    #
    #if (graphics == "points" && "col" %in% names(dots)
    #    && is_col_vec(dots$col))
    #{
    #
    #}
    #rm(is_col_vec)
  }

  xlim = NULL
  if ("xlim" %in% names(dots) && !is.null(dots$xlim)) {
    xlim = dots$xlim
  } else {
    if (is.function(stat_obj))
      xlim = range(samples, na.rm = TRUE)
    else
      xlim = range(stat_obj$x, na.rm = TRUE)
  }

  tmp = check_lim(xlim, "x")
  if (!is.null(tmp))
    stop(tmp)
  else
    rm(tmp)

  if (is.null(at))
    x = seq.default(xlim[1L], xlim[2L], length.out = 501L)
  else
    x = at

  type = switch(graphics, "lines" = "l", "points" = "p", stop())
  if ("type" %in% names(dots)) {
    type = dots$type
    dots$type = NULL
  }

  stopifnot(vek::is_chr_vec_xb1(type))
  if (graphics == "lines")
    stopifnot(type %in% c("l", "h"))
  else if (graphics == "points")
    stopifnot(type == "p")
  else
    stop()

  if (type == "h")
    stop('Setting "type" to "h" is currently not supported')

  if (is.function(dens_obj)) {
    y = dens_obj(x) # TODO safely()
    xy = list(x = x, y = y)
  } else if (is_xy_density(dens_obj)) {
    xy = dens_obj[c("x", "y")]
    fill_zero_left = attr(dens_obj, "is_left_tail_zero", TRUE) %||% FALSE
    fill_zero_right = attr(dens_obj, "is_right_tail_zero", TRUE) %||% FALSE
    stopifnot(exprs = {
      vek::is_lgl_vec_x1(fill_zero_left)
      vek::is_lgl_vec_x1(fill_zero_right)
    })

    min_x = x[1L]
    max_x = x[length(x)]

    # Segment xy to be in between the range of x.
    is_left_cut = FALSE
    is_right_cut = FALSE
    if (min(xy$x, na.rm = TRUE) < min_x) {
      xy = slice_xy(xy$x, xy$y, min_x, FALSE)
      is_left_cut = TRUE
    }
    if (max(xy$x, na.rm = TRUE) > max_x) {
      xy = slice_xy(xy$x, xy$y, max_x, TRUE)
      is_right_cut = TRUE
    }

    # Extend the tails of the distribution up to xlim[1] and xlim[2] with points
    # of zero density.
    if (!is.null(dots$xlim)) {
      if (fill_zero_left && !is_left_cut &&
          xlim[1L] < min(xy$x, na.rm = TRUE))
      {
        lower = max(xlim[1L], var_bounds$lower, na.rm = FALSE)
        xy = list(x = c(lower, xy$x), y = c(0L, xy$y))
        rm(lower)
      }
      if (fill_zero_right && !is_right_cut &&
          xlim[2L] > max(xy$x, na.rm = TRUE))
      {
        upper = min(xlim[2L], var_bounds$upper, na.rm = FALSE)
        xy = list(x = c(xy$x, upper), y = c(xy$y, 0L))
        rm(upper)
      }
    }

    # TODO error when length(x) == 1
    if (graphics == "points") {
      xy = stats::approx(xy$x, xy$y, xout = x, method = "linear")
    }
  } else {
    stop()
  }

  xlab = get_var_lab(var_name)
  legend = xlab
  ylab = "Density"
  ylim = range(xy$y, na.rm = TRUE)
  data = xy |>
    c(list(
      x_ = var_name, transform = transform, add = add,
      result = list(stat = stat_obj_, stat_check = stat_check, at = at_obj)
    ))

  args = dots |>
    smth(get_theme_args(
      func_name = graphics,
      func_namespace = "graphics",
      #class = class,
      data = data
    ))

  if (graphics == "lines")
    args = args |> keep(get_supported_lines_params())
  else if (graphics == "points")
    args = args |> keep(get_supported_points_params())
  else
    stop()

  data = named_list_rm(data, c("x", "y"))
  args = c(xy, args)
  func = switch(
    graphics,
    "lines" = "lines.default",
    "points" = "points.default",
    stop()
  )

  w = c(get_warning(stat_obj_), get_warning(at_obj))
  e = list()
  graphics_obj = new_pcj_plot_object(func, args, data, e, w)

  if (add) {
    return(graphics_obj)
  } else {
    data = list(x = NULL, y = NULL, content = list(graphics_obj))

    args = list(type = "n") |>
      smth(dots) |>
      smth(use_if_no_theme(
        ylab = ylab,
        xlab = xlab,
        xlim = xlim,
        ylim = ylim,
        main = "Prior Predictive"
      )) |>
      smth(get_theme_args(
        func_name = "plot.default",
        func_namespace = "graphics",
        #class = class,
        data = data
    )) |>
      keep(get_supported_plot_default_params())

    if ("ylim" %in% names(args) && !is.null(args$ylim)) {
      tmp = check_lim(args$ylim, "y")
      if (!is.null(tmp)) stop(tmp)
      else rm(tmp)
    }

    if ("xlim" %in% names(args) && !is.null(args$xlim)) {
      tmp = check_lim(args$xlim, "x")
      if (!is.null(tmp)) stop(tmp)
      else rm(tmp)
    }

    data = named_list_rm(data, c("x", "y", "content"))
    args = c(list(x = NULL, y = NULL), args)

    # TODO set error/warnings
    plot_default_obj = new_pcj_plot_object("plot.default", args, data)
    plot_obj = list(plot_default_obj, graphics_obj)
    class(plot_obj) = "pcj_plot_object_list"
    return(plot_obj)
  }
}


plot_posterior_ = function(
    object,
    ...,
    x = NULL,
    stat = NULL,
    transform = list()
  )
{
  stopifnot(exprs = {
    is.pcj_process_capability_jags1(object) || is.pcj_model1(object)
    vek::is_chr_vec_xb1(x)

    #vek::is_num_vec_xyz(transform) # TODO
    #x %in% c("mu", "sigma") # TODO
  })

  # TODO throw error if model has error
  #if (is.pcj_model1(object))
  #  stopifnot(is.null(object$error))

  if (is.null(stat))
    stat = gaussian_density

  stopifnot(exprs = {
    !is.object(stat)
    is.function(stat)
    length(formals(stat)) > 0L
  })

  graphics = "lines"
  if (...length() > 0L)
    if (dots_names(...)[1L] == "")
      graphics = ...elt(1L)

  stopifnot(exprs = {
    vek::is_chr_vec_xb1(graphics)
    graphics %in% c("lines", "points")
  })

  dots = list(...)
  if (...length() > 0L)
    if (dots_names(...)[1L] == "")
      dots = dots[-1L]

  #browser()

  add = FALSE
  if ("add" %in% names(dots)) {
    add = dots$add
    dots$add = NULL
  }

  stopifnot(vek::is_lgl_vec_x1(add))

  var_name = x
  rm(x)
  var_info = get_var_info()
  var_bounds = get_var_bounds(var_name, var_info)

  #browser()

  if (is.pcj_model1(object)) {
    samples = get_sample.pcj_model1(object, var_name, "all")
  }
  else if (is.pcj_process_capability_jags1(object) &&
           !is.null(object$sequential_analysis))
  {
    last_i = length(object$sequential_analysis$fit)
    last_fit = object$sequential_analysis$fit[[last_i]]
    # TODO the error check
    #stopifnot(is.null(last_fit$error))
    samples = last_fit |>
      get_sample.pcj_model1(var_name, "all")
  }
  else if (is.pcj_process_capability_jags1(object) &&
           is.null(object$sequential_analysis))
  {
    # TODO the error check
    #stopifnot(is.null(object$pcj_model1$error))
    samples = get_sample.pcj_model1(object$pcj_model1, var_name, "all")
  }
  else {
    stop()
  }

  stat_args = list(samples)
  if ("..." %in% names(formals(stat))) {
    # Provide metadata about the variable.
    #stat_args = c(stat_args, list(
    #  var_name = var_name,
    #  lower_bound = var_bounds$lower,
    #  upper_bound = var_bounds$upper
    #))
  }

  stat_obj_ = pcj_safely(do.call(stat, stat_args))

  if (has_error(stat_obj_)) {
    meta = list(result = list(stat = stat_obj_))
    e = get_error(stat_obj_)
    w = get_warning(stat_obj_)
    return(new_pcj_plot_object(NULL, NULL, meta, e, w))
  }

  stat_obj = recursive_unclass(stat_obj_$result, 5L) # TODO adjust depth

  stat_check = check_stat_result(stat_obj, "stat")
  if (!is_empty(stat_check)) {
    data = list(result = list(stat = stat_obj_, stat_check = stat_check))
    e = stat_check
    w = get_warning(stat_obj_)
    return(new_pcj_plot_object(NULL, NULL, data, e, w))
  }

  at = NULL
  if ("at" %in% names(dots)) {
    at = dots$at
    dots$at = NULL
  }

  at_obj = get_at(at, samples, stat)
  if (has_error(at_obj)) {
    data = list(result = list(
      stat = stat_obj_, stat_check = stat_check, at = at_obj))

    e = get_error(at_obj)
    w = c(get_warning(stat_obj_), get_warning(at_obj))
    return(new_pcj_plot_object(NULL, NULL, data, e, w))
  }

  at = at_obj$result
  stopifnot({
    vek::is_num_vec_xyz(at) || is.null(at)
  })

  if (!is.null(at)) {
    if (graphics == "lines")
      stopifnot(length(at) > 1L)
    else if (graphics == "points")
      stopifnot(length(at) > 0L)
    else
      stop()

    # Store the original order of "at", so other vectorized arguments may be
    # re-ordered in the new order.
    at_order = order(at, na.last = TRUE, decreasing = FALSE, method = "auto")

    at = at[at_order]
    stopifnot(exprs = {
      !is.unsorted(at, na.rm = FALSE, strictly = TRUE)
    })
  }

  dens_obj = get_stat_density(stat_obj)

  if ("xlim" %in% names(dots) && !is.null(dots$xlim)) {
    xlim = dots$xlim
  } else {
    if (is.function(dens_obj))
      xlim = range(samples, na.rm = TRUE)
    else if (is_xy_density(dens_obj))
      xlim = range(dens_obj$x, na.rm = TRUE)
    else
      stop()
  }

  tmp = check_lim(xlim, "x")
  if (!is.null(tmp))
    stop(tmp)
  else
    rm(tmp)

  if (is.null(at))
    x = seq.default(xlim[1L], xlim[2L], length.out = 501L)
  else
    x = at

  type = switch(graphics, "lines" = "l", "points" = "p", stop())
  if ("type" %in% names(dots)) {
    type = dots$type
    dots$type = NULL
  }

  stopifnot(vek::is_chr_vec_xb1(type))
  if (graphics == "lines")
    stopifnot(type %in% c("l", "h"))
  else if (graphics == "points")
    stopifnot(type == "p")
  else
    stop()

  if (type == "h")
    stop('Setting "type" to "h" is currently not supported')

  if (is.function(dens_obj)) {
    y_obj = pcj_safely(dens_obj(x)) # TODO check condition
    y = y_obj$result
    stopifnot(exprs = {
      vek::is_num_vec_xyz(y)
    })

    xy = list(x = x, y = y)
  } else if (is_xy_density(dens_obj)) {
    xy = dens_obj[c("x", "y")]

    fill_zero_left = attr(dens_obj, "is_left_tail_zero", TRUE) %||% FALSE
    fill_zero_right = attr(dens_obj, "is_right_tail_zero", TRUE) %||% FALSE
    stopifnot(exprs = {
      vek::is_lgl_vec_x1(fill_zero_left)
      vek::is_lgl_vec_x1(fill_zero_right)
    })

    min_x = x[1L]
    max_x = x[length(x)]

      #browser()
    # Segment xy to be in between the range of x.
    is_left_cut = FALSE
    is_right_cut = FALSE
    if (min(xy$x, na.rm = TRUE) < min_x) {
      #browser()
      xy = slice_xy(xy$x, xy$y, min_x, FALSE)
      is_left_cut = TRUE
    }
    if (max(xy$x, na.rm = TRUE) > max_x) {
      #browser()
      xy = slice_xy(xy$x, xy$y, max_x, TRUE)
      is_right_cut = TRUE
    }

    # Extend the tails of the distribution up to xlim[1] and xlim[2] with points
    # of zero density.
    if (!is.null(dots$xlim)) {
      if (fill_zero_left && !is_left_cut &&
          xlim[1L] < min(xy$x, na.rm = TRUE))
      {
        lower = max(xlim[1L], var_bounds$lower, na.rm = FALSE)
        xy = list(x = c(lower, xy$x), y = c(0L, xy$y))
        #browser()
        rm(lower)
      }
      if (fill_zero_right && !is_right_cut &&
          xlim[2L] > max(xy$x, na.rm = TRUE))
      {
        upper = min(xlim[2L], var_bounds$upper, na.rm = FALSE)
        xy = list(x = c(xy$x, upper), y = c(xy$y, 0L))
        #browser()
        rm(upper)
      }
    }

    if (graphics == "points") {
      xy = stats::approx(xy$x, xy$y, xout = x, method = "linear")
    }
  } else {
    stop()
  }

  # TODO determining xlim when graphics == points
  xlab = get_var_lab(var_name)
  legend = xlab
  ylab = "Density"
  ylim = range(xy$y, na.rm = TRUE)
  data = c(xy, list(x_ = var_name, transform = transform, add = add,
                    stat = stat_obj_))

  args = dots |>
    smth(get_theme_args(
      func_name = graphics,
      func_namespace = "graphics",
      #class = class,
      data = data
    ))

  if (graphics == "lines")
    args = args |> keep(get_supported_lines_params())
  else if (graphics == "points")
    args = args |> keep(get_supported_points_params())
  else
    stop()

  data = named_list_rm(data, c("x", "y"))
  #if ("xlab" %in% names(args)) {
  #  data$legend = args$xlab
  #  args$xlab = NULL
  #} else {
  #  data$legend = xlab
  #}

  args = c(xy, args)
  func = switch(
    graphics,
    "lines" = "lines.default",
    "points" = "points.default",
    stop()
  )

  e = list()
  w = c(get_warning(at_obj), get_warning(stat_obj_))
  graphics_obj = new_pcj_plot_object(func, args, data, e, w)

  if (add) {
    return(graphics_obj)
  } else {
    data = list(x = NULL, y = NULL, content = list(graphics_obj))

    args = list(type = "n") |>
      smth(dots) |>
      smth(use_if_no_theme(
        xlab = xlab,
        ylab = ylab,
        xlim = xlim,
        ylim = ylim,
        main = "Posterior"
      )) |>
      smth(get_theme_args(
        func_name = "plot.default",
        func_namespace = "graphics",
        #class = class,
        data = data
      )) |>
      keep(get_supported_plot_default_params())

    if ("ylim" %in% names(args) && !is.null(args$ylim)) {
      tmp = check_lim(args$ylim, "y")
      if (!is.null(tmp)) stop(tmp)
      else rm(tmp)
    }

    if ("xlim" %in% names(args) && !is.null(args$xlim)) {
      tmp = check_lim(args$xlim, "x")
      if (!is.null(tmp)) stop(tmp)
      else rm(tmp)
    }

    data = named_list_rm(data, c("x", "y", "content"))
    args = c(list(x = NULL, y = NULL), args)
    # TODO add error/warnings
    plot_default_obj = new_pcj_plot_object("plot.default", args, data)
    plot_obj = list(plot_default_obj, graphics_obj)
    class(plot_obj) = "pcj_plot_object_list"
    return(plot_obj)
  }
}


plot_area = function(
    object,
    ...,
    x = NULL,
    distribution,
    stat = NULL,
    transform = list()
  )
{
  stopifnot(exprs = {
    # TODO add object checks
    vek::is_chr_vec_xb1(x)
    vek::is_chr_vec_xb1(distribution)
    distribution %in% c("prior", "prior_predictive", "posterior")

  })

  dots = list(...)
  #stopifnot(is_uniquely_named_list(dots))

  # TODO at logic
  #at = NULL
  #if ("at" %in% names(dots)) {
  #  at = dots$at
  #  stopifnot({
  #    vek::is_num_vec_xyz(at) || is.null(at)
  #  })
  #}

  add = FALSE
  if ("add" %in% names(dots)) {
    add = dots$add
    stopifnot(vek::is_lgl_vec_x1(add))
    dots$add = NULL
  }

  var_name = x
  rm(x)

  # TODO densitiy_func ommit when distr is prior
  args = list(object = object) |>
    c(dots) |>
    c(list(add = TRUE, x = var_name, stat = stat, transform = list()))

  func = switch(
    distribution,
    "prior" = plot_prior_,
    "posterior" = plot_posterior_,
    "prior_predictive" = plot_prior_predictive_,
    stop()
  )

  prior_lines = do.call(func, args)

  if (has_error(prior_lines)) {
    e = get_error(prior_lines)
    w = get_warning(prior_lines)
    return(new_pcj_plot_object(NULL, NULL, prior_lines$result$data, e, w))
  }

  x_ = prior_lines$result$args$x
  y_ = prior_lines$result$args$y
  x = c(x_[1L], x_, x_[length(x_)], x_[length(x_)], x_[1L])
  y = c(y_[1L], y_, y_[length(y_)], 0L, 0L)

  xlab = get_var_lab(var_name)
  ylab = "Density"
  ylim = range(y, na.rm = TRUE)
  xlim = range(x, na.rm = TRUE)
  main = switch(
    distribution,
    "prior" = "Prior",
    "prior_predictive" = "Prior Predictive",
    "posterior" = "Posterior",
    stop()
  )

  data = list(x = x, y = y, x_ = var_name, transform = transform, add = add)
  if (distribution %in% c("posterior", "prior_predictive")) {
    data$stat = prior_lines$result$data$stat
  }

  args = list(density = NULL, angle = 45L, fillOddEven = FALSE) |>
    smth(dots) |>
    smth(use_if_no_theme(
      #density = NULL, # No shading lines
      #angle = 45L,
      border = NA, # No border
      col = "#D3D3D3FF", # lightgray
      lty = 1L#,
      #fillOddEven = FALSE
    )) |>
    smth(get_theme_args(
      func_name = "polygon",
      func_namespace = "graphics",
      #class = class,
      data = data
    )) |>
    keep(get_supported_polygon_params())

  data = named_list_rm(data, c("x", "y"))
  #if ("xlab" %in% names(args)) {
  #  data$legend = args$xlab
  #  args$xlab = NULL
  #} else {
  #  data$legend = xlab
  #}

  args = c(list(x = x, y = y), args)
  e = list()
  w = get_warning(prior_lines)
  polygon_obj = new_pcj_plot_object("polygon", args, data, e, w)

  if (add) {
    return(polygon_obj)
  } else {
    data = list(x = NULL, y = NULL, content = list(polygon_obj))

    args = list(type = "n") |>
      smth(dots) |>
      smth(use_if_no_theme(
        ylab = ylab,
        xlab = xlab,
        xlim = xlim,
        ylim = ylim,
        main = main
      )) |>
      smth(get_theme_args(
        func_name = "plot.default",
        func_namespace = "graphics",
        #class = class,
        data = data
      )) |>
      keep(get_supported_plot_default_params())

    if ("ylim" %in% names(args) && !is.null(args$ylim)) {
      tmp = check_lim(args$ylim, "y")
      if (!is.null(tmp)) stop(tmp)
      else rm(tmp)
    }

    if ("xlim" %in% names(args) && !is.null(args$xlim)) {
      tmp = check_lim(args$xlim, "x")
      if (!is.null(tmp)) stop(tmp)
      else rm(tmp)
    }

    data = named_list_rm(data, c("x", "y", "content"))
    args = c(list(x = NULL, y = NULL), args)
    # TODO add error/warnings
    plot_default_obj = new_pcj_plot_object("plot.default", args, data)

    plot_obj = list(plot_default_obj, polygon_obj)
    class(plot_obj) = "pcj_plot_object_list"
    return(plot_obj)
  }
}



#x = c(from, x[is_in_interval], to, to, from),
#y = c(y_start, y[is_in_interval], y_end, 0L, 0L))
plot_prior_area = function(
    object,
    ...,
    x = NULL,
    transform = list()
  )
{
  plot_area(object, ..., distribution = "prior", x = x, stat = NULL, transform = transform)
}


plot_posterior_area = function(
    object,
    ...,
    x = NULL,
    stat = NULL,
    transform = list()
  )
{
  plot_area(object, ..., distribution = "posterior", x = x, stat = stat, transform = transform)
}


plot_prior_predictive_area = function(
    object,
    ...,
    x = NULL,
    stat = NULL,
    transform = list()
  )
{
  plot_area(object, ..., distribution = "prior_predictive", x = x, stat = stat, transform = transform)
}


#' @export
plot_sequential = function(
    object,
    ...,
    x = NULL,
    show_prior = TRUE,
    position = "stack",
    order = -1L,
    condition_action = "omit_error",
    stat = NULL,
    transform = list()
  )
{
  stopifnot(exprs = {
    is.pcj_process_capability_jags1(object)
    vek::is_chr_vec_xb1(x)
    #vek::is_num_vec_xyz(at) || is.null(at)
    #is.function(stat) # TODO
    vek::is_int_vec_x1(order)
    order %in% c(-1L, 1L)
    vek::is_chr_vec_xb(condition_action)
    all(condition_action %in% "omit_error", na.rm = FALSE)
    # TODO transform
    # TODO add
  })

  # TODO check it's actually sequential object

  dots = list(...)
  if (length(dots) > 0L)
    stop('Parameter "..." currently serves no purpose')

  add = FALSE
  #if ("add" %in% names(dots)) {
  #  add = dots$add
  #  stopifnot(vek::is_lgl_vec_x1(add))
  #  dots$add = NULL
  #}

  var_name = x
  rm(x)

  if (var_name %in% c("mu", "sigma")) {
    prior_key = sprintf("%s%s", "prior_", var_name)
    prior_obj = object$prior_study$result[[prior_key]]
    if (is_pcj_point_prior(prior_obj))
      stop(paste0("Function 'plot_sequential' currently doesn't support ",
                  "plotting variables with point priors"))
  }

  # 1. -------------------------------------------------------------------------
  # Create the individual plots to obtain information about their heights.
  plots = list()
  if (show_prior) {
    if (var_name %in% c("mu", "sigma")) {
      # Create prior plot.
      args = c(list(object, add = TRUE, x = var_name, transform = list()), dots)
      plots[[1L]] = do.call(plot_prior, args)

    } else {
      # Create prior predictive plot.
      args = c(list(
        object, "lines", add = TRUE, x = var_name, stat = stat,
        transform = list()
        #class = class
      ), dots)

      plots[[1L]] = do.call(plot_prior_predictive, args)
    }
  }

  # Create posterior plots.
  seq_an = object$sequential_analysis
  args = list(at = NULL, add = TRUE, x = var_name, stat = stat, transform = list()
              #class = class
  ) |>
    c(dots)

  for (i in seq_along(seq_an$fit)) {
    fit_i = seq_an$fit[[i]]
    plt = do.call(plot_posterior, c(list(fit_i), args))
    plots[[length(plots) + 1L]] = plt
  }

  has_error = sapply(plots, \(p) has_error(p),
                     simplify = TRUE, USE.NAMES = FALSE)

  if ("omit_error" %in% condition_action) {
    if (any(has_error, na.rm = FALSE))
      plots = plots[!has_error]
  }

  # TODO handle the case where all plots carried errors.
  if (length(plots) == 0L) {
    # ...
  }

  # Obtain the height of each plot, and the overall range of all x values.
  y_max = sapply(plots, \(x) pcj_plot_object_axis_lim_raw(x, "y", "max"))
  x_min = sapply(plots, \(x) pcj_plot_object_axis_lim_raw(x, "x", "min")) |>
    min(na.rm = FALSE)

  x_max = sapply(plots, \(x) pcj_plot_object_axis_lim_raw(x, "x", "max")) |>
    max(na.rm = FALSE)

  # The position argument determines the height of each plot (except the last).
  #adj_y_max = rep_len(position, length(y_max) - 1L) |> c(y_max[length(y_max)])

  # Scale the height of each plot (except the last) by a factor.
  #k = y_max[1:(length(y_max) - 1L)] * position
  #adj_y_max = c(k, y_max[length(y_max)])

  if (position == "ridges") {
    # The height of each plot (except the last) is averaged.
    y_max_mean = mean(y_max[1:(length(y_max) - 1L)], na.rm = FALSE)
    y_max_mean = y_max_mean * .75
    adj_y_max = rep_len(y_max_mean, length(y_max) - 1L) |>
      c(y_max[length(y_max)])
  }
  else if (position == "stack") {
    # Plots are stacked.
    adj_y_max = y_max
  } else {
    stop()
  }

  #plot_height = sum(adj_y_max, na.rm = FALSE)

  transform_y = cumsum(c(0L, adj_y_max))[1:length(adj_y_max)]
  plot_height = max(transform_y + y_max, na.rm = FALSE)
  xlim = c(x_min, x_max)

  # 2. -------------------------------------------------------------------------
  # Create the individual plot objects.
  plots = list()

  show_prior_ = show_prior
  if (show_prior_) {
    omit_prior = "omit_error" %in% condition_action && has_error[1L]
    show_prior_ = show_prior_ && !omit_prior
  }

  if (show_prior_) {
    prior_area = NULL
    prior_plot = NULL
    if (var_name %in% c("mu", "sigma")) {
      # Create the prior plot.
      args = list(
        object, at = NULL, add = TRUE, xlim = xlim, x = var_name, transform = list()
        #class = class,

      ) |>
        c(rm_plot_default_params(dots))

      prior_plot = do.call(plot_prior, args)

      if (!is_pcj_point_prior(prior_obj))
        prior_area = do.call(plot_prior_area, args)
    } else {
      # Create the prior predictive plot.
      args = list(
        object,
        at = NULL,
        add = TRUE,
        xlim = xlim
      ) |>
        c(keep(dots, get_supported_lines_params())) |>
        c(list(
          x = var_name,
          stat = stat,
          transform = list()
          #class = class,
        ))

      prior_area = do.call(plot_prior_predictive_area, args)
      prior_plot = do.call(plot_prior_predictive, args)
    }

    prior_objects = list(prior_plot)
    if (!is.null(prior_area))
      prior_objects = c(list(prior_area), prior_objects)

    #prior_obj_has_error = sapply(prior_objects, \(p) return(!is.null(p$error)),
    #                         simplify = TRUE, USE.NAMES = FALSE)

    plots[[1L]] = prior_objects
  }

  # Create posterior plots.
  args = list(
    at = NULL, add = TRUE, xlim = xlim) |>
    c(keep(dots, get_supported_lines_params())) |>
    c(list(x = var_name, stat = stat,
           transform = list()))

  j = if (show_prior) 1L else 0L
  for (i in seq_along(seq_an$fit)) {
    fit_i = seq_an$fit[[i]]
    args$transform = list(offset = c(0L, transform_y[i + j]))
    args3 = c(list(fit_i), args)

    post_area = do.call(plot_posterior_area, args3)
    post_curve = do.call(plot_posterior, args3)

    plots[[length(plots) + 1L]] = list(post_area, post_curve)
  }

  has_error_ = sapply(plots, \(p_li) {
    stopifnot(exprs = {
      is_list(p_li)
      length(p_li) > 0L
    })

    has_p_li_error = sapply(p_li, \(p) has_error(p),
                             simplify = TRUE, USE.NAMES = FALSE)

    return(any(has_p_li_error, na.rm = FALSE))
  }, simplify = TRUE, USE.NAMES = FALSE)

  # If no error was thrown initially (when obtaining xlim/ylim of each), then
  # reproducing the plots with the only difference being an offset and expanded
  # 'xlim' is unlikely to cause errors. Hence, new errors at this point are
  # currently treated as 'unexpected', but more error handling logic and
  # recovery attempts might be inserted later.
  if (any(has_error_, na.rm = FALSE)) {
    stop('One or more of the sequential plots produced an unexpected error')
  }

  if (order == -1L)
    plots = rev(plots)

  plots = unlist(plots, recursive = FALSE, use.names = FALSE)

  # 3. -------------------------------------------------------------------------
  if (!add) {
    # TODO render or not based on "ann" param?
    # Create the y-axis.
    labels = seq_an$sequential_params$at
    if (show_prior)
      labels = c(0L, labels)

    if (any(has_error, na.rm = FALSE)) {
      stopifnot(length(labels) == length(has_error))
      labels = labels[!has_error]
      #transform_y = transform_y[!has_error]
    }

    data = list(side = 2L, at = transform_y, labels = labels)
    # TODO
    args = list()
    #args = dots |>
    #  smth(get_theme_args(
    #    func_name = "axis",
    #    func_namespace = "graphics",
    #    #class =
    #    data = data
    #  )) |>
    #  rm_plot_default_params() # TODO

    args = list(side = 2L, at = transform_y, labels = labels) |> c(args)
    data = named_list_rm(data, c("side", "at", "labels"))
    axis_plot_obj = new_pcj_plot_object("axis", args, data)

    plots = c(list(axis_plot_obj), plots)


    # Create the plot.default.
    data = list(x = NULL, y = NULL, content = plots)

    args = list(type = "n", yaxt = "n") |>
      smth(dots) |>
      smth(use_if_no_theme(
        xlim = xlim,
        ylim = c(0, plot_height),
        xlab = get_var_lab(var_name),
        ylab = "N"
      )) |>
      smth(get_theme_args( # TODO remove non plot.default dots
        func_name = "plot.default",
        func_namespace = "graphics",
        #class =
        data = data
      )) |>
      keep(c(get_supported_plot_default_params(), "yaxt"))

    data = named_list_rm(data, c("x", "y", "content"))
    args = c(list(x = NULL, y = NULL), args)
    # TODO add error/warnings
    plot_default_obj = new_pcj_plot_object("plot.default", args, data)

    plots = c(list(plot_default_obj), plots)
  }

  class(plots) = "pcj_plot_object_list"
  return(plots)
}


# TODO
plot_trace = function(object, x, transform = list(), ...) {

}




# TODO
create_histogram_density_func = function(
    breaks = "Sturges",
    include.lowest = TRUE,
    fuzz = 1e-07,
    density = NULL
  )
{
  args = list(
    x = y, breaks = breaks, freq = freq, probability = probability,
    include.lowest = include.lowest, right = right, fuzz = fuzz,
    density = density, plot = FALSE
  )

  o = do.call(graphics::hist.default, args)
  # TODO deal with weird breaks length
  x = c(o$breaks[1L], o$breaks, o$breaks[length(o$breaks)])
  y = c(0L, o$density, o$density[length(o$density)], 0L)

  return(structure(
    list(x = x, y = y),
    is_left_tail_zero = TRUE,
    is_right_tail_zero = TRUE
  ))
}


#new_pcj_plot_object = function(func, args, data) {
#  structure(
#    list(func = func, args = args, data = data),
#    class = "pcj_plot_object"
#  )
#}

new_pcj_plot_object = function(
    func,
    args,
    data,
    error = list(),
    warnings = list()
  )
{
  structure(
    list(
      condition = c(error, warnings),
      result = list(func = func, args = args, data = data)
    ),
    class = "pcj_plot_object"
  )
}


#' @export
get_error.pcj_plot_object = get_error_
#' @export
get_warning.pcj_plot_object = get_warning_
#' @export
get_message.pcj_plot_object = get_message_
#' @export
get_condition.pcj_plot_object = get_condition_


preprocess_pcj_plot_object = function(object) {
  stopifnot(is.pcj_plot_object(object))

  args = object$result$args

  if ("offset" %in% names(object$result$data$transform)) {
    offset = object$result$data$transform$offset

    if (object$result$func == "arrows") {
      if (vek::is_num_vec(args$x0))
        args$x0 = args$x0 + offset[1L]

      if (vek::is_num_vec(args$x1))
        args$x1 = args$x1 + offset[1L]

      if (vek::is_num_vec(args$y0))
        args$y0 = args$y0 + offset[2L]

      if (vek::is_num_vec(args$y1))
        args$y1 = args$y1 + offset[2L]

    } else {
      if (vek::is_num_vec(args$x))
        args$x = args$x + offset[1L]

      if (vek::is_num_vec(args$y))
        args$y = args$y + offset[2L]
    }

    # TODO add else. and axis offset?
  }

  object$result$args = args
  return(object)
}


#' @export
plot.pcj_plot_object = function(object) {
  stopifnot(is.pcj_plot_object(object))

  if (has_error(object))
    stop(get_error(object)[[1L]])

  if (has_warning(object)) {
    for (w in get_warning(object))
      warning(w)
  }

  func = switch(
    object$result$func,
    "plot.default" = graphics::plot.default,
    "plot.xy" = graphics::plot.xy,
    "lines.default" = graphics::lines.default,
    "points.default" = graphics::points.default,
    "polygon" = graphics::polygon,
    "arrows" = graphics::arrows,
    "axis" = graphics::axis,
    stop()
  )

  obj = preprocess_pcj_plot_object(object)

  do.call(func, obj$result$args)

  return(invisible(object))
}


get_graphics_driver = function() {
  graphics_driver = getOption("pcj.graphics_driver", "graphics")
  stopifnot(exprs = {
    vek::is_chr_vec_xb1(graphics_driver)
    graphics_driver %in% c("graphics", "ggplot2")
  })

  if (graphics_driver == "ggplot2")
    requireNamespace("ggplot2", quietly = FALSE)

  return(graphics_driver)
}


#' @export
plot.pcj_plot_object_list = function(object) {
  stopifnot(is.pcj_plot_object_list(object))

  graphics_driver = get_graphics_driver()

  if (graphics_driver == "graphics") {
    for (obj in object) {
      plot(obj)
    }
  } else if (graphics_driver == "ggplot2") {
    ggobj = gg_build(object)
    plot(ggobj)
  } else {
    stop()
  }

  #return(invisible(object))
}





#' @export
print.pcj_plot_object_list = function(object) {
  plot(object)
}


#' @export
print.pcj_plot_object = function(object) {
  plot(object)
}


get_var_lab = function(var_name) {
  if (var_name %in% c("mu", "sigma")) {
    var_info = get_var_info()
    return(str2expression(var_info[var_name, "name_r_expr"]))
  }
  else {
    return(str2expression(pci::pci_info[var_name, "name_r_expr"]))
  }
}


gaussian_density = function(x, ...) {
  # Intentionally left missing: width, from, to
  # TODO handle warnings?
  # TODO use pairlist for explicit missing args?
  result = stats::density.default(
    x, bw = "nrd0", adjust = 1, kernel = "gaussian", weights = NULL,
    window = kernel, give.Rkern = FALSE, subdensity = FALSE,
    warnWbw = var(weights) > 0, n = 512, cut = 3, ext = 4,
    old.coords = FALSE, na.rm = FALSE
  )

  attr(result, "is_left_tail_zero") = TRUE
  attr(result, "is_right_tail_zero") = TRUE
  return(result)
}


# TODO managage weird length behaviors
slice_xy = function(x, y, a, side) {
  stopifnot(exprs = {
    vek::is_num_vec(x)
    vek::is_num_vec(y)
    vek::is_num_vec_xyz1(a)
    vek::is_lgl_vec_x1(side)
    !is.unsorted(x, na.rm = TRUE, strictly = TRUE) # TODO strictly?
    length(x) == length(y)
  })

  rx = abs(x - a) |> rank(ties.method = "first")
  rxi1 = which.min(rx)
  rxi2 = which(rx == 2L, FALSE, FALSE)
  if (rxi1 > rxi2) {
    tmp = rxi1
    rxi1 = rxi2
    rxi2 = tmp
  }

  x_ = x[c(rxi1, rxi2)]
  y_ = y[c(rxi1, rxi2)]
  k = stats::approx(x_, y_, xout = a, method = "linear")$y

  x_sliced = if (side) c(x[1:rxi1], a) else c(a, x[rxi2:length(x)])
  y_sliced = if (side) c(y[1:rxi1], k) else c(k, y[rxi2:length(x)])
  return(list(
    x = x_sliced,
    y = y_sliced
  ))
}


slice2_xy = function(x, y, x_a, x_b) {
  stopifnot(exprs = {
    vek::is_num_vec(x)
    vek::is_num_vec(y)
    !is.unsorted(x, TRUE, TRUE)
    length(x) == length(y)
    vek::is_num_vec_xyz1(x_a) || is.null(x_a)
    vek::is_num_vec_xyz1(x_b) || is.null(x_b)
  })

  xy = list(x = x, y = y)
  if (!is.null(x_a))
    xy = slice_xy(xy$x, xy$y, x_a, FALSE)
  if (!is.null(x_b))
    xy = slice_xy(xy$x, xy$y, x_b, TRUE)

  return(xy)
}


# TODO improve logic
auto_xlim_pcj_jags_dist = function(dist_obj) {
  stopifnot(is.pcj_jags_dist(dist_obj)) # TODO is valid check

  trunc_a = NULL
  trunc_b = NULL
  x = numeric(0L)
  x_range = c(NA_real_, NA_real_)
  if (!is.null(dist_obj$truncation)) {
    trunc_a = dist_obj$truncation[[1L]]
    trunc_b = dist_obj$truncation[[2L]]
  } else {
    set.seed(1L)
    x = pcj_jags_dist_rng(dist_obj, 1000L)
    x_range = range(x, na.rm = TRUE)
  }

  xlim = c(NA_real_, NA_real_)
  xlim[1L] = if (is.null(trunc_a)) x_range[1L] else trunc_a
  xlim[2L] = if (is.null(trunc_b)) x_range[2L] else trunc_b
  return(xlim)
}


splice_truncation_xy = function(x, y, a, b, ya, yb) {
  if (!is.null(a)) {
    x_gt_a = x > a
    x = c(x[1L], a, a, x[x_gt_a])
    y = c(0L, 0L, ya, y[x_gt_a])
  }
  if (!is.null(b)) {
    x_lt_b = x < b
    x = c(x[x_lt_b], b, b, x[length(x)])
    y = c(y[x_lt_b], yb, 0L, 0L)
  }

  return(list(x = x, y = y))
}


smth = function(x, y) {
  stopifnot(exprs = {
    is_uniquely_named_list(x)
    is_uniquely_named_list(y)
  })

  must_copy = !(names(y) %in% names(x))
  if (any(must_copy, na.rm = FALSE))
    return(c(x, y[must_copy]))
  else
    return(x)
}


get_plot_default_args = function() {
  list(
    ylab = NULL, xlim = NULL, ylim = NULL, sub = NULL, main = NULL,
    axes = TRUE, xgap.axis = NA, ygap.axis = NA, panel.first = NULL,
    panel.last = NULL, log = "", asp = NA, frame.plot = TRUE, ann = TRUE
  )
}


use_if_no_theme = function(...) {
  f = getOption("pcj.theme_func", default = NULL)
  if (is.function(f))
    return(list())
  else
    return(list(...))
}


get_graphics_args = function(...) {
  dots = list(...)
  func_name = dots$func_name %||% stop()

  gr_args = list()
  if (func_name == "plot.default")
    gr_args = get_plot_default_args()

  #gr_args = smth(gr_args, graphics::par(no.readonly = TRUE))
  gr_args = smth(gr_args, list(xaxp = NULL, yaxp = NULL))
  return(gr_args)
}


get_theme_args = function(...) {
  f = getOption("pcj.theme_func", default = get_graphics_args)
  stopifnot(is.function(f))
  return(f(...))
}


get_supported_polygon_params = function() {
  c("col", "border", "lty", "lwd ", "lend")
}


get_plot_default_params = function() {
  c("xlim", "ylim", "log", "main", "sub", "xlab", "ylab", "ann", "axes",
    "frame.plot", "panel.first", "panel.last", "asp", "xgap.axis", "ygap.axis")
}


get_supported_lines_params = function() {
  c("type", "lty", "lwd", "col", "lend")
}


get_supported_points_params = function() {
  c("pch", "col", "bg", "lty", "lwd", "lend")
}


get_supported_arrows_params = function() {
  c("lty", "lwd", "col", "lend")
}

get_supported_plot_default_params = function() {
  # "log"
  # "panel.first", "panel.last", "asp", "xgap.axis", "ygap.axis"
  c("xlim", "ylim", "main", "sub", "xlab", "ylab", "ann",
    "axes", "frame.plot")
}


rm_secondary_plot_default_params = function(x) {
  stopifnot(is_list(x))
  if (length(x) == 0L)
    return(x)

  return(x[names(x) %in% get_plot_default_params()])
}


rm_plot_default_params = function(x) {
  named_list_rm(x, get_plot_default_params())
}


rm_unnamed = function(x) {
  stopifnot(is_list(x))
  if (is.null(names(x)) || length(x) == 0L)
    return(list())
  else
    return(x[names(x) != ""])
}


named_list_rm = function(x, entries) {
  stopifnot(exprs = {
    is_named_list(x)
    vek::is_chr_vec_xb(entries)
  })

  return(x[!(names(x) %in% entries)])
}


keep = function(x, entries) {
  stopifnot(exprs = {
    is_named_list(x)
    vek::is_chr_vec_xb(entries)
  })

  return(x[names(x) %in% entries])
}


get_var_bounds = function(var_name, var_info) {
  return(Find(
    \(k) k$type == "bounds" && k$target == var_name,
    var_info[var_name, "attributes"],
    right = FALSE,
    nomatch = NULL
  ))
}


is_within_var_bounds = function(x, var_bounds) {
  if (var_bounds$is_upper_inclusive)
    a = max(x, na.rm = FALSE) <= var_bounds$upper
  else
    a = max(x, na.rm = FALSE) < var_bounds$upper

  if (var_bounds$is_lower_inclusive)
    b = min(x, na.rm = FALSE) >= var_bounds$lower
  else
    b = min(x, na.rm = FALSE) > var_bounds$lower

  return(a && b)
}


pcj_plot_object_axis_lim_raw = function(object, side, lim) {
  stopifnot(exprs = {
    is.pcj_plot_object(object)
    vek::is_chr_vec_xb1(side)
    vek::is_chr_vec_xb1(lim)
    side %in% c("x", "y")
    lim %in% c("min", "max")
  })

  g = switch(lim, "min" = min, "max" = max, stop())
  arrows_params = sprintf("%s%s", side, c("0", "1")) # e.g. c('y0', 'y1')

  if (object$result$func == "arrows") {
    value0 = object$result$args[[arrows_params[1L]]]
    value1 = object$result$args[[arrows_params[2L]]]
    if (vek::is_num_vec(value0) || vek::is_num_vec(value1))
      return(g(value0, value1, na.rm = FALSE)) # TODO what if is NA or NaN?
    else
      return(NA) # TODO what return value?
  }
  else {
    values = object$result$args[[side]]
    if (vek::is_num_vec(values))
      return(g(values, na.rm = TRUE))
    else
      return(NA)
  }
}


# TODO list_get also exists
list_get2 = function(x, key, default = NULL) {
  stopifnot(exprs = {
    is_list(x)
    vek::is_chr_vec_xb1(key)
  })

  if (key %in% names(x))
    return(x$key)
  else
    return(default)
}


is_xy_density = function(x) {
  is_list(x) && is_uniquely_named_list(x[c("x", "y")]) &&
    all(c("x", "y") %in% names(x), na.rm = FALSE)
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


check_lim = function(x, label) {
  stopifnot(exprs = {
    vek::is_chr_vec_xb1(label)
    label %in% c("x", "y")
  })

  if (is.null(x))
    return(invisible(NULL))

  if (!vek::is_num_vec_xyz(x)) {
    msg = sprintf('"%slim" must be a base-R numeric vector', label)
    return(typeError(msg))
  }

  if (!(length(x) == 2L)) {
    msg = sprintf('"%slim" must be of length 2', label)
    return(valueError(msg))
  }

  if (x[1L] > x[2L]) {
    msg = paste0('Inverting the %s-axis by setting "%slim[1] > %slim[2]" is',
                 ' currently not supported', collapse = NULL,
                 recycle0 = FALSE) |>
      sprintf(label, label, label)

    return(valueError(msg))
  }

  return(invisible(NULL))
}

