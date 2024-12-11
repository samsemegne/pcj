

#' @export
plot_prior_density.pcj_process_capability_model1 = function(
    object,
    ...,
    graphics = "lines",
    what = NULL
  )
{
  stopifnot(exprs = {
    is.pcj_process_capability_model1(object)
    vek::is_chr_vec_xb1(graphics)
    vek::is_chr_vec_xb1(what)
    graphics %in% c("lines", "points", "area", "arrows")
    what %in% stats::variable.names(object, "prior")
  })

  prior_obj = new_pcj_distribution(get_prior(object, what))

  if (is_pcj_single_point_prior(prior_obj)) {
    stop('Plotting a point prior is currently not supported')
    #return(plot_point_prior(object, ..., what = what, offset = offset))
  }
  else {
    if (graphics %in% c("lines", "points")) {
      return(plot_prior_(
        object,
        ...,
        graphics = graphics,
        what = what
      ))
    }
    else if (graphics == "area") {
      return(plot_prior_area(
        object,
        ...,
        what = what
      ))
    }
    else {
      stop()
    }
  }
}


#' @export
plot_prior_predictive_density.pcj_process_capability_model1 = function(
    object,
    ...,
    graphics = "lines",
    what = NULL
  )
{
  stopifnot(exprs = {
    is.pcj_process_capability_model1(object)
    vek::is_chr_vec_xb1(graphics)
    vek::is_chr_vec_xb1(what)
    graphics %in% c("lines", "area", "points")
    what %in% stats::variable.names(object, "prior_predictive")
  })

  if (graphics %in% c("lines", "points")) {
    return(plot_prior_predictive_(
      object,
      ...,
      graphics = graphics,
      what = what
    ))
  }
  else if (graphics == "area") {
    return(plot_prior_predictive_area(
      object,
      ...,
      what = what
    ))
  }
  else {
    stop()
  }
}


#' @export
plot_posterior_density.pcj_process_capability_model1 = function(
    object,
    ...,
    graphics = "lines",
    what = NULL
  )
{
  stopifnot(exprs = {
    is.pcj_process_capability_model1(object)
    vek::is_chr_vec_xb1(graphics)
    vek::is_chr_vec_xb1(what)
    graphics %in% c("lines", "points", "area", "arrows")
    what %in% stats::variable.names(object, "posterior")
  })

  if (what %in% stats::variable.names(object, "prior")) {
    prior_obj = new_pcj_distribution(get_prior(object, what))

    if (is_pcj_single_point_prior(prior_obj)) {
      stop('Plotting the posterior for a point prior is currently not supported')
    }

    if (is_pcj_single_point_prior(prior_obj)) {
      stopifnot(graphics %in% c("lines", "arrows"))
      dots = list(...)
      stopifnot(is_all_unique(names(dots)))
      if (!("main" %in% names(dots)))
        dots$main = "Posterior"

      args = c(
        list(object),
        dots,
        list(graphics = graphics, what = what)
      )

      plt = do.call(plot_point_prior, args)
      return(plt)
    }
  }

  if (graphics %in% c("lines", "points")) {
    return(plot_posterior_(
      object,
      ...,
      graphics = graphics,
      what = what
    ))
  }
  else if (graphics == "area") {
    return(plot_posterior_area(
      object,
      ...,
      what = what
    ))
  } else {
    stop()
  }
}


# TODO
# TODO apply offset
plot_point_prior = function(
    object,
    ...,
    x = NULL,
    offset = c(0L, 0L)
  )
{
  stopifnot(exprs = {
    is.pcj_process_capability_model1(object)
    vek::is_chr_vec_xb1(x)
    x %in% stats::variable.names(object, "prior")
  })

  offset_check = check_offset(offset)
  if (!is_empty(offset_check))
    stop(offset_check[[1L]])

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

  at = NULL
  if ("at" %in% names(dots)) {
    at = dots$at
    dots$at = NULL
  }

  at_info = get_at_info(at)
  at = at_info$at
  n = at_info$n
  by = at_info$by
  from = at_info$from
  to = at_info$to



  var_name = x
  rm(x)
  var_info = get_variable_info()

  prior_obj = new_pcj_distribution(get_prior(object, var_name))
  stopifnot(is_pcj_single_point_prior(prior_obj))


  if (length(at) > 1L) {
    msg = paste0('If "length(at) > 0", then the "at" argument for a single ',
                 'point prior may currently only contain a single value',
                 collapse = NULL, recycle0 = FALSE)
    stop(msg)
  }

  if (length(at) == 1L && at != prior_obj$value) {
    msg = paste0('If "length(at) > 0", then "at" must currently equal the ',
                 'value of the single point prior',
                 collapse = NULL, recycle0 = FALSE)
    stop(msg)
  }

  # TODO
  #if (length(at) == 0L) {
  #  if ()
  #}

  xlim = c(-1L, 1L) + prior_obj$value
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

    xy = list(x = c(prior_obj$value, prior_obj$value), y = c(0L, 1L))
    meta = xy |> c(list(what = what, offset = offset, add = add))

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
        data = meta
      )) |>
      keep(get_supported_lines_params())

    meta = named_list_rm(meta, c("x", "y"))
    args = c(xy, args)
    graphics_obj = new_pcj_plot_object("lines.default", args, meta, list())
  }
  else if (graphics == "arrows") {
    x01y01 = list(x0 = prior_obj$value, y0 = 0L, x1 = prior_obj$value, y1 = 1L)
    meta = x01y01 |> c(list(what = what, offset = offset, add = add))

    args = dots |>
      smth(get_theme_args(
        func_name = graphics,
        func_namespace = "graphics",
        #class = class,
        data = meta
      )) |>
      keep(get_supported_arrows_params())

    data = named_list_rm(meta, c("x0", "y0", "x1", "y1"))
    args = c(x01y01, args)
    graphics_obj = new_pcj_plot_object("arrows", args, meta, list())
  } else {
    stop()
  }

  if (add) {
    return(graphics_obj)
  } else {
    meta = list(x = NULL, y = NULL, content = list(graphics_obj))

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
        data = meta
      )) |>
      keep(get_supported_plot_default_params())

    if ("ylim" %in% names(args) && !is.null(args$ylim)) {
      tmp = check_lim(args$ylim, "y")
      throw_first_error(tmp)
      rm(tmp)
    }

    if ("xlim" %in% names(args) && !is.null(args$xlim)) {
      tmp = check_lim(args$xlim, "x")
      throw_first_error(tmp)
      rm(tmp)
    }

    meta = named_list_rm(meta, c("x", "y", "content"))
    args = c(list(x = NULL, y = NULL), args)
    plot_default_obj = new_pcj_plot_object("plot.default", args, meta, list())

    return(new_pcj_plot_object_list(list(plot_default_obj, graphics_obj)))
  }
}


plot_prior_ = function(
    object,
    ...,
    graphics = "lines",
    what = NULL
  )
{
  stopifnot(exprs = {
    is.pcj_process_capability_model1(object)
    vek::is_chr_vec_xb1(graphics)
    vek::is_chr_vec_xb1(what)
    graphics %in% c("lines", "points")
    what %in% stats::variable.names(object, "prior")
  })

  dots = list(...)
  stopifnot(is_uniquely_named_list(dots))

  offset = c(0L, 0L)
  if ("offset" %in% names(dots)) {
    offset = dots$offset
    dots$offset = NULL
  }

  tfm_check = check_offset(offset)
  throw_first_error(tfm_check)
  rm(tfm_check)

  add = FALSE
  if ("add" %in% names(dots)) {
    add = dots$add
    dots$add = NULL
  }

  stopifnot(vek::is_lgl_vec_x1(add))

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

  var_info = get_variable_info()
  var_bounds = get_var_bounds(what, var_info)

  interval_str = paste0(">= min(truncation_lower_closed, sample_min) & ",
                        "<= max(truncation_upper_closed, sample_max)")

  location_str = c(
    interval_str,
    "truncation_lower_open",
    "truncation_upper_open"
  )

  # Default "x".
  x = structure(location_str, n = 100L, sample_size = 10000L)

  if ("x" %in% names(dots)) {
    x = dots$x
    dots$x = NULL
  }

  prior = new_pcj_distribution(get_prior(object, what))
  stopifnot(exprs = {
    is.pcj_jags_distribution(prior$value)
    !is_pcj_single_point_prior(prior)
  })

  at_res = get_at3(x, prior)

  if (has_error(at_res)) {
    return(new_pcj_plot_object(NULL, NULL, list(), get_condition(at_res)))
  }

  x = get_result(at_res) |> as.numeric() # TODO
  names(x) = names(get_result(at_res))
  #browser()
  x = x[!is.na(x)] # TODO temporary

  stopifnot(exprs = {
    vek::is_num_vec_xyz(x)
    length(x) > 1L
    !is.unsorted(x, na.rm = FALSE, strictly = FALSE)
    is_within_var_bounds(x, var_bounds)
  })

  # Calculate the densities.
  y = pcj_jags_distribution_pdf(prior$value, x)

  if ("truncation_lower_open" %in% names(x)) {
    i = which(names(x) == "truncation_lower_open")
    stopifnot(vek::is_int_vec_x1(i))
    y[i] = 0L
    rm(i)
  }
  if ("truncation_upper_open" %in% names(x)) {
    i = which(names(x) == "truncation_upper_open")
    stopifnot(vek::is_int_vec_x1(i))
    y[i] = 0L
    rm(i)
  }

  # Cut x values outside of the supported range for the given prior, and their
  # associated y values.
  # (Hard coded).
  if (what == "sigma" && x[1L] < 0L) {
    ya = pcj_jags_distribution_pdf(prior_obj, 0L)
    y = c(ya, y[x > 0L])
    x = c(0L, x[x > 0L])
  }

  if (offset[1L] != 0L)
    x = x + offset[1L]
  if (offset[2L] != 0L)
    y = y + offset[2L]

  if ("xlim" %in% names(dots))
    xlim = dots$xlim
  else
    xlim = range(x, na.rm = FALSE)

  tmp = check_lim(xlim, "x")
  throw_first_error(tmp)
  rm(tmp)

  ylab = "Density"
  xlab = get_var_lab(what)
  #legend = xlab
  ylim = range(y, na.rm = TRUE)
  data = list(x = x, y = y, what = what, offset = offset, add = add)

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
    graphics,
    "lines" = "lines.default",
    "points" = "points.default",
    stop()
  )

  graphics_obj = new_pcj_plot_object(func, args, data, list())

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
      throw_first_error(tmp)
      rm(tmp)
    }

    if ("xlim" %in% names(args) && !is.null(args$xlim)) {
      tmp = check_lim(args$xlim, "x")
      throw_first_error(tmp)
      rm(tmp)
    }

    data = named_list_rm(data, c("x", "y", "content"))
    args = c(list(x = NULL, y = NULL), args)
    plot_default_obj = new_pcj_plot_object("plot.default", args, data, list())

    return(new_pcj_plot_object_list(list(plot_default_obj, graphics_obj)))
  }
}


plot_prior_predictive_ = function(
    object,
    ...,
    graphics = "lines",
    what = NULL
  )
{
  stopifnot(exprs = {
    is.pcj_process_capability_model1(object)
    vek::is_chr_vec_xb1(graphics)
    vek::is_chr_vec_xb1(what)
    graphics %in% c("lines", "points")
    what %in% stats::variable.names(object, "prior_predictive")
  })

  dots = list(...)
  stopifnot(is_uniquely_named_list(dots))

  offset = c(0L, 0L)
  if ("offset" %in% names(dots)) {
    offset = dots$offset
    dots$offset = NULL
  }

  tfm_check = check_offset(offset)
  throw_first_error(tfm_check)
  rm(tfm_check)

  add = FALSE
  if ("add" %in% names(dots)) {
    add = dots$add
    dots$add = NULL
  }

  stopifnot(vek::is_lgl_vec_x1(add))

  type = switch(
    graphics,
    "lines" = "l",
    "points" = "p",
    stop()
  )

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

  if(has_error(get_result(object)$prior_predictive)) {
    meta = list()
    e = list(simpleError('"object$prior_predictive" exited with errors'))
    w = list()
    return(new_pcj_plot_object(NULL, NULL, meta, object, e, w))
  }

  stat = get_result(object)$stat

  var_info = get_variable_info()
  var_bounds = get_var_bounds(what, var_info)

  samples = get_sample(get_result(object)$prior_predictive, what)
  stat_result = obtain_stat_result(samples, stat)

  if (has_error(stat_result)) {
    meta = list(result = list(stat = stat_result))
    return(new_pcj_plot_object(NULL, NULL, meta, get_condition(stat_result)))
  }

  # Default "x".
  x = structure(c(">= q0 & <= q1"), n = 1000L)

  if ("x" %in% names(dots)) {
    x = dots$x
    dots$x = NULL
  }

  at_result = get_at2(x, stat_result)

  if (has_error(at_result)) {
    meta = list(result = list(
      stat = stat_result, x = at_result
    ))

    cond = c(get_condition(at_result), get_condition(stat_result))
    return(new_pcj_plot_object(NULL, NULL, meta, cond))
  }

  x = get_result(at_result) |> as.numeric() # rhs is temporary
  stopifnot({
    vek::is_num_vec_xyz(x)
    !is.unsorted(x, na.rm = FALSE, strictly = FALSE)
    is_within_var_bounds(x, var_bounds)
  })

  # Note. When lines type "h" is implemented, length 1 is ok.
  if (graphics == "lines")
    stopifnot(length(x) > 1L)
  else if (graphics == "points")
    stopifnot(length(x) > 0L)
  else
    stop()

  dens_obj = get_result(stat_result)$density
  dens_obj = dens_obj() |> unclass()

  if (is.function(dens_obj)) {
    y = dens_obj(x) # TODO safely()
    xy = list(x = x, y = y)

  } else if (is_xy_density(dens_obj)) {
    # Get the non-parameteric density point estimates from the density object.
    xy = dens_obj[c("x", "y")]
    # TODO check xy

    fill_zero_left = attr(dens_obj, "is_left_tail_zero", TRUE) %||% FALSE
    fill_zero_right = attr(dens_obj, "is_right_tail_zero", TRUE) %||% FALSE
    stopifnot(exprs = {
      vek::is_lgl_vec_x1(fill_zero_left)
      vek::is_lgl_vec_x1(fill_zero_right)
    })

    if (length(x) > 1L) {

      min_x = x[1L]
      max_x = x[length(x)]

      # Slice xy to be in between the range of x.
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

      # Extend the tails of the distribution up to min(x) and max(x) with points
      # of zero density.
      if (fill_zero_left && !is_left_cut &&
          min_x < min(xy$x, na.rm = TRUE))
      {
        #lower = max(min_x, var_bounds$lower, na.rm = FALSE)
        lower = min_x
        xy = list(x = c(lower, xy$x), y = c(0L, xy$y))
        rm(lower)
      }
      if (fill_zero_right && !is_right_cut &&
          max_x > max(xy$x, na.rm = TRUE))
      {
        #upper = min(max_x, var_bounds$upper, na.rm = FALSE)
        upper = max_x
        xy = list(x = c(xy$x, upper), y = c(xy$y, 0L))
        rm(upper)
      }
    }

    if (graphics == "points") {
      xy = stats::approx(xy$x, xy$y, xout = x, method = "linear")
    }
  } else {
    stop()
  }

  if (offset[1L] != 0L)
    xy$x = xy$x + offset[1L]
  if (offset[2L] != 0L)
    xy$y = xy$y + offset[2L]

  xlim = NULL
  if ("xlim" %in% names(dots))
    xlim = dots$xlim
  else if (length(x) == 1L)
    xlim = c(-.5, .5) + xy$x
  else if (length(x) > 1L)
    xlim = range(xy$x, na.rm = TRUE)
  else
    stop()

  xlim_check = check_lim(xlim, "x")
  throw_first_error(xlim_check)
  rm(xlim_check)

  xlab = get_var_lab(what)
  #legend = xlab
  ylab = "Density"
  ylim = range(xy$y, na.rm = TRUE)
  data = xy |>
    c(list(
      what = what, offset = offset, add = add,
      result = list(stat = stat_result, at = at_result)
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

  cond = c(
    get_condition(get_result(object)$prior_predictive),
    get_condition(stat_result),
    get_condition(at_result)
  )

  graphics_obj = new_pcj_plot_object(func, args, data, cond)

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
      throw_first_error(tmp)
      rm(tmp)
    }

    if ("xlim" %in% names(args) && !is.null(args$xlim)) {
      tmp = check_lim(args$xlim, "x")
      throw_first_error(tmp)
      rm(tmp)
    }

    data = named_list_rm(data, c("x", "y", "content"))
    args = c(list(x = NULL, y = NULL), args)
    plot_default_obj = new_pcj_plot_object("plot.default", args, data, list())

    return(new_pcj_plot_object_list(list(plot_default_obj, graphics_obj)))
  }
}


plot_posterior_ = function(
    object,
    ...,
    graphics = "lines",
    what = NULL
  )
{
  stopifnot(exprs = {
    is.pcj_process_capability_model1(object)
    vek::is_chr_vec_xb1(graphics)
    vek::is_chr_vec_xb1(what)
    graphics %in% c("lines", "points")
    what %in% stats::variable.names(object, "posterior")
  })

  dots = list(...)
  stopifnot(is_uniquely_named_list(dots))

  offset = c(0L, 0L)
  if ("offset" %in% names(dots)) {
    offset = dots$offset
    dots$offset = NULL
  }

  tfm_check = check_offset(offset)
  throw_first_error(tfm_check)
  rm(tfm_check)

  add = FALSE
  if ("add" %in% names(dots)) {
    add = dots$add
    dots$add = NULL
  }

  stopifnot(vek::is_lgl_vec_x1(add))

  type = switch(
    graphics,
    "lines" = "l",
    "points" = "p",
    stop()
  )

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

  if (has_error(object)) {
    cond = list(simpleError('The model exited with errors'))
    return(new_pcj_plot_object(NULL, NULL, list(), cond))
  }

  stat = get_result(object)$stat

  var_info = get_variable_info()
  var_bounds = get_var_bounds(what, var_info)

  samples = get_sample(object, what, "posterior", "all")

  stat_result = obtain_stat_result(samples, stat)

  if (has_error(stat_result)) {
    meta = list(result = list(stat = stat_result))
    e = get_error(stat_result)
    w = get_warning(stat_result)
    return(new_pcj_plot_object(NULL, NULL, meta, object, e, w))
  }

  # Default "x".
  x = structure(c(">= q0 & <= q1"), n = 1000L)

  if ("x" %in% names(dots)) {
    x = dots$x
    dots$x = NULL
  }

  at_result = get_at2(x, stat_result)

  if (has_error(at_result)) {
    meta = list(result = list(
      stat = stat_result, x = at_result
    ))

    e = get_error(at_result)
    w = c(get_warning(stat_result), get_warning(at_result))
    return(new_pcj_plot_object(NULL, NULL, meta, object, e, w))
  }

  x = get_result(at_result) |> as.numeric() # TODO rhs is temporary
  stopifnot({
    vek::is_num_vec_xyz(x)
    !is.unsorted(x, na.rm = FALSE, strictly = FALSE)
    is_within_var_bounds(x, var_bounds)
  })

  # Note. When lines type "h" is implemented, length 1 is ok.
  if (graphics == "lines")
    stopifnot(length(x) > 1L)
  else if (graphics == "points")
    stopifnot(length(x) > 0L)
  else
    stop()

  dens_obj = get_result(stat_result)$density
  dens_obj = dens_obj() |> unclass()

  if (is.function(dens_obj)) {
    y_obj = pcj_safely(dens_obj(x)) # TODO check condition
    y = get_result(y_obj)
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

    if (length(x) > 1L) {
      min_x = x[1L]
      max_x = x[length(x)]

      # Slice xy to be in between the range of x.
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

      # Extend the tails of the distribution up to min(x) and max(x) with points
      # of zero density.
      if (fill_zero_left && !is_left_cut &&
          min_x < min(xy$x, na.rm = TRUE))
      {
        #lower = max(min_x, var_bounds$lower, na.rm = FALSE)
        lower = min_x
        xy = list(x = c(lower, xy$x), y = c(0L, xy$y))
        rm(lower)
      }
      if (fill_zero_right && !is_right_cut &&
          max_x > max(xy$x, na.rm = TRUE))
      {
        #upper = min(max_x, var_bounds$upper, na.rm = FALSE)
        upper = max_x
        xy = list(x = c(xy$x, upper), y = c(xy$y, 0L))
        rm(upper)
      }
    }

    if (graphics == "points") {
      xy = stats::approx(xy$x, xy$y, xout = x, method = "linear") # TODO
    }
  } else {
    stop()
  }

  if (offset[1L] != 0L)
    xy$x = xy$x + offset[1L]
  if (offset[2L] != 0L)
    xy$y = xy$y + offset[2L]

  xlim = NULL
  if ("xlim" %in% names(dots))
    xlim = dots$xlim
  else if (length(x) == 1L)
    xlim = c(-.5, .5) + xy$x
  else if (length(x) > 1L)
    xlim = range(xy$x, na.rm = TRUE)
  else
    stop()

  xlim_check = check_lim(xlim, "x")
  throw_first_error(xlim_check)
  rm(xlim_check)

  xlab = get_var_lab(what)
  #legend = xlab
  ylab = "Density"
  ylim = range(xy$y, na.rm = TRUE)
  data = c(xy, list(x_ = what, offset = offset, add = add,
                    stat = stat_result))

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

  cond = c(
    get_condition(object),
    get_condition(at_result),
    get_condition(stat_result)
  )

  graphics_obj = new_pcj_plot_object(func, args, data, cond)

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
      throw_first_error(tmp)
      rm(tmp)
    }

    if ("xlim" %in% names(args) && !is.null(args$xlim)) {
      tmp = check_lim(args$xlim, "x")
      throw_first_error(tmp)
      rm(tmp)
    }

    data = named_list_rm(data, c("x", "y", "content"))
    args = c(list(x = NULL, y = NULL), args)
    plot_default_obj = new_pcj_plot_object("plot.default", args, data, list())

    return(new_pcj_plot_object_list(list(plot_default_obj, graphics_obj)))
  }
}


plot_area = function(
    object,
    ...,
    what = NULL,
    distribution
  )
{
  stopifnot(exprs = {
    is.pcj_process_capability_model1(object)
    vek::is_chr_vec_x1(what)
    vek::is_chr_vec_x1(distribution)
    distribution %in% c("prior", "prior_predictive", "posterior")
    what %in% stats::variable.names(object, distribution)
  })

  dots = list(...)
  stopifnot(is_uniquely_named_list(dots))

  offset = c(0L, 0L)
  if ("offset" %in% names(dots)) {
    offset = dots$offset
    dots$offset = NULL
  }

  tfm_check = check_offset(offset)
  throw_first_error(tfm_check)
  rm(tfm_check)

  add = FALSE
  if ("add" %in% names(dots)) {
    add = dots$add
    stopifnot(vek::is_lgl_vec_x1(add))
    dots$add = NULL
  }

  stat = get_result(object)$stat

  # TODO densitiy_func ommit when distr is prior
  args = list(object = object) |>
    c(dots) |>
    c(list(graphics = "lines", what = what, offset = c(0L, 0L), add = TRUE))

  func = switch(
    distribution,
    "prior" = plot_prior_,
    "posterior" = plot_posterior_,
    "prior_predictive" = plot_prior_predictive_,
    stop()
  )

  prior_lines = do.call(func, args)

  if (has_error(prior_lines)) {
    return(new_pcj_plot_object(
      NULL, NULL, get_result(prior_lines)$meta, get_condition(prior_lines)))
  }

  x_ = get_result(prior_lines)$args$x
  y_ = get_result(prior_lines)$args$y
  x = c(x_[1L], x_, x_[length(x_)], x_[length(x_)], x_[1L])
  y = c(y_[1L], y_, y_[length(y_)], 0L, 0L)

  if (offset[1L] != 0L)
    x = x + offset[1L]
  if (offset[2L] != 0L)
    y = y + offset[2L]

  xlab = get_var_lab(what)
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

  data = list(x = x, y = y, what = what, offset = offset, add = add)
  if (distribution %in% c("posterior", "prior_predictive")) {
    data$stat = get_result(prior_lines)$meta$stat
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
    keep(get_supported_polygon_params()) # TODO fillOddEven etc probably lost now

  data = named_list_rm(data, c("x", "y"))
  args = c(list(x = x, y = y), args)
  polygon_obj = new_pcj_plot_object(
    "polygon", args, data, get_condition(prior_lines))

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
      throw_first_error(tmp)
      rm(tmp)
    }

    if ("xlim" %in% names(args) && !is.null(args$xlim)) {
      tmp = check_lim(args$xlim, "x")
      throw_first_error(tmp)
      rm(tmp)
    }

    data = named_list_rm(data, c("x", "y", "content"))
    args = c(list(x = NULL, y = NULL), args)
    plot_default_obj = new_pcj_plot_object("plot.default", args, data, list())

    return(new_pcj_plot_object_list(list(plot_default_obj, polygon_obj)))
  }
}


#x = c(from, x[is_in_interval], to, to, from),
#y = c(y_start, y[is_in_interval], y_end, 0L, 0L))
plot_prior_area = function(
    object,
    ...,
    what = NULL,
    offset = c(0L, 0L)
  )
{
  return(plot_area(
    object,
    ...,
    distribution = "prior",
    what = what,
    offset = offset
  ))
}


plot_posterior_area = function(
    object,
    ...,
    what = NULL,
    offset = c(0L, 0L)
  )
{
  return(plot_area(
    object,
    ...,
    distribution = "posterior",
    what = what,
    offset = offset
  ))
}


plot_prior_predictive_area = function(
    object,
    ...,
    what = NULL,
    offset = c(0L, 0L)
  )
{
  return(plot_area(
    object,
    ...,
    distribution = "prior_predictive",
    what = what,
    offset = offset
  ))
}


#' @export
plot_sequential_procedure = function(
    object,
    ...,
    what = NULL,
    show_prior = TRUE,
    display = "stack",
    draw_order = -1L,
    condition_action = "omit_if_error",
    offset = c(0L, 0L)
  )
{
  stopifnot(exprs = {
    is.pcj_sequential_procedure(object)
    length(get_result(object)$fit) > 0L
    vek::is_chr_vec_xb1(what)
    what %in% stats::variable.names(get_result(object)$fit[[1L]], "posterior")
    vek::is_lgl_vec_x1(show_prior)
    vek::is_chr_vec_xb1(display)
    display %in% c("stack", "ridges")
    vek::is_int_vec_x1(draw_order)
    draw_order %in% c(-1L, 1L)
    vek::is_chr_vec_xb(condition_action)
    all(condition_action %in% "omit_if_error", na.rm = FALSE)
  })

  tfm_check = check_offset(offset)
  throw_first_error(tfm_check)
  rm(tfm_check)


  dots = list(...)
  if (length(dots) > 0L)
    stop('Parameter "..." currently serves no purpose')

  add = FALSE # To be implemented later
  #if ("add" %in% names(dots)) {
  #  add = dots$add
  #  stopifnot(vek::is_lgl_vec_x1(add))
  #  dots$add = NULL
  #}

  first_model = get_result(object)$fit[[1L]]

  stopifnot(exprs = {
    has_probability_density(first_model, "posterior", what)
  })

  if (has_probability_mass(first_model, "posterior", what)) {
    stop(paste0('plot_sequential_procedure() currently does not support',
                'plotting variable that have a probability mass component'))
  }

  # 1. -------------------------------------------------------------------------
  # Create the individual plots to obtain information about their heights.
  plots = list()
  if (show_prior) {
    if (what %in% stats::variable.names(first_model, "prior")) {
      # Create prior plot.
      args = list(
        first_model,
        add = TRUE,
        graphics = "lines",
        what = what,
        offset = c(0L, 0L)
      )

      plots[[1L]] = do.call(plot_prior_density, args)

    }
    else if (what %in% stats::variable.names(first_model, "prior_predictive")) {
      # Create prior predictive plot.
      args = list(
        first_model,
        add = TRUE,
        graphics = "lines",
        what = what,
        offset = c(0L, 0L)
      )

      plots[[1L]] = do.call(plot_prior_predictive_density, args)
    } else {
      stop()
    }
  }

  # Create posterior plots.
  seq_an = get_result(object)
  args = list(add = TRUE, graphics = "lines", what = what, offset = c(0L, 0L))

  for (i in seq_along(seq_an$fit)) {
    fit_i = seq_an$fit[[i]]
    plt = do.call(plot_posterior_density, c(list(fit_i), args))
    plots[[length(plots) + 1L]] = plt
  }

  has_error__ = sapply_(plots, has_error)

  if ("omit_if_error" %in% condition_action) {
    if (any(has_error__, na.rm = FALSE))
      plots = plots[!has_error__]
  }

  # TODO handle the case where all plots carried errors.
  if (length(plots) == 0L) {
    # ...
  }

  # Obtain the height of each plot, and the overall range of all x values.
  y_max = sapply_(plots, \(x) pcj_plot_object_axis_lim_raw(x, "y", "max"))
  x_min = sapply_(plots, \(x) pcj_plot_object_axis_lim_raw(x, "x", "min")) |>
    min(na.rm = FALSE)

  x_max = sapply(plots, \(x) pcj_plot_object_axis_lim_raw(x, "x", "max")) |>
    max(na.rm = FALSE)

  ## The display argument determines the height of each plot (except the last).
  #adj_y_max = rep_len(display, length(y_max) - 1L) |> c(y_max[length(y_max)])

  ## Scale the height of each plot (except the last) by a factor.
  #k = y_max[1:(length(y_max) - 1L)] * display
  #adj_y_max = c(k, y_max[length(y_max)])

  if (display == "ridges") {
    # The height of each plot (except the last) is averaged.
    y_max_mean = mean(y_max[1:(length(y_max) - 1L)], na.rm = FALSE)
    y_max_mean = y_max_mean * .75
    adj_y_max = rep_len(y_max_mean, length(y_max) - 1L) |>
      c(y_max[length(y_max)])
  }
  else if (display == "stack") {
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
    omit_prior = "omit_if_error" %in% condition_action && has_error__[1L]
    show_prior_ = show_prior_ && !omit_prior
  }

  if (show_prior_) {
    prior_area = NULL
    prior_plot = NULL
    if (what %in% stats::variable.names(first_model, "prior")) {
      # Create the prior plot.
      args = list(
        first_model,
        add = TRUE,
        what = what,
        offset = c(0L, 0L)
      )

      prior_plot = do.call(plot_prior_density, args)

      if (!is_pcj_single_point_prior(prior_obj))
        prior_area = do.call(plot_prior_area, args)

    } else if (what %in% stats::variable.names(first_model, "prior_predictive"))
    {
      # Create the prior predictive plot.
      args = list(
        first_model,
        add = TRUE,
        what = what,
        offset = c(0L, 0L)
      )

      prior_area = do.call(plot_prior_predictive_area, args) # TODO
      prior_plot = do.call(plot_prior_predictive_density, args)
    } else {
      stop()
    }

    prior_objects = list(prior_plot)
    if (!is.null(prior_area))
      prior_objects = c(list(prior_area), prior_objects)

    #prior_obj_has_error = sapply(prior_objects, \(p) return(!is.null(p$error)),
    #                         simplify = TRUE, USE.NAMES = FALSE)

    plots[[1L]] = prior_objects
  }

  # Create posterior plots.
  args = list(add = TRUE, what = what, offset = c(0L, 0L))

  j = if (show_prior) 1L else 0L
  for (i in seq_along(seq_an$fit)) {
    fit_i = seq_an$fit[[i]]
    args$offset = c(0L, transform_y[i + j])
    args3 = c(list(fit_i), args)

    post_area = do.call(plot_posterior_area, args3) # TODO
    post_curve = do.call(plot_posterior_density, args3)

    plots[[length(plots) + 1L]] = list(post_area, post_curve)
  }

  has_error_ = sapply_(plots, \(p_li) {
    stopifnot(exprs = {
      is_list(p_li)
      length(p_li) > 0L
    })

    has_p_li_error = sapply_(p_li, has_error)
    return(any(has_p_li_error, na.rm = FALSE))
  })

  if (any(has_error_, na.rm = FALSE)) {
    stop('One or more of the sequential plots produced an unexpected error')
  }

  if (draw_order == -1L)
    plots = rev(plots)

  plots = unlist(plots, recursive = FALSE, use.names = FALSE)

  # 3. -------------------------------------------------------------------------
  if (add) {
    stop()
  }
  else {
    # TODO render or not based on "ann" param?
    # Create the y-axis.
    labels = seq_an$at
    if (show_prior)
      labels = c(0L, labels)

    if (any(has_error__, na.rm = FALSE)) {
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
    axis_plot_obj = new_pcj_plot_object("axis", args, data, list())

    plots = c(list(axis_plot_obj), plots)


    # Create the plot.default.
    data = list(x = NULL, y = NULL, content = plots)

    args = list(type = "n", yaxt = "n") |>
      #smth(dots) |>
      smth(use_if_no_theme(
        xlim = xlim,
        ylim = c(0, plot_height),
        xlab = get_var_lab(what),
        ylab = "Data Size"
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
    plot_default_obj = new_pcj_plot_object("plot.default", args, data, list())

    plots = c(list(plot_default_obj), plots)
  }

  return(new_pcj_plot_object_list(plots))
}


# TODO
#plot_trace = function(object, x, offset = c(0L, 0L), ...) {
#
#}




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


get_var_lab = function(var_name) {
  stopifnot(vek::is_chr_vec_x1(var_name))

  if (var_name %in% c("mu", "sigma")
      || startsWith(var_name, "p_nonconformance"))
  {
    var_info = get_variable_info()
    return(str2expression(var_info[var_name, "name_r_expr"]))
  }
  else {
    return(str2expression(pci::pci_info[var_name, "name_r_expr"]))
  }
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
    panel.last = NULL, log = "", asp = NA, frame.plot = FALSE, ann = TRUE
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
    \(k) return(k$type == "bounds" && k$target == var_name),
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

  if (get_result(object)$func == "arrows") {
    value0 = get_result(object)$args[[arrows_params[1L]]]
    value1 = get_result(object)$args[[arrows_params[2L]]]
    if (vek::is_num_vec(value0) || vek::is_num_vec(value1))
      return(g(value0, value1, na.rm = FALSE)) # TODO what if is NA or NaN?
    else
      return(NA) # TODO what return value?
  }
  else {
    values = get_result(object)$args[[side]]
    if (vek::is_num_vec(values))
      return(g(values, na.rm = TRUE))
    else
      return(NA)
  }
}


# TODO
is_xy_density = function(x) {
  is_list(x) && is_uniquely_named_list(x[c("x", "y")]) &&
    all(c("x", "y") %in% names(x), na.rm = FALSE)
}


check_lim = function(x, label) {
  stopifnot(exprs = {
    vek::is_chr_vec_xb1(label)
    label %in% c("x", "y")
  })

  if (is.null(x))
    return(new_pcj_check(list()))

  if (!vek::is_num_vec_xyz(x)) {
    msg = sprintf('"%slim" must be a base-R numeric vector', label)
    return(new_pcj_check(list(typeError(msg))))
  }

  if (!(length(x) == 2L)) {
    msg = sprintf('"%slim" must be of length 2', label)
    return(new_pcj_check(valueError(msg)))
  }

  if (x[1L] > x[2L]) {
    msg = paste0('Inverting the %s-axis by setting "%slim[1] > %slim[2]" is',
                 ' currently not supported', collapse = NULL,
                 recycle0 = FALSE) |>
      sprintf(label, label, label)

    return(new_pcj_check(valueError(msg)))
  }

  return(new_pcj_check(list()))
}


check_offset = function(x, label = "offset") {
  stopifnot(exprs = {
    vek::is_chr_vec_xb1(label)
  })

  bag = list()

  if (!vek::is_num_vec(x)) {
    msg = sprintf('"%s" must be a base-R numeric vector', label)
    bag = c(bag, list(typeError(msg)))
    return(new_pcj_check(bag))
  }

  if (length(x) != 2L) {
    msg = sprintf('"%s" must be of length 2', label)
    bag = c(bag, list(valueError(msg)))
    return(new_pcj_check(bag))
  }

  if (!vek::is_num_vec_xyz(x)) {
    msg = sprintf('"%s" must be finite', label)
    bag = c(bag, list(valueError(msg)))
    return(new_pcj_check(bag))
  }

  return(new_pcj_check(bag))
}


parse_prior_inequality_str = function(x, prior, attrs) {
  f = Vectorize(
    parse_prior_inequality_str_,
    vectorize.args = "x",
    SIMPLIFY = FALSE,
    USE.NAMES = TRUE
  )

  return(f(x, prior, attrs))
}


parse_prior_inequality_str_ = function(x, prior, attrs) {
  stopifnot(exprs = {
    vek::is_chr_vec_x1(x)
    is_of_mono_class(prior, "pcj_distribution")
    is.pcj_jags_distribution(prior$value)
    # TODO attrs
  })

  x = strsplit_(x, "&") |>
    trimws()

  if (length(x) > 2L) {
    # TODO return error result
  }

  if (length(x) == 1L) {
    return(parse_prior_inequality_str1(x, prior, attrs))
  } else if (length(x) == 2L) {
    ineq1 = parse_prior_inequality_str1(x[1L], prior, attrs)
    ineq2 = parse_prior_inequality_str1(x[2L], prior, attrs)

    # TODO handle errors
    # TODO handle invalid case, e.g. > 3 & > 4

    tail1 = get_result(ineq1)$interval
    tail2 = get_result(ineq2)$interval
    interval = intersect_lower_and_upper_tail(tail1, tail2)

    obj = list(
      interval = interval,
      value_name_a = get_result(ineq1)$value_name,
      value_name_b = get_result(ineq2)$value_name
    )

    #browser()

    return(new_pcj_result(
      obj,
      c(get_condition(ineq1), get_condition(ineq2)),
      c(get_output(ineq1), get_output(ineq2))
    ))
  }
}


get_at3 = function(at, prior) {
  stopifnot(exprs = {
    vek::is_chr_vec_x(strip_attributes_if_valuetype(at))
    is_of_mono_class(prior, "pcj_distribution")
    is.pcj_jags_distribution(prior$value)
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
    attrs = attributes(at) %||% list()

    foo = pcj_safely(parse_prior_inequality_str(x, prior, attrs))
    #browser()
    return(foo)
  }


  is_prior_code_ = \(x) {
    x %in% c("sample_mean.default", "sample_median.default",
      "truncation_lower_open", "truncation_lower_closed",
      "truncation_upper_open", "truncation_upper_closed",
      "sample_min", "sample_max")
  }

  parse_prior_code_ = \(x) {
    attributes(x) = attributes(at)
    k = obtain_prior_at(prior, x)
    val = do.call(c, lapply(k, get_result))
    cond = do.call(c, lapply(k, get_condition))
    out = do.call(c, lapply(k, get_output))
    return(new_pcj_result(val, cond, out))
  }

  map = list(
    literal = list(chr_is_num, parse_literal_),
    inequality = list(chr_starts_with_inequality, parse_inequality_),
    code = list(is_prior_code_, parse_prior_code_)
    # TODO mean/median.default
  )

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

  # Create sequences from inequality strings representing concrete intervals,
  # e.g. ">= 1 & <= 10" will effectively translate into
  # seq(from = 1, to = 10, by = attr(at, "by")) or
  # seq(from = 1, to = 10, length.out = attr(at, "n")).
  extra_points = list()
  if (has_error(results$inequality)) {
    # TODO ...
    #browser()
  } else {
    ineq_res = get_result(results$inequality) # list of pcj_result
    extra_points = lapply(ineq_res, \(res) {
      if (has_error(res))
        return(list())

      res_val = get_result(res)
      interval = res_val$interval
      k = create_sequence_from_interval(interval, seq_args$n, seq_args$by)
      names(k) = as.character(k)

      #browser()
      if (interval$is_a_inclusive && !is.na(res_val$value_name_a)) {
        names(k)[1L] = get_result(res)$value_name_a
      }

      if (interval$is_b_inclusive && !is.na(res_val$value_name_b)) {
        names(k)[length(k)] = get_result(res)$value_name_b
      }

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

  values = sort_prior_truncation(values)

  # Handle duplicate values.
  # Note. This requirement might change in the future, but is enforced currently
  # for simplicity.
  unique_check_condition = list()
  # Note. truncation points such as "truncation_lower_open" and
  # "truncation_lower_closed" are equal and are exempt from the duplicate value
  # rule.
  not_truncation = !startsWith(names(values), "truncation")
  if (!is_all_unique(values[not_truncation])) {
    browser()
    error_msg = '"x" must resolve to unique values, currently'
    unique_check_condition = list(simpleError(error_msg))
    rm(error_msg)
  }

  rm(not_truncation)

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


parse_prior_inequality_str1 = function(x, prior, attrs) {
  stopifnot(exprs = {
    vek::is_chr_vec_x1(x)
    is_of_mono_class(prior, "pcj_distribution")
    is.pcj_jags_distribution(prior$value)
    # TODO attrs
    is_uniquely_named_list(attrs)
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

  rhs_res = parse_prior_expr_str(rhs, prior, attrs)
  #browser()
  if (has_error(rhs_res)) {
    rhs_res$result = NULL
    return(rhs_res)
  }

  # TODO case xyz

  inequality_str = sprintf("%s %s", lhs, as.character(get_result(rhs_res)))
  interval = inequality_to_interval1(inequality_str)
  stopifnot(!is.null(interval))

  val_name = names(get_result(rhs_res))
  if (is.null(val_name) || (!is.null(val_name) && val_name == ""))
    val_name = NA_character_

  val = list(
    interval = interval,
    value_name = val_name
  )

  return(new_pcj_result(val, get_condition(rhs_res), get_output(rhs_res)))
}


parse_prior_expr_str = function(x, prior, attrs) {
  stopifnot(exprs = {
    vek::is_chr_vec_x1(x)
    is_of_mono_class(prior, "pcj_distribution")
    is.pcj_jags_distribution(prior$value)
    # TODO attrs
    is_uniquely_named_list(attrs)
  })

  y = trimws(x)

  # case, a numeric: "1"
  # case, a numeric: "NA"
  if (chr_is_num(y)) {
    res = pcj_safely({ as.numeric(y) })
    stopifnot(vek::is_num_vec(get_result(res)) && length(get_result(res)) == 1L)
    names(res$result) = y
    #browser()
    return(res)
  }

  codes = c("sample_mean.default", "sample_median.default",
            "truncation_lower_open", "truncation_lower_closed",
            "truncation_upper_open", "truncation_upper_closed",
            "sample_min", "sample_max")

  if (y %in% codes) {
    res = obtain_prior_at(prior, y)[[1L]]
    return(res)
  }

  # case, a call: min(1, 2)
  # case, a call with stat codes: min(mean, 2)
  if (startsWith(y, "min(") && endsWith(y, ")")) {
    #f = \(...) return(min(..., na.rm = TRUE))
    f = \(k) return(which.min(k))
  } else if (startsWith(y, "max(") && endsWith(y, ")")) {
    #f = \(...) return(max(..., na.rm = TRUE))
    f = \(k) return(which.max(k))
  } else {
    return(new_pcj_result(NULL, list(simpleError('Failed to parse expression'))))
  }

  z = substr(y, 5L, nchar(y) - 1L) |>
    strsplit_(",") |>
    trimws()

  attributes(z) = attrs
  # TODO literal parsing
  res = obtain_prior_at(prior, z)
  cond = do.call(c, lapply(res, get_condition))
  out = do.call(c, lapply(res, get_output))
  if (has_error(new_pcj_result(NULL, cond, out))) {
    return(new_pcj_result(NULL, cond, out))
  }

  values = lapply(res, get_result)
  values = values[!is.na(values)]

  stopifnot(exprs = {
    all(sapply_(values, vek::is_num_vec), na.rm = FALSE)
    all(sapply_(values, \(k) return(length(k) == 1L)), na.rm = FALSE)
  })

  i = f(values)
  stopifnot(exprs = {
    vek::is_int_vec_x1(i)
  })

  val = values[i]

  #browser()
  return(new_pcj_result(val, cond, out))
}


obtain_prior_at = function(prior, at) {
  stopifnot(exprs = {
    is_of_mono_class(prior, "pcj_distribution")
    is.pcj_jags_distribution(prior$value)
    !is.object(at)
    vek::is_chr_vec_x(strip_attributes_if_valuetype(at))
    is_all_unique(at)
  })

  codes = c("sample_mean.default", "sample_median.default",
            "truncation_lower_open", "truncation_lower_closed",
            "truncation_upper_open", "truncation_upper_closed",
            "sample_min", "sample_max")

  is_code_str = at %in% codes
  #is_inequality_str = at |>
  #  strip_attributes_if_valuetype() |>
  #  chr_starts_with_inequality()

  #is_lit_str = !is_code_str & !is_inequality_str
  #lit_str = at[is_lit_str]
  code_str = at[is_code_str]

  # Parse numeric literals, e.g. c("1", "1.5").
  #lit = integer(0L)
  #if (length(lit_str) > 0L) {
  #  lit_res = pcj_safely({ as.numeric(lit_str) })
  #  throw_first_error(lit_res)
  #
  #  lit = get_result(lit_res)
  #  stopifnot(vek::is_num_vec_xyz(lit))
  #  names(lit) = lit_str
  #}

  #seq_args = get_by_and_n(at, "at")
  sample_size = attr(at, "sample_size", TRUE)
  stopifnot(exprs = {
    # TODO perform checks
  })

  if (is.null(sample_size) &&
      any(startsWith(code_str, "sample_"), na.rm = FALSE))
  {
    stop(paste0('"at" must carry a "sample_size" attribute if any of ',
                'its elements starts with "sample_"'))
  }

  res = list()

  if ("truncation_lower_open" %in% code_str ||
      "truncation_lower_closed" %in% code_str)
  {
    trunc_a = NA_integer_
    if ("truncation" %in% names(prior$value)) {
      trunc_a = prior$value$truncation[[1L]] %||% NA_integer_
    }

    trunc_a_res = new_pcj_result(trunc_a)

    if ("truncation_lower_open" %in% code_str)
      res = c(res, list(`truncation_lower_open` = trunc_a_res))

    if ("truncation_lower_closed" %in% code_str)
      res = c(res, list(`truncation_lower_closed` = trunc_a_res))
  }

  if ("truncation_upper_open" %in% code_str ||
      "truncation_upper_closed" %in% code_str)
  {
    trunc_b = NA_integer_
    if ("truncation" %in% names(prior$value)) {
      trunc_b = prior$value$truncation[[2L]] %||% NA_integer_
    }

    trunc_b_res = new_pcj_result(trunc_b)

    if ("truncation_upper_open" %in% code_str)
      res = c(res, list(`truncation_upper_open` = trunc_b_res))

    if ("truncation_upper_closed" %in% code_str)
      res = c(res, list(`truncation_upper_closed` = trunc_b_res))
  }

  if (!is.null(sample_size)) {
    jags_dist = prior$value
    samples = pcj_jags_distribution_rng(jags_dist, sample_size)

    if ("sample_mean.default" %in% code_str) {
      mean.default_res = pcj_safely({ mean.default(samples) })
      # TODO perform checks
      res = c(res, list(sample_mean.default = mean.default_res))
    }

    if ("sample_median.default" %in% code_str) {
      median.default_res = pcj_safely({ stats::median.default(samples) })
      # TODO perform checks
      res = c(res, list(sample_median.default = median.default_res))
    }


    if ("sample_min" %in% code_str) {
      sample_min = min(samples, na.rm = FALSE)
      sample_min_res = new_pcj_result(sample_min)
      res = c(res, list(sample_min = sample_min_res))
    }

    if ("sample_max" %in% code_str) {
      sample_max = max(samples, na.rm = FALSE)
      sample_max_res = new_pcj_result(sample_max)
      res = c(res, list(sample_max = sample_max_res))
    }

  }

  #code_val = sapply_(res, get_result)
  #names(code_val) = names(res)

  #at_val = c(code_val, lit)
  #at_val = code_val

  #cond = do.call(c, lapply(res, get_condition))
  #out = do.call(c, lapply(res, get_output))
  #
  #at_result = new_pcj_result(at_val, cond, out)
  #return(at_result)
  return(res)
}


sort_prior_truncation = function(x) {
  stopifnot(exprs = {
    vek::is_num_vec(x) || is_list_of_num_1(x)
    #is_sorted(x)
    # TODO
  })

  if (length(x) < 2L)
    return(x)
  else if (is.null(names(x))) # TODO deal with names if x is list
    return(x)

  stopifnot(exprs = {
    sum(names(x) == "truncation_lower_open", na.rm = TRUE) <= 1L
    sum(names(x) == "truncation_upper_open", na.rm = TRUE) <= 1L
    sum(names(x) == "truncation_lower_closed", na.rm = TRUE) <= 1L
    sum(names(x) == "truncation_upper_closed", na.rm = TRUE) <= 1L
  })

  if ("truncation_lower_open" %in% names(x) &&
      "truncation_lower_closed" %in% names(x))
  {
    open_i =   which(names(x) == "truncation_lower_open")
    closed_i = which(names(x) == "truncation_lower_closed")
    stopifnot(exprs = {
      vek::is_int_vec_x1(open_i)
      vek::is_int_vec_x1(closed_i)
      open_i != closed_i
      abs(open_i - closed_i) == 1L # Check the elements are next to each other
      # TODO drop the requirement one line above, and also bring them together
      # if they're not next to each other.
    })

    if (open_i > closed_i) {
      # Switch places.
      open_val = x[open_i]
      closed_val = x[closed_i]
      names_ = names(x)
      names_[open_i] = names(x)[closed_i]
      names_[closed_i] = names(x)[open_i]
      x[open_i] = closed_val
      x[closed_i] = open_val
      names(x) = names_
    }
  }

  if ("truncation_upper_open" %in% names(x) &&
      "truncation_upper_closed" %in% names(x))
  {
    open_i =   which(names(x) == "truncation_upper_open")
    closed_i = which(names(x) == "truncation_upper_closed")
    stopifnot(exprs = {
      vek::is_int_vec_x1(open_i)
      vek::is_int_vec_x1(closed_i)
      open_i != closed_i
      abs(open_i - closed_i) == 1L # Check the elements are next to each other
      # TODO drop the requirement one line above, and also bring them together
      # if they're not next to each other.
    })

    if (open_i < closed_i) {
      # Switch places.
      open_val = x[open_i]
      closed_val = x[closed_i]
      names_ = names(x)
      names_[open_i] = names(x)[closed_i]
      names_[closed_i] = names(x)[open_i]
      x[open_i] = closed_val
      x[closed_i] = open_val
      names(x) = names_
    }
  }

  return(x)
}


get_at_info = function(at) {
  stopifnot(exprs = {
    vek::is_num_vec(strip_attributes_if_valuetype(at)) ||
      vek::is_chr_vec(strip_attributes_if_valuetype(at))
  })

  by = NULL
  n = NULL
  sample_size = NULL

  at_attrs = names(attributes(at))
  if ("by" %in% at_attrs && "n" %in% at_attrs)
    stop('"at" may only carry one of "by" or "n" as attributes')

  if ("by" %in% at_attrs)
    stop('"at" currently does not support use of attribute "by"')

  if ("by" %in% at_attrs) {
    by = attr(at, "by", TRUE)
    attr(at, "by") = NULL
    stopifnot(exprs = {
      vek::is_num_vec_xyz1(by)
      by > 0
    })
  }

  if ("n" %in% at_attrs) {
    n = attr(at, "n", TRUE)
    attr(at, "n") = NULL
    stopifnot(exprs = {
      vek::is_int_vec_x1(n)
      n > 1L
    })
  }

  if ("sample_size" %in% at_attrs) {
    sample_size = attr(at, "sample_size", TRUE)
    attr(at, "sample_size") = NULL
    stopifnot(exprs = {
      vek::is_int_vec_x1(sample_size)
      sample_size > 1L
    })
  }

  if (vek::is_num_vec_xyz1(from) && vek::is_num_vec_xyz1(to)) {
    stopifnot(from <= to)
  }

  return(list(
    at = at,
    from = from,
    to = to,
    by = by,
    n = n,
    sample_size = sample_size
  ))
}



#slice2_xy = function(x, y, x_a, x_b) {
#  stopifnot(exprs = {
#    vek::is_num_vec(x)
#    vek::is_num_vec(y)
#    !is.unsorted(x, TRUE, TRUE)
#    length(x) == length(y)
#    vek::is_num_vec_xyz1(x_a) || is.null(x_a)
#    vek::is_num_vec_xyz1(x_b) || is.null(x_b)
#  })
#
#  xy = list(x = x, y = y)
#  if (!is.null(x_a))
#    xy = slice_xy(xy$x, xy$y, x_a, FALSE)
#  if (!is.null(x_b))
#    xy = slice_xy(xy$x, xy$y, x_b, TRUE)
#
#  return(xy)
#}


## TODO improve logic
#auto_xlim_pcj_jags_distribution = function(dist_obj) {
#  stopifnot(is.pcj_jags_distribution(dist_obj)) # TODO is valid check
#
#  trunc_a = NULL
#  trunc_b = NULL
#  x = numeric(0L)
#  x_range = c(NA_real_, NA_real_)
#  if (!is.null(dist_obj$truncation)) {
#    trunc_a = dist_obj$truncation[[1L]]
#    trunc_b = dist_obj$truncation[[2L]]
#  } else {
#    #set.seed(1L)
#    x = pcj_jags_distribution_rng(dist_obj, 1000L)
#    x_range = range(x, na.rm = TRUE)
#  }
#
#  xlim = c(NA_real_, NA_real_)
#  xlim[1L] = if (is.null(trunc_a)) x_range[1L] else trunc_a
#  xlim[2L] = if (is.null(trunc_b)) x_range[2L] else trunc_b
#  return(xlim)
#}


#splice_truncation_xy = function(x, y, a, b, ya, yb) {
#  if (!is.null(a)) {
#    x_gt_a = x > a
#    x = c(x[1L], a, a, x[x_gt_a])
#    y = c(0L, 0L, ya, y[x_gt_a])
#  }
#  if (!is.null(b)) {
#    x_lt_b = x < b
#    x = c(x[x_lt_b], b, b, x[length(x)])
#    y = c(y[x_lt_b], yb, 0L, 0L)
#  }
#
#  return(list(x = x, y = y))
#}


#fix_vec = function(name, dots, order, y) {
#  stopifnot(exprs = {
#    vek::is_chr_vec_xb(name)
#    is_all_unique(name)
#    is_uniquely_named_list(dots)
#  })
#
#  present_names = name[name %in% names(dots)]
#  if (length(present_names) == 0L)
#    return(dots)
#
#  for (x in present_names) {
#    dots[[x]] = expand_and_reorder_vec_if_needed(dots[[x]], order, y)
#  }
#
#  return(dots)
#}


#expand_and_reorder_vec_if_needed = function(x, order, y) {
#  stopifnot(exprs = {
#    # TODO check x is vec
#    # TODO check y is vec
#    vek::is_int_vec_x(order)
#    length(order) == length(y)
#    length(x) <= length(y)
#  })
#
#  if (length(x) > 1L) {
#    adj_x = x
#    x_class = class(adj_x)
#    adj_x = unclass(adj_x)
#    if (length(adj_x) < length(y))
#      adj_x = rep_len(adj_x, length(y))
#
#    adj_x = adj_x[order]
#    class(adj_x) = x_class
#    return(adj_x)
#  } else {
#    return(x)
#  }
#}


# A function to facilitate piping.
#pcj_plot_object_plot_dist = function(func_name, object, ...) {
#  stopifnot(exprs = {
#    is.pcj_plot_object(object) || is.pcj_plot_object_list(object)
#    vek::is_chr_vec_xb1(func_name)
#    func_name %in% c("plot_prior_density", "plot_prior_predictive_density", "plot_posterior_density")
#  })
#
#  func = switch(
#    func_name,
#    "plot_prior_density" = plot_prior_density,
#    "plot_prior_predictive_density" = plot_prior_predictive_density,
#    "plot_posterior_density" = plot_posterior_density,
#    stop()
#  )
#
#  if (is.pcj_plot_object(object))
#    obj = object
#  else if (is.pcj_plot_object_list(object))
#    obj = object[[length(object)]]
#  else
#    stop()
#
#  args = c(list(get_result(obj)$object), list(...))
#
#  if ((!"add" %in% names(args)))
#    args$add = TRUE
#
#  if (!("x" %in% names(args)))
#    args$x = get_result(obj)$data$x_
#
#  b = do.call(func, args)
#
#  return(c(object, b))
#}


##' @export
#points.pcj_process_capability_model1 = function(
    #    object,
#    ...,
#    x,
#    distribution,
#    at,
#    offset
#  )
#{
#  stopifnot(exprs = {
#    is.pcj_process_capability_model1(object)
#    vek::is_chr_vec_xb1(x)
#    vek::is_chr_vec_xb1(distribution)
#    distribution %in% c("prior", "prior_predictive", "posterior")
#  })
#
#  func = switch(
#    distribution,
#    "prior" = plot_prior_density,
#    "prior_predictive" = plot_prior_predictive_density,
#    "posterior" = plot_posterior_density,
#    stop()
#  )
#
#  return(func(object, "points", ..., x = x))
#}


##' @export
#lines.pcj_process_capability_model1 = function(
    #    object,
#    ...,
#    x,
#    distribution,
#    at,
#    offset
#  )
#{
#  stopifnot(exprs = {
#    is.pcj_process_capability_model1(object)
#    vek::is_chr_vec_xb1(x)
#    vek::is_chr_vec_xb1(distribution)
#    distribution %in% c("prior", "prior_predictive", "posterior")
#  })
#
#  func = switch(
#    distribution,
#    "prior" = plot_prior_density,
#    "prior_predictive" = plot_prior_predictive_density,
#    "posterior" = plot_posterior_density,
#    stop()
#  )
#
#  return(func(object, "lines", ..., x = x))
#}
