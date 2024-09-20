
#' @importFrom ggplot2 ggplot_add
#'
# col = colour / fill
# lty = linetype 0:6
# lwd = linewidth
# lend = lineend
# ... = linejoin (see ljoin, lmitre)
# pty = shape

# geom_line:

# polygon
# border: colour, linetype, linewidth
# inside: fill

# geom_point:
# shape, size (of filled part), stroke (size of outline part) fill
# 0:25
# 21:24 both stroke, colour, and fill

pcj_plot_object_to_ggplot2 = function(object) {
  stopifnot(is.pcj_plot_object(object))

  func = switch(
    object$func,
    "plot.default" = plot_default_to_ggplot2,
    "lines.default" = lines_default_to_ggplot2,
    "points.default" = points_default_to_ggplot2,
    "polygon" = polygon_to_ggplot2,
    "arrows" = arrows_to_ggplot2,
    "axis" = axis_to_ggplot2,
    stop()
  )

  return(func(object))
}


gg_add = function(parent, element) {
  if (is.list(element)) {
    for (el in element)
      parent = parent + el
    return(parent)
  } else {
    return(parent + element)
  }
}


gg_build = function(object) {
  stopifnot(exprs = {
    is.pcj_plot_object_list(object) || is.pcj_plot_object(object)
  })

  if (is.pcj_plot_object(object)) {
    obj = preprocess_pcj_plot_object(object)
    return(pcj_plot_object_to_ggplot2(obj))
  }
  else if (is.pcj_plot_object_list(object)) {
    ggobj = NULL
    for (obj in object) {
      obj = preprocess_pcj_plot_object(obj)
      ggobj_ = pcj_plot_object_to_ggplot2(obj)
      if (is.null(ggobj))
        ggobj = ggobj_
      else
        ggobj = gg_add(ggobj, ggobj_)
    }

    return(ggobj)
  } else {
    stop()
  }
}


# TODO check if plot.default
#' @export
ggplot_add.pcj_plot_object_list = function(object, plot, object_name) {
  stopifnot(exprs = {
    is.pcj_plot_object_list(object)
    inherits(plot, "ggplot", which = FALSE)
  })

  ggobj = gg_build(object)
  return(plot + ggobj)
}


# TODO check if plot.default
#' @export
ggplot_add.pcj_plot_object = function(object, plot, object_name) {
  stopifnot(exprs = {
    is.pcj_plot_object(object)
    inherits(plot, "ggplot", which = FALSE)
  })

  ggobj = gg_build(object)
  return(plot + ggobj)
}


get_aes_name_map = function() {
  return(c(
    col = "colour",
    lty = "linetype",
    lwd = "linewidth",
    lend = "lineend",
    pty = "shape"
  ))
}


get_aes_map = function() {
  return(list(
    colour = identity,
    linetype = identity,
    linewidth = identity,
    lineend = identity,
    pty = identity
  ))
}


plot_default_to_ggplot2 = function(object) {
  stopifnot(is.pcj_plot_object(object))

  args = object$args
  keys = names(object$args)
  obj = ggplot2::ggplot()

  coord_args = list()
  if ("xlim" %in% keys)
    coord_args = c(coord_args, list(xlim = args$xlim))
  if ("ylim" %in% keys)
    coord_args = c(coord_args, list(ylim = args$ylim))

  coord_obj = do.call(ggplot2::coord_cartesian, coord_args)
  obj = obj + coord_obj #ggplot2::ggplot_add(obj, coord_obj, "")

  ann = TRUE
  if ("ann" %in% keys) {
    ann = args$ann
    stopifnot(vek::is_lgl_vec_x1(ann))
  }

  if (ann) {
    if ("xlab" %in% keys)
      obj = obj + do.call(ggplot2::xlab, list(args$xlab))
    if ("ylab" %in% keys)
      obj = obj + do.call(ggplot2::ylab, list(args$ylab))

    labs_args = list()
    if ("main" %in% keys)
      labs_args = c(labs_args, list(title = args$main))
    if ("sub" %in% keys)
      labs_args = c(labs_args, list(subtitle = args$sub))

    labs_obj = do.call(ggplot2::labs, labs_args)
    obj = obj + labs_obj #ggplot2::ggplot_add(obj, labs_obj, "")
  }

  axes = TRUE
  if ("axes" %in% keys) {
    axes = args$axes
    stopifnot(vek::is_lgl_vec_x1(axes))
  }

  if (!axes) {
    obj = obj + ggplot2::theme(
      axis.text.x = ggplot2::element_blank(),
      axis.text.y = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank()
    )
  }

  # TODO...
  return(obj)
}


# function (x0, y0, x1 = x0, y1 = y0, length = 0.25, angle = 30,
# code = 2, col = par("fg"), lty = par("lty"), lwd = par("lwd"),
# ...)

#geom_segment(
#  x = 1, y = 1,
#  xend = 4, yend = 7,
#  lineend = "round", # See available arrow types in example above
#  linejoin = "round",
#  size = 2,
#  arrow = arrow(length = unit(0.3, "inches")),
#  colour = "#EC7014" # Also accepts "red", "blue' etc
#)

#t |> ggplot(aes(x=x,y=y)) + geom_point() +
#  annotate("segment", x=1, y=2, xend=2, yend=2,
#           arrow = arrow())

arrows_to_ggplot2 = function(object) {
  stopifnot(is.pcj_plot_object(object))

  # Note. If no 'data' is provided, the arrow won't display.
  args = list(data = new_df(foo = NA))

  dots = list_get(object$args, c("x0", "x1", "y0", "y1"))
  if (length(dots) > 0L) {
    arrows_map = c(x0 = "x", x1 = "xend", y0 = "y", y1 = "yend")
    names(dots) = arrows_map[names(dots)]
    args = c(args, dots)
  } else {
    stop()
  }

  dots = list_get(object$args, c("col", "lty", "lwd", "lend"))
  if (length(dots) > 0L) {
    arrows_map = c(
      pch = "shape",
      col = "colour",
      lwd = "linewidth", # TODO size?
      lty = "linetype",
      lend = "lineend"
    )

    names(dots) = arrows_map[names(dots)]
    args = c(args, dots)
  }

  dots = list_get(object$args, c("angle", "length"))
  if (length(dots) > 0L) {
    a_map = c(angle = "angle", length = "length") # TODO add 'ends', 'type'
    names(dots) = a_map[names(dots)]

    a_obj = do.call(ggplot2::arrow, dots)
    args = c(args, list(arrow = a_obj))
  } else {
    args = c(args, list(arrow = ggplot2::arrow()))
  }

  obj = do.call(ggplot2::geom_segment, args)
  # Add a transparent point to assure the axes
  obj2 = ggplot2::geom_point(
      mapping = ggplot2::aes(x = 0L, y = 0L), colour = "transparent")

  return(list(obj, obj2))
}


points_default_to_ggplot2 = function(object) {
  stopifnot(is.pcj_plot_object(object))

  dots = list_get(object$args, c("pch", "col", "bg", "lty", "lwd", "lend"))
  points_map = c(
    pch = "shape",
    col = "colour",
    bg = "fill",
    cex = "",
    lwd = "linewidth",
    lty = "linetype",
    lend = "lineend"
  )

  names(dots) = points_map[names(dots)]
  args = list(
    mapping = ggplot2::aes(x = object$args$x, y = object$args$y)
  ) |>
    c(dots)

  obj = do.call(ggplot2::geom_point, args)
  return(obj)
}


lines_default_to_ggplot2 = function(object) {
  stopifnot(is.pcj_plot_object(object))

  dots = list_get(object$args, c("col", "lty", "lwd", "lend"))
  names(dots) = get_aes_name_map()[names(dots)]
  args = list(
    mapping = ggplot2::aes(x = object$args$x, y = object$args$y)
  ) |>
    c(dots)

  obj = do.call(ggplot2::geom_line, args)
  return(obj)
}


axis_to_ggplot2 = function(object) {
  stopifnot(is.pcj_plot_object(object))


  side = object$args$side
  stopifnot(exprs = {
    vek::is_num_vec_xyz1(side)
    side %% 2L == 0L
  })

  if (side == 1L)
    side_lab = "x"
  else if (side == 2L)
    side_lab = "y"
  else
    stop()

  args = list()
  dots = list_get(object$args, c("col", "lty", "lwd", "lend"))
  if (length(dots) > 0L) {
    axis_line_map = c(
      col = "colour",
      lty = "linetype",
      lwd = "linewidth",
      lend = "lineend"
    )

    names(dots) = axis_line_map[names(dots)]
    line_obj = do.call(ggplot2::element_line, dots)
    line_args = list(axis.line = line_obj)
    names(line_args) = sprintf("axis.line.%s", side_lab)
    args = c(args, line_args)
  }

  dots = list_get(object$args, c("col.ticks", "lty", "lwd.ticks", "lend"))
  if (length(dots) > 0L) {
    axis_line_map = c(
      col.ticks = "colour",
      lty = "linetype",
      lwd.ticks = "linewidth",
      lend = "lineend"
    )

    names(dots) = axis_line_map[names(dots)]
    line_obj = do.call(ggplot2::element_line, dots)
    ticks_args = list(axis.ticks.x = line_obj)
    names(ticks_args) = sprintf("axis.ticks.%s", side_lab)
    args = c(args, ticks_args)
  }

  theme_obj = NULL
  if (length(args) > 0L)
    theme_obj = do.call(ggplot2::theme, args)

  axis_obj = NULL
  dots = list_get(object$args, c("at", "labels")) #"line" "tick"
  if (length(dots) > 0L) {
    axis_map = c(at = "breaks", labels = "labels")
    names(dots) = axis_map[names(dots)]
    func = switch(
      side_lab,
      "x" = ggplot2::scale_x_continuous,
      "y" = ggplot2::scale_y_continuous,
      stop()
    )

    axis_obj = do.call(func, dots)
  }

  obj = list()
  if (!is.null(theme_obj))
    obj = c(obj, list(theme_obj))
  if (!is.null(axis_obj))
    obj = c(obj, list(axis_obj))

  return(obj)
}


polygon_to_ggplot2 = function(object) {
  stopifnot(is.pcj_plot_object(object))

  polygon_map = c(
    col = "fill",
    border = "colour",
    lty = "linetype",
    lwd = "linewidth",
    lend = "lineend"
  )

  dots = list_get(object$args, c("col", "lty", "lwd", "border"))
  names(dots) = polygon_map[names(dots)]
  args = list(
    mapping = ggplot2::aes(x = object$args$x, y = object$args$y)
  ) |>
    c(dots)

  obj = do.call(ggplot2::geom_polygon, args)
  return(obj)
}


#data = NULL,
#stat = "identity",
#position = "identity",
#na.rm = FALSE,
#orientation = NA,
#show.legend = NA,
#inherit.aes = TRUE,

list_get = function(x, entries) {
  return(x[names(x) %in% entries])
}

