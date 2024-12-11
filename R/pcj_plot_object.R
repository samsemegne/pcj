

is.pcj_plot_object = function(x) return(is_of_mono_class(x, "pcj_plot_object"))


new_pcj_plot_object = function(
    func,
    args,
    meta,
    condition
  )
{
  stopifnot(exprs = {
    vek::is_chr_vec_x1(func)
    is_uniquely_named_list(args)
    is_uniquely_named_list(meta)
    is_list(condition)
    all(sapply_(condition, is_cond), na.rm = FALSE)
  })

  return(structure(
    list(
      condition = condition,
      result = list(func = func, args = args, meta = meta)
    ),
    class = "pcj_plot_object"
  ))
}


#' @export
get_error.pcj_plot_object = get_error_
#' @export
get_warning.pcj_plot_object = get_warning_
#' @export
get_message.pcj_plot_object = get_message_
#' @export
get_condition.pcj_plot_object = get_condition_
#' @export
get_result.pcj_plot_object = get_result_


#' @export
print.pcj_plot_object = function(object) {
  plot(object)
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
    get_result(object)$func,
    "plot.default" = graphics::plot.default,
    "plot.xy" = graphics::plot.xy,
    "lines.default" = graphics::lines.default,
    "points.default" = graphics::points.default,
    "polygon" = graphics::polygon,
    "arrows" = graphics::arrows,
    "axis" = graphics::axis,
    stop()
  )

  do.call(func, get_result(object)$args)

  return(invisible(object))
}


#' @export
c.pcj_plot_object = function(object, ...) {
  stopifnot(exprs = {
    is.pcj_plot_object(object)
    ...length() > 0L
    is.null(...names())
  })

  if (...length() == 1L) {
    stopifnot(exprs = {
      is.pcj_plot_object(...elt(1L)) || is.pcj_plot_object_list(...elt(1L))
    })

    o = ...elt(1L)

    if (is.pcj_plot_object(o)) {
      return(structure(
        list(object, o),
        class = "pcj_plot_object_list"
      ))
    } else if (is.pcj_plot_object_list(o)) {
      o_ = c(list("temp"), unclass(o))
      o_[[1L]] = object
      class(o_) = "pcj_plot_object_list"
      return(o_)
    } else {
      stop()
    }
  } else {
    o = object
    for (i in 1:(...length()))
      o = c(o, ...elt(i))

    return(o)
  }
}


#preprocess_pcj_plot_object = function(object) {
#  stopifnot(is.pcj_plot_object(object))
#
#  args = get_result(object)$args
#
#  if ("offset" %in% names(get_result(object)$meta)) {
#    offset = get_result(object)$meta$offset
#
#    if (get_result(object)$func == "arrows") {
#      if (vek::is_num_vec(args$x0))
#        args$x0 = args$x0 + offset[1L]
#
#      if (vek::is_num_vec(args$x1))
#        args$x1 = args$x1 + offset[1L]
#
#      if (vek::is_num_vec(args$y0))
#        args$y0 = args$y0 + offset[2L]
#
#      if (vek::is_num_vec(args$y1))
#        args$y1 = args$y1 + offset[2L]
#
#    } else {
#      if (vek::is_num_vec(args$x))
#        args$x = args$x + offset[1L]
#
#      if (vek::is_num_vec(args$y))
#        args$y = args$y + offset[2L]
#    }
#
#    # TODO add else. and axis offset?
#  }
#
#  object$result$args = args
#  return(object)
#}


# Functions to facilitate piping.
##' @export
#plot_prior_density.pcj_plot_object = function(object, ...) {
#  return(pcj_plot_object_plot_dist("plot_prior_density", object, ...))
#}


##' @export
#plot_prior_predictive_density.pcj_plot_object = function(object, ...) {
#  return(pcj_plot_object_plot_dist("plot_prior_predictive_density", object, ...))
#}


##' @export
#plot_posterior_density.pcj_plot_object = function(object, ...) {
#  return(pcj_plot_object_plot_dist("plot_posterior_density", object, ...))
#}
