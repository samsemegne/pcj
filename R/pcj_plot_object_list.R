

is.pcj_plot_object_list = function(x) {
  return(is_of_mono_class(x, "pcj_plot_object_list"))
}


#' @export
print.pcj_plot_object_list = function(object) {
  plot(object)
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
c.pcj_plot_object_list = function(object, ...) {
  stopifnot(exprs = {
    is.pcj_plot_object_list(object)
    ...length() > 0L
    is.null(...names())
  })

  if (...length() == 1L) {
    stopifnot(exprs = {
      is.pcj_plot_object(...elt(1L)) || is.pcj_plot_object_list(...elt(1L))
    })

    o = ...elt(1L)

    if (is.pcj_plot_object(o)) {
      object[[length(object) + 1L]] = o
      return(object)
    } else if (is.pcj_plot_object_list(o)) {
      o_ = c(unclass(object), unclass(o))
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


# Functions to facilitate piping.
##' @export
#plot_prior_density.pcj_plot_object_list = function(object, ...) {
#  return(pcj_plot_object_plot_dist("plot_prior_density", object, ...))
#}


##' @export
#plot_prior_predictive_density.pcj_plot_object_list = function(object, ...) {
#  return(pcj_plot_object_plot_dist("plot_prior_predictive_density", object, ...))
#}


##' @export
#plot_posterior_density.pcj_plot_object_list = function(object, ...) {
#  return(pcj_plot_object_plot_dist("plot_posterior_density", object, ...))
#}
