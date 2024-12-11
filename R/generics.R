

#' @export
get_data = function(object, ...) UseMethod("get_data")


#' @export
get_sample = function(object, ...) UseMethod("get_sample")


#' @export
get_prior = function(object, ...) UseMethod("get_prior")


#' @export
get_error = function(object, ...) UseMethod("get_error")


#' @export
get_warning = function(object, ...) UseMethod("get_warning")


#' @export
get_message = function(object, ...) UseMethod("get_message")


#' @export
get_condition = function(object, ...) UseMethod("get_condition")


#' @export
get_output = function(object, ...) UseMethod("get_output")


#' @export
get_result = function(object, ...) UseMethod("get_result")


#' @export
probability = function(object, ...) UseMethod("probability")


#' @export
plot_prior_density = function(object, ...) UseMethod("plot_prior_density")


#' @export
plot_prior_predictive_density = function(object, ...) {
  UseMethod("plot_prior_predictive_density")
}


#' @export
plot_posterior_density = function(object, ...) {
  UseMethod("plot_posterior_density")
}


#' @export
plot_posterior_predictive_density = function(object, ...) {
  UseMethod("plot_posterior_predictive_density")
}


#' @export
plot_prior_mass = function(object, ...) UseMethod("plot_prior_mass")


#' @export
plot_prior_predictive_mass = function(object, ...) {
  UseMethod("plot_prior_predictive_mass")
}


#' @export
plot_posterior_mass = function(object, ...) {
  UseMethod("plot_posterior_mass")
}


#' @export
plot_posterior_predictive_mass = function(object, ...) {
  UseMethod("plot_posterior_predictive_mass")
}


#' @export
has_probability_density = function(object, ...) {
  UseMethod("has_probability_density")
}


#' @export
has_probability_mass = function(object, ...) UseMethod("has_probability_mass")

