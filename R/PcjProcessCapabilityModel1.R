

#' @export
PcjProcessCapabilityModel1 = R6::R6Class(
  "PcjProcessCapabilityModel1",
  private = list(
    content_ = NULL,
    prior_ = NULL,
    prior_predictive_ = NULL,
    posterior_ = NULL,
    setup_obj_ = function() {
      private$prior_ = create_bayes_distribution(
        "prior", self$variable.names("prior"), self)

      private$prior_predictive_ = create_bayes_distribution(
        "prior_predictive", self$variable.names("prior_predictive"), self)

      private$posterior_ = create_bayes_distribution(
        "posterior", self$variable.names("posterior"), self)

      return(invisible(self))
    }
  ),

  active = list(
    content = function(value) {
      if (missing(value))
        if (is.null(private$content_))
          stop(runtimeError("Runtime error"))
        else
          return(private$content_)
      else
        stop(runtimeError("Runtime error"))
    },

    error = function(value) {
      if (missing(value))
        return(get_error(self$content))
      else
        stop(runtimeError("Runtime error"))
    },

    warning = function(value) {
      if (missing(value))
        return(get_warning(self$content))
      else
        stop(runtimeError("Runtime error"))
    },

    prior = function(value) {
      if (missing(value))
        return(private$prior_)
      else
        stop(runtimeError("Runtime error"))
    },

    prior_predictive = function(value) {
      if (missing(value))
        return(private$prior_predictive_)
      else
        stop(runtimeError("Runtime error"))
    },

    posterior = function(value) {
      if (missing(value))
        return(private$posterior_)
      else
        stop(runtimeError("Runtime error"))
    }
  ),

  public = list(
    update = function(
        data,
        pci_parameters,
        prior_mu,
        prior_sigma,
        prior_predictive_parameters,
        sampler_parameters,
        stat,
        evaluate = FALSE
      )
    {
      if (is.null(private$content_)) {
        if (any(missing(data), missing(pci_parameters), missing(prior_mu),
                missing(prior_sigma), missing(prior_predictive_parameters),
                missing(sampler_parameters), na.rm = FALSE))
        {
          stop('All required parameters must be specified')
        }

        if (missing(stat))
          stat = default_stats

        obj = new_pcj_process_capability_model1(
          data,
          pci_parameters,
          prior_mu,
          prior_sigma,
          prior_predictive_parameters,
          sampler_parameters,
          stat,
          evaluate
        )
      } else {
        args = list(object = self$content)

        if (!missing(data))
          args$data = data
        if (!missing(pci_parameters))
          args$pci_parameters = pci_parameters
        if (!missing(prior_mu))
          args$prior_mu = prior_mu
        if (!missing(prior_sigma))
          args$prior_sigma = prior_sigma
        if (!missing(prior_predictive_parameters))
          args$prior_predictive_parameters = prior_predictive_parameters
        if (!missing(sampler_parameters))
          args$sampler_parameters = sampler_parameters
        if (!missing(stat))
          args$stat = stat
        if (!missing(evaluate)) # TODO default value behavior?
          args$evaluate = evaluate

        obj = do.call(update.pcj_process_capability_model1, args)
      }

      private$content_ = obj
      private$setup_obj_()

      return(invisible(self))
    },

    run = function() {
      self$update(evaluate = TRUE)
      return(invisible(self))
    },

    summary = function(
      statistics = c("mean", "median", "sd", "q.025", "q.25", "q.5", "q.75",
                                      "q.975")
    ) {
      return(summary.pcj_process_capability_model1(self$content, statistics))
    },

    variable.names = function(distribution) {
      return(stats::variable.names(self$content, distribution))
    },

    plot_prior_density = function(what, ...) {
      return(plot_prior_density(self$content, ..., what = what))
    },

    plot_prior_predictive_density = function(what, ...) {
      return(plot_prior_predictive_density(self$content, ..., what = what))
    },

    plot_posterior_density = function(what, ...) {
      return(plot_posterior_density(self$content, ..., what = what))
    },

    probability = function(distribution, what, value, ...) {
      return(probability(self$content, distribution, what, value, ...))
    },

    quantile = function(distribution, what, value, ...) {
      return(stats::quantile(self$content, distribution, what, value, ...))
    },

    new_pci_parameters1 = function(
        capability_indices,
        target,
        lsl,
        usl,
        dl
      )
    {
      return(new_pci_parameters1(capability_indices, target, lsl, usl, dl))
    },

    new_prior_predictive_parameters = function(
        sample_size,
        seed,
        rng_kind = "Wichmann-Hill",
        rng_version = "4.4.0"
      )
    {
      return(new_prior_predictive_parameters(
        sample_size, seed, rng_kind, rng_version
      ))
    },

    new_rjags_parameters = function(
        burnin,
        sample_size,
        thin,
        n_chains,
        seed,
        rng_kind,
        initial_value = list()
      )
    {
      return(new_rjags_parameters(
        burnin,
        sample_size,
        thin,
        n_chains,
        seed,
        rng_kind,
        initial_value
      ))
    }
  )
)


#' @export
get_condition.PcjProcessCapabilityModel1 = function(object) {
  return(get_condition(object$content))
}


#' @export
get_error.PcjProcessCapabilityModel1 = function(object) return(object$error)
#' @export
get_warning.PcjProcessCapabilityModel1 = function(object) return(object$warning)
#' @export
get_message.PcjProcessCapabilityModel1 = function(object) {
  return(get_message(object$content))
}


#' @export
get_output.PcjProcessCapabilityModel1 = function(object) {
  return(get_output(object$content))
}


#' @export
get_result.PcjProcessCapabilityModel1 = function(object) {
  return(get_result(object$content))
}


#' @export
update.PcjProcessCapabilityModel1 = function(object, ...) {
  return(object$update(...))
}


# TODO add ... params
#' @export
summary.PcjProcessCapabilityModel1 = function(object) {
  return(object$summary())
}


#' @export
variable.names.PcjProcessCapabilityModel1 = function(object, distribution) {
  return(stats::variable.names(object$content, distribution))
}


#' @export
probability.PcjProcessCapabilityModel1 = function(
    object, distribution, what, value, ...)
{
  return(probability(object$content, distribution, what, value, ...))
}


#' @export
quantile.PcjProcessCapabilityModel1 = function(
    object, distribution, what, value, ...)
{
  return(stats::quantile(object$content, distribution, what, value, ...))
}


#' @export
mean.PcjProcessCapabilityModel1 = function(object, distribution, what, ...) {
  return(mean(object$content, distribution, what, ...))
}


#' @export
median.PcjProcessCapabilityModel1 = function(object, distribution, what, ...) {
  return(stats::median(object$content, distribution, what, ...))
}


#' @export
get_data.PcjProcessCapabilityModel1 = function(object) {
  return(get_data(object$content))
}


# TODO check params
#' @export
plot_prior_density.PcjProcessCapabilityModel1 = function(
    object, ..., what)
{
  return(object$plot_prior_density(what, ...))
}


#' @export
plot_prior_predictive_density.PcjProcessCapabilityModel1 = function(
    object,
    ...,
    what
  )
{
  return(object$plot_prior_predictive_density(what, ...))
}


#' @export
plot_posterior_density.PcjProcessCapabilityModel1 = function(
    object,
    ...,
    what
  )
{
  return(object$plot_posterior_density(what, ...))
}

