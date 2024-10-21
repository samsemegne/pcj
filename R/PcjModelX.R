

#' @export
PcjModelX = R6::R6Class(
  "PcjModelX",
  private = list(
    content_ = NULL
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

    condition = function(value) {
      if (missing(value))
        return(get_condition(self$content))
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

    message = function(value) {
      if (missing(value))
        return(get_message(self$content))
      else
        stop(runtimeError("Runtime error"))
    },

    output = function(value) {
      if (missing(value))
        return(get_output(self$content))
      else
        stop(runtimeError("Runtime error"))
    }
  ),

  public = list(
    update = function(
        data,
        pci_params,
        prior_mu,
        prior_sigma,
        prior_predictive_params,
        sampler_params,
        stat,
        evaluate = TRUE
      )
    {
      if (is.null(private$content_)) {
        if (any(missing(data), missing(pci_params), missing(prior_mu),
                missing(prior_sigma), missing(prior_predictive_params),
                missing(sampler_params), missing(evaluate), na.rm = FALSE))
        {
          stop('All required parameters must be specified')
        }

        if (missing(stat))
          stat = default_stats

        obj = new_pcj_modelx(
          data,
          pci_params,
          prior_mu,
          prior_sigma,
          prior_predictive_params,
          sampler_params,
          stat,
          evaluate
        )
      } else {
        args = list(object = self$content)

        if (!missing(data))
          args$data = data
        if (!missing(prior_mu))
          args$prior_mu = prior_mu
        if (!missing(prior_sigma))
          args$prior_sigma = prior_sigma
        if (!missing(prior_predictive_params))
          args$prior_predictive_params = prior_predictive_params
        if (!missing(sampler_params))
          args$sampler_params = sampler_params
        if (!missing(stat))
          args$stat = stat
        if (!missing(evaluate))
          args$evaluate = evaluate

        obj = do.call(update.pcj_modelx, args)
      }

      private$content_ = obj
      return(invisible(self))
    },

    summary = function() {
      return(summary.pcj_modelx(self$content))
    },

    variable.names = function(distribution) {
      return(variable.names.pcj_modelx(self$content, distribution))
    },

    plot_prior = function(x, ...) {
      return(plot_prior.pcj_modelx(self$content, ..., x = x))
    },

    plot_prior_predictive = function(x, ...) {
      return(plot_prior_predictive.pcj_modelx(self$content, ..., x = x))
    },

    plot_posterior = function(x, ...) {
      return(plot_posterior.pcj_modelx(self$content, ..., x = x))
    },

    probability = function(x, distribution, value) {
      return(probability.pcj_modelx(self$content, x, distribution, value))
    },

    quantile = function(x, distribution, value) {
      return(quantile.pcj_modelx(self$content, x, distribution, value))
    },

    new_pci_params1 = function(
        capability_indices,
        target,
        lsl,
        usl,
        dl
      )
    {
      return(new_pci_params1(capability_indices, target, lsl, usl, dl))
    },

    new_prior_predictive_params = function(
        sample_size,
        seed,
        rng_kind = "Wichmann-Hill",
        rng_version = "4.4.0"
      )
    {
      return(new_prior_predictive_params(
        sample_size, seed, rng_kind, rng_version
      ))
    },

    new_rjags_params = function(
        burnin,
        sample_size,
        thin,
        n_chains,
        seed,
        rng_kind,
        initial_values = list()
      )
    {
      return(new_rjags_params(
        burnin,
        sample_size,
        thin,
        n_chains,
        seed,
        rng_kind,
        initial_values
      ))
    }
  )
)


#' @export
get_condition.PcjModelX = function(object) return(object$condition)
#' @export
get_error.PcjModelX = function(object) return(object$error)
#' @export
get_warning.PcjModelX = function(object) return(object$warning)
#' @export
get_message.PcjModelX = function(object) return(object$message)
#' @export
get_output.PcjModelX = function(object) return(object$output)
#' @export
get_result.PcjModelX = function(object) return(get_result(object$content))


#' @export
update.PcjModelX = function(object, ...) {
  return(object$update(...))
}


#' @export
summary.PcjModelX = function(object) {
  return(object$summary())
}


#' @export
variable.names.PcjModelX = function(object, distribution) {
  return(object$variable.names(distribution))
}


#' @export
probability.PcjModelX = function(object, x, distribution, value) {
  return(object$probability(x, distribution, value))
}


#' @export
quantile.PcjModelX = function(object, x, distribution, value) {
  return(object$quantile(x, distribution, value))
}


#' @export
get_data.PcjModelX = function(object) {
  return(get_data(object$content))
}


#' @export
variable.names.PcjModelX = function(object, distribution) {
  return(variable.names(object$content, distribution))
}

