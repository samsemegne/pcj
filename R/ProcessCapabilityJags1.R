

R6::R6Class(
  "ProcessCapabilityJags1",
  private = list(
    content_ = NULL
  ),
  active = list(
    content = function(value) {
      if (missing(value))
        if (is.null(private$content_))
          stop()
        else
          return(private$content_)
      else
        stop()
    },
    error = function(value) {

    },
    warning = function(value) {},
    condition = function(value) {}
  ),
  public = list(
    estimate = function(data, pci_params, prior_mu, prior_sigma,
                        prior_predictive_params, sampler_params,
                        sequential_params)
    {
      obj = estimate_capability(data, pci_params, prior_mu, prior_sigma,
                                prior_predictive_params, sampler_params,
                                sequential_params)

      private$content_ = obj
    },
    summary = function() {
      base::summary(self$content)
    },
    plot_prior = function() {},
    plot_prior_predictive = function() {},
    plot_posterior = function() {},
    plot_sequential = function() {},
    new_sequential_params = function(at) {
      return(new_sequential_params(at))
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
    new_prior_predictive_params = function() {},
    new_rjags_params = function() {}
  )
)
