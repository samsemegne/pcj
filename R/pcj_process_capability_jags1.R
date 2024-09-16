


is.pcj_process_capability_jags1 = function(x) {
  is_of_mono_class(x, "pcj_process_capability_jags1")
}


#' @export
estimate_capability = function(
    data,
    pci_params,
    prior_mu,
    prior_sigma,
    prior_predictive_params,
    sampler_params,
    sequential_params
  )
{
  stopifnot(exprs = {
    vek::is_num_vec_xyz(data)
    length(data) > 1L
    is.pci_params(pci_params)
    is_valid__pci_params(pci_params)
    vek::is_chr_vec_xb1(prior_mu) || is_pcj_point_prior(prior_mu)
    vek::is_chr_vec_xb1(prior_sigma) || is_pcj_point_prior(prior_sigma)
    !(is_pcj_point_prior(prior_mu) && is_pcj_point_prior(prior_sigma))
    is.rjags_params(sampler_params)
    is_valid__rjags_params(sampler_params)
    is.sequential_params(sequential_params)
    is_valid__sequential_params(sequential_params)
  })

  if (vek::is_chr_vec_xb1(prior_mu)) {
    prior_mu = parse_jags_dist(prior_mu)
    stopifnot(is.pcj_jags_dist(prior_mu))
  }

  if (vek::is_chr_vec_xb1(prior_sigma)) {
    prior_sigma = parse_jags_dist(prior_sigma)
    stopifnot(is.pcj_jags_dist(prior_sigma))
  }

  # Generate prior predictive samples.
  prior_study_obj = new_prior_predictive1(
    pci_params,
    prior_mu,
    prior_sigma,
    prior_predictive_params
  )

  pcj_model1 = NULL
  sequential = NULL
  if (is.null(sequential_params)) {
    # Estimate a single model using all data.
    pcj_model1 = rjags_estimate(
      data,
      pci_params,
      prior_mu,
      prior_sigma,
      sampler_params
    )
  } else {
    # Run a sequential analysis.
    sequential = sequential_analysis(
      data,
      prior_mu,
      prior_sigma,
      pci_params,
      sampler_params,
      sequential_params
    )
  }

  final_obj = list(
    prior_study = prior_study_obj,
    pcj_model1 = pcj_model1,
    sequential_analysis = sequential
  )

  class(final_obj) = "pcj_process_capability_jags1"

  return(final_obj)
}

