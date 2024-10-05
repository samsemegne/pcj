

is.pcj_sequential_analysis1 = function(object, ...) {
  is_of_mono_class(object, "pcj_sequential_analysis1")
}


sequential_analysis = function(
    data,
    prior_mu,
    prior_sigma,
    pci_params,
    sampler_params,
    sequential_params
  )
{
  stopifnot(exprs = {
    vek::is_num_vec_xyz(data)
    length(data) > 1L
    is.pci_params(pci_params)
    is_valid__pci_params(pci_params)
    is_pcj_prior(prior_mu)
    is_pcj_prior(prior_sigma)
    !(is_pcj_point_prior(prior_mu) && is_pcj_point_prior(prior_sigma))
    is.rjags_params(sampler_params)
    is_valid__rjags_params(sampler_params)
    all(names(sampler_params$initial_values) %in% c("mu", "sigma"),
        na.rm = FALSE)
    is.sequential_params(sequential_params)
    is_valid__sequential_params(sequential_params)
  })

  if (is_pcj_point_prior(prior_sigma))
    stopifnot(prior_sigma > 0L)

  at = sequential_params$at

  result = list()
  for (i in 1:length(at)) {
    n = at[i]
    dat = utils::head(data, n)
    obj = new_pcj_model1(
      dat,
      pci_params,
      prior_mu,
      prior_sigma,
      sampler_params
    )

    result[[length(result) + 1L]] = obj
  }

  return(structure(
    list(
      fit = result,
      sequential_params = sequential_params
    ),
    class = "pcj_sequential_analysis1"
  ))
}


# TODO implement get_error etc
