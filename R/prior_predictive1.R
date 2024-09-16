

is.pcj_prior_predictive1 = function(x) inherits(x, "pcj_prior_predictive1")


new_prior_predictive1 = function(
    pci_params,
    prior_mu,
    prior_sigma,
    prior_predictive_params
  )
{
  stopifnot(exprs = {
    is.pci_params(pci_params)
    is_valid__pci_params(pci_params)
    is_pcj_prior(prior_mu)
    is_pcj_prior(prior_sigma)
    !(is_pcj_point_prior(prior_mu) && is_pcj_point_prior(prior_sigma))
    is.prior_predictive_params(prior_predictive_params)
    is_valid__prior_predictive_params(prior_predictive_params)
  })

  if (is_pcj_point_prior(prior_sigma))
    stopifnot(prior_sigma > 0L)

  seed = prior_predictive_params$seed
  rng_kind = prior_predictive_params$rng_kind
  rng_version = prior_predictive_params$rng_version
  sample_size = prior_predictive_params$sample_size

  target = pci_params$target
  lsl = pci_params$lsl
  usl = pci_params$usl
  dl = pci_params$dl

  f = function() {
    RNGversion(rng_version)
    set.seed(seed, kind = rng_kind)
    stopifnot(RNGkind()[1L] == rng_kind)

    df = new_df(
      mu = pcj_rng(prior_mu, n = sample_size),
      sigma = pcj_rng(prior_sigma, n = sample_size)
    )

    df$data = stats::rnorm(sample_size, df$mu, df$sigma)

    if ("C_p" %in% pci_params$capability_indices)
      df$C_p =  pci::C_p(df$sigma, lsl, usl, dl)

    if ("C_pl" %in% pci_params$capability_indices)
      df$C_pl = pci::C_pl(df$mu, df$sigma, lsl, dl / 2L)

    if ("C_pu" %in% pci_params$capability_indices)
      df$C_pu = pci::C_pu(df$mu, df$sigma, usl, dl / 2L)

    if ("C_pk" %in% pci_params$capability_indices)
      df$C_pk = pci::C_pk(df$mu, df$sigma, lsl, usl, dl)

    if ("C_pm" %in% pci_params$capability_indices)
      df$C_pm = pci::C_pm(df$mu, df$sigma, target, lsl, usl, dl)

    df$p_nonconformance = 1L - bqc__norm__prob(df$mu, df$sigma, lsl, usl)
    df$p_nonconformance_above = bqc__norm__prob(df$mu, df$sigma, usl, Inf)
    df$p_nonconformance_below = bqc__norm__prob(df$mu, df$sigma, -Inf, lsl)

    df = subset(df, select = -c(mu, sigma))

    stopifnot(exprs = {
      nrow(df) == sample_size
      all(apply(df, 2L, vek::is_num_vec_xyz), na.rm = FALSE)
    })

    return(list(
      prior_predictive_sample = df,
      pci_params = pci_params,
      prior_mu = store_prior(prior_mu),
      prior_sigma = store_prior(prior_sigma),
      prior_predictive_params = prior_predictive_params,
      r_version = R.version$version.string
    ))
  }

  obj = pcj_safely(f())

  return(structure(
    obj,
    class = "pcj_prior_predictive1"
  ))
}


#' @export
get_sample.pcj_prior_predictive1 = function(object, x) {
  stopifnot(exprs = {
    is.pcj_prior_predictive1(object)
    vek::is_chr_vec_xb1(x)
  })

  return(object$result$prior_predictive_sample[, x]) # TODO check indexing behavior
}


#prior_interval_inference = function() {
#  table22 = bqc__bqc_six_sigma1_bayestools_jags__new_empty_interval_prob_table()
#  if (!is.null(interval)) {
#    interval_inference = bqc__bqc_six_sigma1_bayestools_jags__prior_predictive_interval_prob(
#      interval, prior_predictive_sample)
#
#    table22 = bqc_map_by_name(
#      x = list(mu = prior_mu, sigma = prior_sigma),
#      y = interval$obj,
#      \(x, y, id) {
#        data.frame(
#          name = id,
#          prob = bqc__bayestools__prior_interval_prob(x, y),
#          lower = y$a,
#          upper = y$b,
#          method = "BayesTools",
#          sample_size = 0L
#        )
#      }) |>
#      bqc__df__list_row_bind()
#
#    interval_inference = rbind(interval_inference, table22)
#    interval_inference$sample_size = 0L
#    table22 = interval_inference
#  }
#}
