
new_bounds = function(
  target, lower, is_lower_inclusive, upper, is_upper_inclusive)
{
  stopifnot(exprs = {
    vek::is_chr_vec_nxb1(target)
    vek::is_num_vec_nxy1(lower)
    vek::is_lgl_vec_nx1(is_lower_inclusive)
    vek::is_num_vec_nxy1(upper)
    vek::is_lgl_vec_nx1(is_upper_inclusive)
  })

  return(list(
    type = "bounds",
    target = target,
    lower = lower,
    is_lower_inclusive = is_lower_inclusive,
    upper = upper,
    is_upper_inclusive = is_upper_inclusive
  ))
}


get_var_info = function() {

  new_df = function(...) {
    return(data.frame(
      ..., row.names = NULL, check.rows = FALSE, check.names = TRUE,
      fix.empty.names = TRUE, stringsAsFactors = FALSE
    ))
  }

  info__mu = new_df(
    var_id = "mu",
    name_r = "mu",
    name_latex = "\\mu",
    name_r_expr = "mu",
    attributes = I(list(
      new_bounds("mu", -Inf, FALSE, Inf, FALSE)
    ))
  )

  info__sigma = new_df(
    var_id = "sigma",
    name_r = "sigma",
    name_latex = "\\sigma",
    name_r_expr = "sigma",
    attributes = I(list(
      new_bounds("sigma", 0L, TRUE, Inf, FALSE)
    ))
  )

  info__p_nonconformance = new_df(
    var_id = "p_nonconformance",
    name_r = "p_nonconformance",
    name_latex = "\\text{P}(\\text{Nonconformance})",
    name_r_expr = "P(Nonconformance)",
    attributes = I(list(
      new_bounds("p_nonconformance", 0L, TRUE, 1L, TRUE)
    ))
  )

  info__p_nonconformance_below = new_df(
    var_id = "p_nonconformance_below",
    name_r = "p_nonconformance_below",
    name_latex = "\\text{P}(\\text{Nonconformance Below})",
    name_r_expr = "P(Nonconformance~Below)",
    attributes = I(list(
      new_bounds("p_nonconformance", 0L, TRUE, 1L, TRUE)
    ))
  )

  info__p_nonconformance_above = new_df(
    var_id = "p_nonconformance_above",
    name_r = "p_nonconformance_above",
    name_latex = "\\text{P}(\\text{Nonconformance Above})",
    name_r_expr = "P(Nonconformance~Above)",
    attributes = I(list(
      new_bounds("p_nonconformance_above", 0L, TRUE, 1L, TRUE)
    ))
  )

  info__data = new_df(
    var_id = "data",
    name_r = "data",
    name_latex = "Y", # TODO
    name_r_expr = "Data",
    attributes = I(list(
      new_bounds("data", -Inf, FALSE, Inf, FALSE)
    ))
  )

  info = rbind.data.frame(
    info__mu,
    info__sigma,
    info__p_nonconformance,
    info__p_nonconformance_below,
    info__p_nonconformance_above,
    info__data,

    deparse.level = 1,
    make.row.names = TRUE,
    stringsAsFactors = FALSE,
    factor.exclude = TRUE
  )

  row.names(info) = info$var_id

  pci_info = pci::pci_info[
    c("C_p", "C_pl", "C_pu", "C_pk", "C_pm"),
    c("pci_id", "name_r", "name_latex", "name_r_expr", "attributes")
  ]

  cn = colnames(pci_info)
  cn[1L] = "var_id"
  colnames(pci_info) = cn

  info = rbind(info, pci_info)

  return(info)
}
