

is.pci_params = function(x) is_of_mono_class(x, "pci_params")


#' @export
new_pci_params = function(
    capability_indices,
    target,
    lsl,
    usl,
    dl
  )
{
  stopifnot(exprs = {
    vek::is_chr_vec_xb(capability_indices)
    vek::is_num_vec_xyz1(target)
    vek::is_num_vec_xyz1(lsl)
    vek::is_num_vec_xyz1(usl)
    vek::is_num_vec_xyz1(dl)
    length(capability_indices) > 0L
    is_all_unique(capability_indices)
    all(capability_indices %in% get_pci_params1_supported_pci(), na.rm = FALSE)
    usl > lsl
    target > lsl
    usl > target
    dl > 0L
  })

  return(structure(
    list(
      capability_indices = capability_indices,
      target = target,
      lsl = lsl,
      usl = usl,
      dl = dl
    ),
    class = "pci_params"
  ))
}



is_valid__pci_params = function(x) {
  stopifnot(is.pci_params(x))
  return(all(
    is_uniquely_named_list(unclass(x)),
    identical(names(x), get_pci_params_names()),
    vek::is_chr_vec_xb(x$capability_indices),
    vek::is_num_vec_xyz1(x$target),
    vek::is_num_vec_xyz1(x$lsl),
    vek::is_num_vec_xyz1(x$usl),
    vek::is_num_vec_xyz1(x$dl),
    length(x$capability_indices) > 0L,
    is_all_unique(x$capability_indices),
    all(x$capability_indices %in% get_pci_params1_supported_pci(),
        na.rm = FALSE),
    x$usl > x$lsl,
    x$target > x$lsl,
    x$usl > x$target,
    x$dl > 0L,
    na.rm = FALSE
  ))
}


get_pci_params1_supported_pci = function() {
  return(get_supported_pci())
}


get_pci_params_names = function() {
  return(c("capability_indices", "target", "lsl", "usl", "dl"))
}
