

is.pcj_check = function(object) return(is_of_mono_class(object, "pcj_check"))


new_pcj_check = function(condition) {
  stopifnot(exprs = {
    is_list(condition)
    all(sapply_(condition, is_cond), na.rm = FALSE)
    # TODO check all conditions are unique
  })

  return(structure(list(condition = condition), class = "pcj_check"))
}


#' @export
get_condition.pcj_check = get_condition_
#' @export
get_error.pcj_check = get_error_
#' @export
get_warning.pcj_check = get_warning_
#' @export
get_message.pcj_check = get_message_


