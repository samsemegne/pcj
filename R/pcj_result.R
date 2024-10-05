

is.pcj_result = function(object) is_of_mono_class(object, "pcj_result")


pcj_safely = function(expr) {
  condition = list()
  result = NULL
  has_e = FALSE

  result = withCallingHandlers({
    tryCatch({
      expr
    }, error = \(e) {
      condition[[length(condition) + 1L]] <<- e
      has_e <<- TRUE
    })
  }, condition = \(cond) {
    condition[[length(condition) + 1L]] <<- cond
  })

  if (has_e)
    result = NULL

  out = list()
  #out = utils::capture.output({
  #}, file = NULL, append = FALSE, type = "output", split = FALSE)

  #out = trimws(out, which = "both", whitespace = "[ \t\r\n]")
  #out = out[out != ""] |>
  #  as.list()

  return(
    structure(list(
      condition = condition,
      output = out,
      result = result
    ), class = "pcj_result")
  )
}


#' @export
get_error.pcj_result = get_error_
#' @export
get_warning.pcj_result = get_warning_
#' @export
get_message.pcj_result = get_message_
#' @export
get_condition.pcj_result = get_condition_




#pcj_safely = function(expr) {
#  warnings_ = list()
#  error_ = NULL
#  other = list()
#  condition = list()
#  result = NULL
#
#  result = withCallingHandlers({
#    tryCatch({
#      expr
#    }, error = \(e) {
#      error_ <<- e
#    })
#  }, condition = \(cond) {
#    condition[[length(condition) + 1L]] <<- cond
#  })
#
#  out = list()
#  #out = utils::capture.output({
#  #}, file = NULL, append = FALSE, type = "output", split = FALSE)
#
#  #out = trimws(out, which = "both", whitespace = "[ \t\r\n]")
#  #out = out[out != ""] |>
#  #  as.list()
#
#  warnings_ = Filter(\(k) inherits(k, "warning", FALSE), condition)
#  other = Filter(\(k) !inherits(k, "warning", FALSE), condition)
#
#  if (length(warnings_) == 0L)
#    warnings_ = NULL
#
#  if (!is.null(error_))
#    result = NULL
#
#  if (length(other) == 0L)
#    other = NULL
#
#  return(list(
#    error = error_,
#    warnings = warnings_,
#    condition = other,
#    output = out,
#    result = result
#  ))
#}
