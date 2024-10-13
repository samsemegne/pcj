

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
#' @export
get_result.pcj_result = get_result_

