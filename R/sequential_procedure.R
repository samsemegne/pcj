

is.pcj_sequential_procedure = function(object, ...) {
  is_of_mono_class(object, "pcj_sequential_procedure")
}


#' @export
new_pcj_sequential_procedure = function(
    model,
    at
  )
{
  stopifnot(exprs = {
    # TODO check model

    vek::is_int_vec_x(at)
    length(at) > 1L
    !is.unsorted(at, na.rm = FALSE, strictly = TRUE)
    all(at > 0L, na.rm = FALSE) # TODO min sample size?
  })

  result = list()
  for (i in 1:length(at)) {
    n = at[i]
    dat = utils::head(get_data(model), n)
    obj = update(model, data = dat, evaluate = TRUE)

    result[[length(result) + 1L]] = obj
  }

  has_e = sapply_(result, has_error)
  stopifnot(exprs = {
    vek::is_lgl_vec_x(has_e)
    length(has_e) == length(result)
  })

  w = list()
  if (any(has_e, na.rm = FALSE))
    w = list(simpleWarning('Some models exited with errors'))

  obj = new.env(TRUE, parent.frame(1L), NA)
  obj$result = list(fit = result, at = at)
  obj$condition = w
  obj$output = list()

  class(obj) = "pcj_sequential_procedure"

  lockEnvironment(obj, bindings = TRUE)

  return(obj)
}


#' @export
update.pcj_sequential_procedure = function(object, ...) {
  stopifnot(is.pcj_sequential_procedure(object))

  if (...length() > 0L) {
    stopifnot(exprs = {
      !is.null(...names())
      all(...names() %in% names(formals(new_pcj_sequential_procedure)),
          na.rm = FALSE)
      is_all_unique(...names())
    })
  }

  dots = list(...)

  res = get_result(object)
  last_model = res$fit[[length(res$fit)]]

  #browser()

  obj = new_pcj_sequential_procedure(
    dots$model %||% last_model,
    dots$at %||%res$at
  )

  return(obj)
}


#' @export
get_error.pcj_sequential_procedure = get_error_
#' @export
get_warning.pcj_sequential_procedure = get_warning_
#' @export
get_message.pcj_sequential_procedure = get_message_
#' @export
get_condition.pcj_sequential_procedure = get_condition_
#' @export
get_result.pcj_sequential_procedure = get_result_


#' @export
summary.pcj_sequential_procedure = function(object, ...) {
  stopifnot(is.pcj_sequential_procedure(object))

  results = lapply(get_result(object)$fit, summary)

  dfs = lapply(results, get_result)
  df = do.call(rbind.data.frame, dfs)
  row.names(df) = 1:nrow(df)

  cond_ = lapply(results, get_condition)
  cond = list()
  for (k in cond_)
    cond = c(cond, k)

  res = list(
    condition = cond,
    output = list(),
    result = df
  )

  class(res) = "pcj_sequential_procedure_summary"

  return(res)
}


#' @export
get_error.pcj_sequential_procedure_summary = get_error_
#' @export
get_warning.pcj_sequential_procedure_summary = get_warning_
#' @export
get_message.pcj_sequential_procedure_summary = get_message_
#' @export
get_condition.pcj_sequential_procedure_summary = get_condition_
#' @export
get_result.pcj_sequential_procedure_summary = get_result_


#' @export
print.pcj_sequential_procedure_summary = function(object, ...) {
  stopifnot(is_of_mono_class(object, "pcj_sequential_procedure_summary"))

  if (has_error(object))
    stop(get_error(object)[[1L]])

  if (has_warning(object)) {
    for (w in get_warning(object))
      warning(w)
  }

  print.data.frame(get_result(object), ...)

  return(invisible(object))
}

