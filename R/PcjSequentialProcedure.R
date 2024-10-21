

#' @export
PcjSequentialProcedure = R6::R6Class(
  "PcjSequentialProcedure",
  private = list(
    content_ = NULL
  ),

  active = list(
    content = function(value) {
      if (missing(value))
        if (is.null(private$content_))
          stop(runtimeError("Runtime error"))
      else
        return(private$content_)
      else
        stop(runtimeError("Runtime error"))
    },


    condition = function(value) {
      if (missing(value))
        return(get_condition(self$content))
      else
        stop(runtimeError("Runtime error"))
    },

    error = function(value) {
      if (missing(value))
        return(get_error(self$content))
      else
        stop(runtimeError("Runtime error"))
    },

    warning = function(value) {
      if (missing(value))
        return(get_warning(self$content))
      else
        stop(runtimeError("Runtime error"))
    },

    message = function(value) {
      if (missing(value))
        return(get_message(self$content))
      else
        stop(runtimeError("Runtime error"))
    },

    output = function(value) {
      if (missing(value))
        return(get_output(self$content))
      else
        stop(runtimeError("Runtime error"))
    },

    model_list = function(value) {
      if (missing(value))
        return(get_result(self$content)$fit)
      else
        stop(runtimeError("Runtime error"))
    },

    max_n_model = function(value) {
      if (missing(value))
        return(self$model_list[[length(self$model_list)]])
      else
        stop(runtimeError("Runtime error"))
    }
  ),

  public = list(
    update = function(model, at) {
      if (is.null(private$content_)) {
        if (any(missing(model), missing(at), na.rm = FALSE)) {
          stop('All required parameters must be specified')
        }

        obj = new_pcj_sequential_procedure(model, at)
      } else {
        args = list(object = self$content)
        if (!missing(model))
          args$model = model
        if (!missing(at))
          args$at = at

        #browser()

        obj = do.call(update, args)
      }

      private$content_ = obj
      #browser()
    },

    summary = function() return(summary(self$content)),

    plot = function(
        x,
        ...,
        show_prior = TRUE,
        display = "stack",
        draw_order = -1L,
        condition_action = "omit_if_error",
        offset = c(0L, 0L)
      )
    {
      return(plot_sequential_procedure(
        self$content,
        ...,
        x = x,
        show_prior = show_prior,
        display = display,
        draw_order = draw_order,
        condition_action = condition_action,
        offset = offset
      ))
    }
  )
)



#' @export
get_condition.PcjSequentialProcedure = function(object) return(object$condition)
#' @export
get_error.PcjSequentialProcedure = function(object) return(object$error)
#' @export
get_warning.PcjSequentialProcedure = function(object) return(object$warning)
#' @export
get_message.PcjSequentialProcedure = function(object) return(object$message)
#' @export
get_output.PcjSequentialProcedure = function(object) return(object$output)
#' @export
get_result.PcjSequentialProcedure = function(object) {
  return(get_result(object$content))
}


#' @export
update.PcjSequentialProcedure = function(object, ...) {
  return(object$update(...))
}

