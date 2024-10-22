

PcjBayesDistribution = R6::R6Class(
  "PcjBayesDistribution",

  private = list(
    parent_ = NULL,
    distribution_ = NULL,
    x_ = list()
  )
)


create_bayes_distribution = function(distribution, x, parent) {
  stopifnot(exprs = {
    vek::is_chr_vec_xb1(distribution)
    vek::is_chr_vec_x(x)
    is_all_unique(x)
    R6::is.R6(parent)
  })

  entities = lapply(x, \(k) {
    return(PcjModelEntity$new(k, distribution, parent))
  })

  names(entities) = x

  gen = R6::R6Class(
    inherit = PcjBayesDistribution,
    class = FALSE,
    public = list(
      initialize = function(distribution, x, parent) {
        stopifnot(exprs = {
          vek::is_chr_vec_xb1(distribution)
          is_list(x)
          # TODO check x for R6 etc
          R6::is.R6(parent)
          is.null(private$parent_)
          is.null(private$distribution)
          is_empty(private$x_)
        })

        private$distribution_ = distribution
        private$x_ = x
        private$parent_ = parent
        return(invisible(self))
      }
    )
  )

  # Dynamically add active fields for each variable.
  for (x_ in x) {
    field_func = sprintf("
      function(value) {
        if (missing(value))
          return(private$x_$`%s`)
        else
          stop(runtimeError('Runtime error'))
      }
    ", x_)

    gen$set("active", x_, eval(str2lang(field_func)))
  }

  obj = gen$new(distribution, entities, parent)

  return(obj)
}
