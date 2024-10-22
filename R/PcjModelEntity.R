

PcjModelEntity = R6::R6Class(
  "PcjModelEntity",

  private = list(
    parent_ = NULL,
    x_ = NULL,
    distribution_ = NULL
  ),

  active = list(),

  public = list(
    initialize = function(x, distribution, parent) {
      stopifnot(exprs = {
        vek::is_chr_vec_xb1(x)
        vek::is_chr_vec_xb1(distribution)
        R6::is.R6(parent)
        distribution %in% c("prior", "prior_predictive", "posterior")
        x %in% variable.names(parent, distribution)
        is.null(private$parent_)
        is.null(private$x_)
        is.null(private$distribution_)
      })

      private$x_ = x
      private$distribution_ = distribution
      private$parent_ = parent

      return(invisible(self))
    },

    summary = function() {
      o = summary(private$parent_)
      df = get_result(o)
      stopifnot(exprs = {
        is.data.frame(df)
        "x" %in% colnames(df)
        "distribution" %in% colnames(df)
      })

      df = subset.data.frame(
        df,
        subset = x == private$x_ & distribution == private$distribution_
      )

      o$result = df
      return(o)
    },

    probability = function(value) {
      return(probability(
        private$parent_,
        private$x_,
        private$distribution_,
        value
      ))
    },

    quantile = function(value) {
      return(quantile(
        private$parent_,
        private$x_,
        private$distribution_,
        value
      ))
    }
  )
)
