

# TODO add 'root' (which is now 'parent'), and make parent the actual parent

PcjModelEntity = R6::R6Class(
  "PcjModelEntity",

  private = list(
    parent_ = NULL,
    what_ = NULL,
    distribution_ = NULL
  ),

  active = list(),

  public = list(
    initialize = function(what, distribution, parent) {
      stopifnot(exprs = {
        vek::is_chr_vec_xb1(what)
        vek::is_chr_vec_xb1(distribution)
        R6::is.R6(parent)
        distribution %in% c("prior", "prior_predictive", "posterior")
        what %in% variable.names(parent, distribution)
        is.null(private$parent_)
        is.null(private$what_)
        is.null(private$distribution_)
      })

      private$what_ = what
      private$distribution_ = distribution
      private$parent_ = parent

      return(invisible(self))
    },

    summary = function() {
      # TODO throws error
      o = summary(private$parent_)
      df = get_result(o)
      stopifnot(exprs = {
        is.data.frame(df)
        "what" %in% colnames(df)
        "distribution" %in% colnames(df)
      })

      df = subset.data.frame(
        df,
        subset = what == private$what_ & distribution == private$distribution_
      )

      row.names(df) = 1:nrow(df)

      o$result = df
      return(o)
    },

    mean = function(...) {
      return(mean(private$parent_, private$distribution_, private$what_, ...))
    },

    median = function(...) {
      return(stats::median(
        private$parent_, private$distribution_, private$what_, ...))
    },

    sd = function(...) {
      if (private$distribution_ == "prior") {
        stop('sd() is currently not supported for the prior distribution')
      }

      stat = get_result(private$parent_$content)$stat
      samples = get_sample(
        private$parent_$content, private$what_, private$distribution_, "all")

      stat_res = obtain_stat_result(samples, stat)
      sd_res = obtain_stat_sd(stat_res, ...)
      throw_first_error(sd_res)
      signal_warnings(sd_res)
      return(get_result(sd_res))
    },

    probability = function(value, ...) {
      return(probability(
        private$parent_,
        private$distribution_,
        private$what_,
        value,
        ...
      ))
    },

    quantile = function(value, ...) {
      return(stats::quantile(
        private$parent_,
        private$distribution_,
        private$what_,
        value,
        ...
      ))
    },

    plot_density = function(..., graphics = "lines") {
      f = switch(
        private$distribution_,
        "prior" = plot_prior_density,
        "prior_predictive" = plot_prior_predictive_density,
        "posterior" = plot_posterior_density,
        stop()
      )

      return(f(private$parent_, ..., what = private$what_, graphics = graphics))
    }
  )
)
