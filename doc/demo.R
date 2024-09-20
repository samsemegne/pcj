## ----setup--------------------------------------------------------------------
library(pcj)
library(pci)

## -----------------------------------------------------------------------------
set.seed(1L)
data = stats::rnorm(30L)

model = pcj::estimate_capability(
  data,
  pcj::new_pci_params(c("C_pl", "C_pu", "C_pk", "C_pm"), 0, -3, 3, 6),
  "dnorm(0, 1)T(-1, 5)",
  "dexp(1)T(1, 4)",
  pcj::new_prior_predictive_params(2000L, 123L),
  pcj::new_rjags_params(50L, 100L, 1L, 4L, 123L, "base::Wichmann-Hill"),
  pcj::new_sequential_params(c(15L, 20L, 25L, 30L))
)

## -----------------------------------------------------------------------------
pci::C_p(stats::sd(data), -3, 3, 6)
pci::C_pk(mean(data), stats::sd(data), -3, 3, 6)
pci::C_pm(mean(data), stats::sd(data), 0, -3, 3, 6)

## -----------------------------------------------------------------------------
options(pcj.graphics_driver = "graphics")

pcj::plot_prior(model, x = "mu")
pcj::plot_prior(model, x = "sigma")
pcj::plot_prior_predictive(model, x = "C_pk")

options(pcj.graphics_driver = "ggplot2")

pcj::plot_posterior(model, x = "mu")
pcj::plot_posterior(model, x = "sigma")
pcj::plot_posterior(model, x = "C_pk") 

## ----fig.width=5, fig.height=5------------------------------------------------
pcj::plot_sequential(model, x = "C_pk", position = "ridges")

## ----fig.width=5, fig.height=5------------------------------------------------
polspline_density = \(x, ...) {
    fit = polspline::logspline(x)
    return(\(k, ...) polspline::dlogspline(k, fit))
}

options(pcj.graphics_driver = "graphics")

pcj::plot_sequential(model, x = "C_pk", position = "ridges", stat = polspline_density)

## -----------------------------------------------------------------------------
model = pcj::estimate_capability(
  data,
  pcj::new_pci_params(c("C_p"), 0, -3, 3, 6),
  0,
  "dexp(3)",
  pcj::new_prior_predictive_params(2000L, 123L),
  pcj::new_rjags_params(500L, 2000L, 1L, 4L, 123L, "base::Wichmann-Hill"),
  pcj::new_sequential_params(c(15L, 20L, 25L, 30L))
)

## -----------------------------------------------------------------------------
polspline_stats = \(x, ...) {
    fit = polspline::logspline(x)
    return(list(
      density = \(k, ...) polspline::dlogspline(k, fit),
      quantile = \(k, ...) polspline::qlogspline(k, fit)
    ))
} 

options(pcj.graphics_driver = "ggplot2")

pcj::plot_prior(model, x = "mu")
pcj::plot_prior(model, "arrows", x = "mu")

pcj::plot_prior(model, "area", x = "sigma")

pcj::plot_posterior(model, x = "C_p")


options(pcj.graphics_driver = "graphics")
pcj::plot_posterior(model, "area", x = "C_p", at = c("q.025", "q.975"))
pcj::plot_posterior(model, "points", x = "C_p", at = c("mean", "median"),
                    add = TRUE, pch = 19)

pcj::plot_posterior(model, x = "C_p", add = TRUE)


normal_stat = \(x, ...) {
  mu = mean(x)
  sigma = stats::sd(x)
  return(\(k, ...) stats::dnorm(k, mu, sigma))
}

# The way "pcj_plot_object" interacts with ggplot objects must yet be adjusted,
# currently 2 objects are rendered unintentionally, but the it demonstrates the 
# goal.
library(jaspGraphs)
options(pcj.graphics_driver = "ggplot2")
plot(pcj::plot_posterior(model, x = "C_p", stat = normal_stat)) +
  pcj::plot_posterior(model, x = "C_p", add = TRUE, col = "steelblue", lwd = 2) +
  jaspGraphs::themeJaspRaw() +
  jaspGraphs::geom_rangeframe()


## -----------------------------------------------------------------------------
summary(model$sequential_analysis$fit[[4L]])

