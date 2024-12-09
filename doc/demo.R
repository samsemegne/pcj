## ----setup--------------------------------------------------------------------
library(pcj)

# Default libraries.
library(grDevices)
library(graphics)
library(stats)

## -----------------------------------------------------------------------------
set.seed(1L)
data = stats::rnorm(30L, mean = .7, sd = 1L)

plot(stats::density(data), main = "Data")
graphics::hist(data, add = TRUE, freq = FALSE, col = NA)
graphics::rug(data)

## -----------------------------------------------------------------------------
model = pcj:::new_pcj_process_capability_model1(
  data,
  pcj::new_pci_parameters1(c("C_p", "C_pk", "C_pm"), 0, -3, 3, 1),
  "dnorm(.7, 1)T(-2, 1.5)",
  "dexp(1)T(1, 4)",
  pcj::new_prior_predictive_parameters(100L, 1L),
  pcj::new_rjags_parameters(50L, 100L, 1L, 4L, 123L, "base::Wichmann-Hill"),
  #stat = f,
  evaluate = TRUE
)

## -----------------------------------------------------------------------------
if (length(get_error(model)) > 0L) {
  # ...
}

if (length(get_warning(model)) > 0L) {
  # ...
}

## -----------------------------------------------------------------------------
summary(model)

## -----------------------------------------------------------------------------
mean(model, "posterior", "C_pm")

stats::median(model, "posterior", "C_pm")

stats::quantile(model, "posterior", "C_pm", c(.25, .5, .75))

probability(model, "posterior", "C_pm", ">= 1")

## ----fig.height = 7.5, fig.width = 6------------------------------------------
layout(t(matrix(1:6, 2L)))

pcj::plot_prior_density(model, what = "mu") 

pcj::plot_prior_density(model, what = "sigma")

pcj::plot_prior_predictive_density(model, what = "C_pm")

pcj::plot_posterior_density(model, what = "C_pm")

pcj::plot_prior_predictive_density(model, what = "p_nonconformance")

# Note. These functions return objects, which can be assigned to variables.
your_plot = pcj::plot_posterior_density(model, what = "p_nonconformance")
plot(your_plot)


## -----------------------------------------------------------------------------
pcj::plot_posterior_density(model, what = "C_pm")

pcj::plot_posterior_density(
  model, what = "C_pm", graphics = "area",
  x = structure(">= q.025 & <= q.975", n = 100L),
  col = grDevices::adjustcolor("black", alpha.f = .1), add = TRUE)

pcj::plot_posterior_density(
  model, what = "C_pm", graphics = "points", x = "mean", pch = 19L, 
  add = TRUE)

## -----------------------------------------------------------------------------
seq_proc = pcj::new_pcj_sequential_procedure(model, c(20L, 25L, 30L))

## -----------------------------------------------------------------------------
summary(seq_proc) |>
  get_result() |>
  subset(what == "C_pm" & distribution == "posterior")

## ----fig.height = 6, fig.width = 6--------------------------------------------
pcj::plot_sequential_procedure(seq_proc, what = "C_pm", display = "ridges")

## -----------------------------------------------------------------------------
options(pcj.graphics_driver = "ggplot2")

## -----------------------------------------------------------------------------
c(
  pcj::plot_posterior_density(model, what = "C_pm"),
  pcj::plot_posterior_density(
    model, what = "C_pm", graphics = "area",
    x = structure(">= q.025 & <= q.975", n = 100L),
    col = grDevices::adjustcolor("black", alpha.f = .1), add = TRUE
  ),
  pcj::plot_posterior_density(
    model, what = "C_pm", graphics = "points", x = c("mean"), pch = 19L, 
    add = TRUE)
)

## -----------------------------------------------------------------------------
# Reset the graphics driver.
options(pcj.graphics_driver = "graphics")

## -----------------------------------------------------------------------------
model = pcj::PcjProcessCapabilityModel1$new()

model$update(
  data,
  model$new_pci_parameters1(c("C_p", "C_pk", "C_pm"), 0, -3, 3, 1),
  "dnorm(.7, 1)T(-2, 1.5)",
  "dexp(1)T(1, 4)",
  model$new_prior_predictive_parameters(100L, 1L),
  model$new_rjags_parameters(50L, 100L, 1L, 4L, 123L, "base::Wichmann-Hill")
)

model$run()

## -----------------------------------------------------------------------------
if (length(model$error) > 0L) {
  # ...
}

if (length(model$warning) > 0L) {
  # ...
}

## -----------------------------------------------------------------------------
model$summary()

## -----------------------------------------------------------------------------
model$posterior$C_pm$mean()

model$posterior$C_pm$median()

model$posterior$C_pm$quantile(c(.25, .5, .75))

model$posterior$C_pm$probability(">= 1")

model$posterior$C_pm$sd()

## -----------------------------------------------------------------------------
model$prior$mu$plot_density()

## -----------------------------------------------------------------------------
model$posterior$C_pm$plot_density(ylim = c(0., 1.))

f = function(x) {
  mu = mean(x)
  sigma = stats::sd(x)
  return(list(
    density = \(...) return(\(x) return(stats::dnorm(x, mu, sigma, FALSE))),
    quantile = \(q, ...) return(stats::qnorm(q, mu, sigma, TRUE, FALSE)),
    mean = \(...) return(mu),
    median = \(...) return(mu),
    sd = \(...) return(sigma)
  ))
}

model$update(stat = f)
model$run()

model$posterior$C_pm$plot_density(
  x = structure(">= q.001 & <= q.999", n = 1000L),
  add = TRUE, col = "steelblue", lwd = 1.5)

