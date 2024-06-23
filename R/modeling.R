# library(tidyverse)
# library(metafor)
# library(formulaic)
# library(clubSandwich)
# library(here)
# library(qs)

### Basemodel and Diagnostics===================================================
rma_basemodel <- function(
    plot_tag,
    profile_plot = FALSE,
    es_plot = FALSE,
    data,
    vcv,
    prior_model = NULL,
    influential = NULL) {

  if(!is.null(influential)) {
    influential_index <- which(data$std_id %in% influential)

    data <- data[-influential_index, ]
    vcv <- vcv[-influential_index, -influential_index]
  }

  ### Random-effects: standard meta-analytic 2-level model.
  # Random "effect size" effects are nested within random "study" effects
  # but accounting for dependent effect sizes within study via a specified
  # variance-covariance matrix ("V").
  # rma.mv does not support the knapp-hartung method, however, the t
  # distribution is better than the z default.
  # The `control` argument was set in order to avoid local maximums in the
  # optimization algorithm.
  # Default method is restricted maximum liklelihood (REML) - can also set as
  # maximum likelihood (ML).
  # dat_es <- data[["yi"]]

  res <- rma.mv(
    yi = data$yi,
    V = vcv,
    data = data,
    random = ~ 1 | std_id / es_id,
    control = list(sigma2.init=0.5),
    test = "t",
    dfs = "contain",
    slab = std_id
  ) |> robust(std_id, clubSandwich = TRUE)

  ### adding statistics to model object
  sigma_ci <- confint(res)

  sigma2.ci <- list(
    sigma2.ci.lb = c(sigma_ci[[1]]$random[1,2], sigma_ci[[2]]$random[1,2]),
    sigma2.ci.ub = c(sigma_ci[[1]]$random[1,3], sigma_ci[[2]]$random[1,3])
  )

  # 95% CI and CI for ES estimates
  pred <- predict(res)

  estimate_int <- list(
    estimate.ci.lb = pred$ci.lb,
    estimate.ci.ub = pred$ci.ub,
    estimate.pi.lb = pred$pi.lb,
    estimate.pi.ub = pred$pi.ub
  )

  # necessary to keep `rma.mv...` object class
  res <- structure(c(res, sigma2.ci, estimate_int), class = class(res))

  ### plots
  if (profile_plot) {
    png(
      str_glue("figures/profile_1_{plot_tag}.png"),
      width = 700,
      height = 325,
      type = "windows",
      bg = "white"
    )

    profile(res, sigma2 = 1, steps = 20, cline = TRUE)
    abline(v = sigma_ci[[1]]$random[1,2:3], lty = "dotted")
    dev.off()

    png(
      str_glue("figures/profile_2_{plot_tag}.png"),
      width = 700,
      height = 325,
      type = "windows",
      bg = "white"
    )

    profile(res, sigma2 = 2, steps = 20, cline = TRUE)
    abline(v = sigma_ci[[2]]$random[1,2:3], lty = "dotted")
    dev.off()
  }

  if (es_plot) {
    plot_es_scatter(
      res = res,
      model_full = prior_model,
      plot_tag = plot_tag,
      influential = influential
    )
  }

  return(res)
}

### Influential Cases, Cook's Distances ========================================
diagnostic_cooks <- function(plot_tag, data_model, threshold_influential) {

  # identify influential cases
  cooks <- cooks.distance.rma.mv(data_model, cluster = std_id)

  threshold <- threshold_influential * mean(cooks)
  influential_std_id <- names(cooks[cooks > threshold])

  plot_cooks(
    cooks_distances = cooks,
    plot_name = plot_tag,
    threshold = threshold
  )

  return(influential_std_id)
}

# diagnostic_cooks <- function(
#     plot_name,
#     profile_plot,
#     es_plot,
#     data_model,
#     data_es,
#     data_matrix) {
#
#   # identify influential cases
#   cooks <- cooks.distance.rma.mv(data_model, cluster = std_id)
#
#   influential_threshold <- 5 * mean(cooks)
#   influential_std_id <- names(cooks[cooks > influential_threshold])
#
#   plot_cooks(
#     cooks_distances = cooks,
#     plot_name = plot_name,
#     threshold = influential_threshold
#   )
#
#   # generate `_out` es_data and vcv_matrix
#   data_es_out <- filter(data_es, !(std_id %in% influential_std_id))
#
#   influential_indices <- which(data_es$std_id %in% influential_std_id)
#
#   data_matrix_out <- data_matrix[-influential_indices, -influential_indices]
#
#   plot_name <- str_glue("{plot_name}_out")
#
#   # redo `rma_basemodel`
#   res <- rma_basemodel(
#     plot_name = plot_name,
#     profile_plot = TRUE,
#     es_plot = TRUE,
#     data = data_es_out,
#     vcv = data_matrix_out
#   )
#
#   # replot cooks
#   cooks <- cooks.distance.rma.mv(res, cluster = std_id)
#
#   plot_cooks(
#     cooks_distances = cooks,
#     plot_name = plot_name,
#     threshold = FALSE
#   )
#
#   return(res)
# }

# ===== Helper functions for `rma_basemodel` ===================================
### Cook's Distances Plot
plot_cooks <- function(cooks_distances, threshold, plot_name) {
  file_name <- str_glue("cooks_distance_{plot_name}.png")

  cooks_data <- tibble(
    x_labels = names(cooks_distances),
    x_values = unname(cooks_distances)
  )

  p <- ggplot(cooks_data, aes(x = x_labels, y = x_values)) +
    geom_point(color = "black") +
    geom_line(aes(group = 1), color = "black") +
    geom_hline(yintercept = mean(cooks_data$x_values), linetype = "dashed", color = "red") +
    geom_hline(yintercept = 0, linetype = "solid", color = "black") +
    labs(
      title = "Cook's Distances",
      x = "Study",
      y = "Cook's Distance"
    ) +
    theme_classic() +
    theme(
      axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 0.5),
    ) +
    # BEGIN SEC: to put the x-axis labels on the y = 0 line
    scale_y_continuous(
      expand = expansion(mult = c(0, 0.05)),  # Ensure enough space below 0
      # sec.axis = dup_axis(name = NULL)  # Duplicate y-axis on the right
    ) +
    expand_limits(y = 0)
  # END SEC
  if (is.numeric(threshold)) {
    p <- p +
      scale_x_discrete(
        breaks = cooks_data$x_labels[cooks_data$x_values > threshold],
        labels = cooks_data$x_labels[cooks_data$x_values > threshold]
      ) +
      geom_hline(yintercept = threshold, linetype = "dotted", color = "blue")
  } else {
    p <- p +
      scale_x_discrete(
        labels = NULL
      )
  }

  ggsave(here("figures", file_name), plot = p, width = 7, height = 3.25, units = "in")
}

### Effect Size Scatterplot
plot_es_scatter <- function(
    res,
    model_full = NULL,
    plot_tag,
    influential =NULL) {

  dat_plot <- data.frame(
    std_id = res$mf.r[[1]]$std_id,
    yi = res$yi.f,
    weight = weights.rma.mv(res))

  if (!is.null(influential)) {
    dat_out <- data.frame(
      std_id = model_full$mf.r[[1]]$std_id,
      yi = model_full$yi.f,
      weight = weights.rma.mv(model_full)
      # out = if_else(model_full$mf.r[[1]]$std_id %in% influential, 1, 0)
    )

    dat_plot <- dat_plot |>
      bind_rows(
        anti_join(x = dat_out, y = dat_plot, by = join_by(std_id))[, 1:3]
      ) |>
      mutate(out = if_else(std_id %in% influential, 1, 0))
  }

  dat_legend <- data.frame(
    x = c(
      (res$estimate.pi.lb + res$estimate.pi.ub) / 2,
      (res$estimate.ci.lb + res$estimate.ci.ub) / 2
    ),
    y = c(0, 0),
    interval = c("95% Prediction", "95% Confidence"))

  x_breaks <- seq(round(min(dat_plot$yi)), max(dat_plot$yi), by = 0.5)
  file_name <- str_glue("es_scatter_{plot_tag}.png")
  set.seed(1992)

  if (is.null(influential)) {
    p <- ggplot(dat_plot, aes(x = yi, y = 0, size = weight)) +
      # 95% CI and PI boxes
      geom_rect(
        aes(
          xmin = res$estimate.pi.lb, xmax = res$estimate.pi.ub,
          ymin = -0.7, ymax = 0.7),
        fill = "navajowhite2", color = NA) +
      geom_rect(
        aes(
          xmin = res$estimate.ci.lb, xmax = res$estimate.ci.ub,
          ymin = -0.7, ymax = 0.7),
        fill = "orange", color = NA) +
      # offsetting plotted points
      geom_jitter(width = 0, height = 0.6, alpha = 0.2) +
      # Adding lines to mark values of interest
      geom_vline(xintercept = 0, linetype = "dashed", color = "black") +
      geom_vline(xintercept = res$b, linetype = "solid", color = "red") +
      # Adding dummy points for legend
      geom_point(
        data = dat_legend,
        aes(x = x, y = y, fill = interval),
        size = 5,
        color = NA) +
      # # Adding text labels for outliers
      # geom_text(
      #   data = outliers,
      #   aes(label = Study_ID),
      #   size = 5 , vjust = -1, hjust = 0.5
      #   ) +
      # Adding legend title for fill
      labs(
        title = "Effect Sizes by Study",
        x = "Effect Size (g)", y = NULL,
        size = "Weight",
        fill = "Interval",
        color = "Influential Study"
      ) +
      # Custom colors for intervals and plotted points
      scale_fill_manual(
        values = c("95% Prediction" = "navajowhite2", "95% Confidence" = "orange")
      ) +
      # Ensuring circles in legend and transparent rectangles
      guides(
        size = guide_legend(
          override.aes = list(shape = 16, fill = "transparent", alpha = 0.2),
          nrow = 3
        ),
        fill = guide_legend(override.aes = list(shape = 22, size = 5, alpha = 1)),
      ) +
      theme_classic() +
      theme(axis.ticks.y = element_blank(), axis.text.y = element_blank()) +
      ylim(c(-1, 1)) +
      scale_x_continuous(breaks = x_breaks, labels = x_breaks)

    ggsave(
      here("figures", file_name),
      plot = p,
      width = 9,
      height = 3.5
    )
  }

  if (!is.null(influential)) {
    p <- ggplot(
      dat_plot,
      aes(x = yi, y = 0, size = weight, color = factor(out))) +
      # 95% CI and PI boxes
      geom_rect(
        aes(
          xmin = res$estimate.pi.lb, xmax = res$estimate.pi.ub,
          ymin = -0.7, ymax = 0.7),
        fill = "navajowhite2", color = NA) +
      geom_rect(
        aes(
          xmin = res$estimate.ci.lb, xmax = res$estimate.ci.ub,
          ymin = -0.7, ymax = 0.7
        ),
        fill = "orange", color = NA) +
      # offsetting plotted points
      geom_jitter(width = 0, height = 0.6, alpha = 0.4) +
      # Adding lines to mark values of interest
      geom_vline(xintercept = 0, linetype = "dashed", color = "black") +
      geom_vline(xintercept = res$b, linetype = "solid", color = "red") +
      # Adding dummy points for legend
      geom_point(
        data = dat_legend,
        aes(x = x, y = y, fill = interval),
        size = 5, color = NA) +
      # # Adding text labels for outliers
      # geom_text(
      #   data = outliers,
      #   aes(label = Study_ID),
      #   size = 5 , vjust = -1, hjust = 0.5
      #   ) +
      # Adding legend title for fill
      labs(
        title = "Effect Sizes by Study",
        x = "Effect Size (g)",
        y = NULL,
        size = "Weight",
        fill = "Interval",
        color = "Influential Study") +
      # Custom colors for intervals and plotted points
      scale_fill_manual(
        values = c(
          "95% Prediction" = "navajowhite2", "95% Confidence" = "orange")) +
      scale_color_manual(
        values = c("0" = "grey20", "1" = "blue"),
        labels = c("0" = "No", "1" = "Yes")) +
      # Ensuring circles in legend and transparent rectangles
      guides(
        size = guide_legend(
          override.aes = list(shape = 16, fill = "transparent", alpha = 0.2),
          nrow = 3),
        fill = guide_legend(override.aes = list(shape = 22, size = 5, alpha = 1)),
        color = guide_legend(
          override.aes = list(shape = 16, size = 5, alpha = 0.6), nrow = 1)) +
      theme_classic() +
      theme(axis.ticks.y = element_blank(), axis.text.y = element_blank()) +
      ylim(c(-1, 1)) +
      scale_x_continuous(breaks = x_breaks, labels = x_breaks)

    ggsave(
      here("figures", file_name),
      plot = p,
      width = 9,
      height = 3.5
    )
  }
}
