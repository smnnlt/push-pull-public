proc_pow <- function(data, sprint = FALSE) {
  d <- find_revs(data)
  if (isTRUE(sprint)) {
    # keep first revolution for sprint
    out <- remove_revs(d, remove_first = 1)
  } else {
    # TODO- manual check??
    # this is currently the default (remove pre, first and two last revolutions)
    out <- remove_revs(d, remove_last = 2)
  }
  out
}

# Calculate percentage of regions
pow_get_phase <- function(a) {
  if (a >= 330 | a < 30) {
    "PressDown"
  } else if (a >= 30 & a < 90) {
    "PullDown"
  } else if (a >= 90 & a < 150) {
    "PullUp"
  } else if (a >= 150 & a < 210) {
    "LiftUp"
  } else if (a >= 210 & a < 270) {
    "PushUp"
  } else if (a >= 270 & a < 330) {
    "PushDown"
  } else {
    NA
  }
}

pow_phase_means <- function(powdata) {
  powdata$section <- purrr::map_vec(powdata$angle, pow_get_phase)
  powdata$section <- factor(powdata$section, levels = c("PressDown", "PullDown", "PullUp", "LiftUp", "PushUp", "PushDown"))
  summ <- dplyr::group_by(powdata, section) |>
    dplyr::summarise(mean = mean(torque))
  summ$rel <- summ$mean / sum(summ$mean)
  summ
}

pow_plot_circle <- function(data, id = NULL, trial = NULL, save = TRUE, tlim = FALSE) {
  d <- pow_phase_means(data)
  
  # polar plot with torque and six zones
  p1 <- ggplot(d, aes(section, rel, fill = section)) +
    geom_hline(yintercept = 1/6, colour = "grey50") +
    geom_col(width = 1, color = "black", alpha = 0.5) +
    geom_text(aes(label = round(rel, 2)), y = 0.07) +
    coord_polar(start = pi / 3) +
    scale_x_discrete(name = NULL, labels = seq(0,360,60)) +
    scale_y_continuous(name = NULL, limits = c(-0.1, 0.35), labels = NULL) +
    scale_fill_discrete(name = "Section") +
    theme_bw() +
    theme(
      panel.border = element_blank(),
      axis.ticks = element_blank()
    )
  
  p2 <- ggplot(data, aes(x = angle, y = torque)) +
    geom_smooth(se = FALSE, colour = "black") +
    coord_polar(start = pi / 2) +
    theme_void()
  
  out1 <- p1 + inset_element(p2, left = 0, right = 1, bottom = 0, top = 1)
  
  # add bar chart for push/pull-ratio on the left side
  
  push <- sum(d$rel[4:6]) - 0.5
  p3 <- ggplot() + 
    geom_col(aes(x = 1, y = push), fill = "darkred", alpha = 0.5) +
    geom_col(aes(x = 1, y = -0.5), color = "black", fill = "transparent") +
    geom_col(aes(x = 1, y = 0.5), color = "black", fill = "transparent") +
    geom_text(aes(x = 1, y = push / 2, label = scales::percent(push))) +
    scale_x_continuous(breaks = NULL, name = NULL) +
    scale_y_continuous(breaks = seq(-0.4, 0.4, 0.2), name = NULL, labels = c("\n -40%\n more pull", "-20%", "0%", "20%", "more push\n 40%\n")) +
    theme_minimal()
  
  out2 <- p3 + out1 + plot_layout(widths = c(1,3))
  
  # add additional tlim analysis separated by quarters
  if (tlim) {
    # get quantiles
    dq <- proc2_plustlim_emg(data)
    # get phase means for quantile
    means_q <- function(i, data, only_pp = TRUE) {
      d_cut <- data[data$rep_class == paste0("q", i),]
      m <- pow_phase_means(d_cut)
      m$rep_class = paste0("q", i)
      # use this if only push-pull difference is needed
      if (only_pp) {
        data.frame(
          push = sum(m$rel[4:6]) - 0.5,
          rep_class = paste0("q", i)
        )
      } else {
        # use this if individual phases are needed
        m
      }
    }
    all_q <- purrr::list_rbind(lapply(1:4, means_q, data = dq))
    
    p_tlimq <- ggplot(data = all_q, aes(x = rep_class)) + 
      geom_col(aes(y = push), fill = "darkred", alpha = 0.5) +
      geom_col(aes(y = -0.5), color = "black", fill = "transparent") +
      geom_col(aes(y = 0.5), color = "black", fill = "transparent") +
      geom_text(aes(y = push / 2, label = scales::percent(push, 1))) +
      scale_x_discrete(name = NULL) +
      scale_y_continuous(breaks = seq(-0.4, 0.4, 0.2), name = NULL, labels = c("\n -40%\n more pull", "-20%", "0%", "20%", "more push\n 40%\n")) +
      theme_minimal()
    if (save) {
      ggsave(
        paste0("plots/pow_polar_pp/", id,"-tlim-q.png"),
        plot = p_tlimq,
        width = 5.5, height = 4, dpi = 300, bg = "white"
      )
    }
  }
  
  if (save) {
    ggsave(
      paste0("plots/pow_polar/", id,"-", trial, ".png"),
      plot = out1,
      width = 7, height = 5, dpi = 300
    )
    ggsave(
      paste0("plots/pow_polar_pp/", id,"-", trial, ".png"),
      plot = out2,
      width = 8, height = 5, dpi = 300
    )
    rnorm(1)
  } else {
    c(out1, out2)
  }
}

sprint_power <- function(powerdata) {
  list(
    max = max(powerdata$power, na.rm = TRUE),
    mean = mean(powerdata$power, na.rm = TRUE)
  )
}

mvc_pow_proc <- function(data, id = NULL, trial = NULL) {
  angle <- switch(trial, `1` = "0°", `2` = "90°", `3` = "180°", `4` = "270°", NA)
  # plot mvc pow check
  p <- ggplot(data, aes(seq_along(time), torque)) +
    geom_line() +
    geom_hline(aes(yintercept = max(torque)), colour = "royalblue") +
    labs(x = "Frames", y = "Torque", title = paste0("MVC torque --- id: ", id, " --- trial: ", angle)) + 
    theme_classic()
  ggsave(
    paste0("plots/mvc_pow_check/", id, "-", trial, ".png"), 
    plot = p, 
    bg = "white",
    width = 6,
    height = 4.5
  )
}

mvc_cross_proc <- function(d1, d2, d3, d4, id = NULL) {
  maxs <- c(max(d1$torque, na.rm = TRUE), max(d2$torque, na.rm = TRUE), 
            max(d3$torque, na.rm = TRUE), max(d4$torque, na.rm = TRUE))
  
  d <- data.frame(
    angle = seq(0,270,90),
    value = maxs
  )
  
  p <- ggplot(d) +
    ggforce::geom_circle(aes(x0 = 0, y0 = 0, r = 50), colour = "grey60") +
    ggforce::geom_circle(aes(x0 = 0, y0 = 0, r = 100), colour = "grey60") +
    ggforce::geom_circle(aes(x0 = 0, y0 = 0, r = 25), colour = "grey90") +
    ggforce::geom_circle(aes(x0 = 0, y0 = 0, r = 75), colour = "grey90") +
    ggforce::geom_circle(aes(x0 = 0, y0 = 0, r = 125), colour = "grey90") +
    geom_rect(xmin = 0, xmax = maxs[1], ymin = -7, ymax = 7) +
    geom_rect(xmin = -7, xmax = 7, ymin = 0, ymax = maxs[2]) +
    geom_rect(xmin = 0, xmax = -maxs[3], ymin = -7, ymax = 7) +
    geom_rect(xmin = -7, xmax = 7, ymin = 0, ymax = -maxs[4]) +
    annotate("text", label = 50, colour = "grey60", x = 58, y = 13) +
    annotate("text", label = 100, colour = "grey60", x = 110, y = 13) +
    scale_x_continuous(limits = c(-150, 150)) +
    scale_y_continuous(limit = c(-150, 150)) +
    coord_equal() +
    theme_void()
  ggsave(
    paste0("plots/mvc_pow_cross/", id, ".png"), 
    plot = p, 
    bg = "white",
    width = 6,
    height = 4.5
  )
  
}