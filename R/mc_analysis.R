# processing of motion capture data
# 1. filter data
# 2. calculate angle velocity
# 3. cut data to relevant revolutions
proc_mc <- function(data, sprint = FALSE, tlim = FALSE, fps = 200) {
  # special behaviour for p03 sprint-1
  # this is not the way this should be written, as it should be put into a 
  # generalised rule. The problem is that some variation around the 0/360
  # transition occurs before the start of the actual first revolution.
  # There should be a function to check for this implemented in find_revs()
  # TODO: write this function
  # by now this is treated as a special case (and this is not how responsible 
  # programming should look like)
  if (abs(data$angle[1] - 0.06252706) < 0.00001) {
    data <- data[425:nrow(data),]
  }
  
  # filter data
  # 4th order low pass Butterworth filter
  # cut-off frequency: 
  # NOTE: for 200 Hz a smaller cut-off frequency might be better suited (e.g., 0.1)
  
  # uses unfiltered data for crank angle determination
  data_filt <- cbind(
    frame = data["frame"],
    as.data.frame(apply(data, 2, spiro::bw_filter, n = 4, W = 0.2, zero_lag = TRUE)[,c(-1, -15)]),
    angle = data["angle"]
  )
  
  # calculate angular velocities
  # based on sampling frequency (frames per second, defaults to 200 Hz)
  data_vel <- cbind(
      frame = data_filt["frame"][-nrow(data_filt),],
      as.data.frame(apply(data_filt, 2, diff)[,-1] * fps)
  )
  
  # find revolution and cut data for positions and velocities
  d <- find_revs(data_filt) # uses position data for revolution identification
  
  # calculate normalized trial duration and quartiles for tlim trials
  if (isTRUE(tlim)) {
    d <- proc2_plustlim_emg(d)
    data_vel <- cbind(data_vel, d[-1,16:18])
  }
  
  if (isTRUE(sprint)) {
    # keep first revolution for sprint and classify revolution to sets
    out <- rbind(
      get_sprint_revs(remove_revs(d, remove_first = 1, type = "pos")),
      get_sprint_revs(remove_revs(cbind(data_vel, rep = d["rep"][-1,]), remove_first = 1, type = "vel"))
    )
  } else {
    out <- rbind(
      remove_revs(d, type = "pos"),
      remove_revs(cbind(data_vel, rep = d["rep"][-1,]), type = "vel")
    )
  }
  out
}

# calculate max, min, RoM (range) for all mc-data angles
mc_params <- function(data, type = "pos") {
  # choose between analysis of pos and vel data
  data <- data[data$posvel == type,]
  # calculate max, min, rom for a single revolution
  mc_params.internal <- function(rep, data) {
    d <- data[data$rep == rep,]
    # find min and max
    onemax <- apply(d[2:14], 2, max, na.rm = TRUE)
    onemin <- apply(d[2:14], 2, min, na.rm = TRUE)
    # calculate range of motion within crank cycle
    onerom <- onemax - onemin
    out <- rbind(
      as.data.frame(t(onemax)), as.data.frame(t(onemin)), as.data.frame(t(onerom))
    )
    out$type <- c("max", "min", "rom")
    out$rep <- rep
    out
  }
  # find all parameter per revolution
  prm <- purrr::list_rbind(lapply(unique(data$rep), mc_params.internal, data = data))
  # calculate overall means
  allmax <- colMeans(prm[prm$type == "max", 1:13])
  allmin <- colMeans(prm[prm$type == "min", 1:13])
  allrom <- colMeans(prm[prm$type == "rom", 1:13])
  out <- rbind(
    as.data.frame(t(allmax)), as.data.frame(t(allmin)), as.data.frame(t(allrom))
  )
  out$type <- c("max", "min", "rom")
  out
}

# plot motion capture data
plot_mc <- function(mc, id = NA, trial = NA, sprint = FALSE, type = "pos") {
  # choose between analysis of pos and vel data
  mc <- mc[mc$posvel == type,]
  p1 <- plot_mc_timecourse(mc, sprint = sprint)
  ggsave(
    paste0("plots/mc_timecourse/", id, "-", trial, ".png"), 
    plot = p1, 
    bg = "white",
    width = 6,
    height = 4.5
  )
}

# plot joint angles over crank angle
plot_mc_timecourse <- function(mc, sprint = FALSE) {
  # transform data to long format
  mc_long <- pivot_longer(mc, 2:14, names_to = "type", values_to = "value")
  # angles of interest
  aoi <- c("s_flex", "s_abad", "s_rotx", "e_flex", "w_flex", "w_udrd", "t_flex")
  aoi_labels <- c(
    expression(SF[theta]), 
    expression(SA[theta]), 
    expression(SR[theta]), 
    expression(EF[theta]), 
    expression(PF[theta]), 
    expression(RD[theta]), 
    expression(TF[theta])
  )
  
  # filter angles of interest
  mc_filt <- mc_long[mc_long$type %in% aoi,]
  mc_filt$type <- factor(mc_filt$type, levels = aoi, labels = aoi_labels)
  
  
  if (isTRUE(sprint)) {
    # stratify lines by revolution set for sprints
    ggplot(mc_filt, aes(angle, value)) +
      geom_smooth(aes(color = rep_class, linetype = rep_class), se = FALSE) +
      scale_x_continuous(name = expression(paste("Crank Angle ", theta, " [°]")), limits = c(0,360), breaks = seq(0,360,120), expand = c(0,0)) +
      scale_y_continuous(name = expression(paste(theta, " [°]"))) +
      scale_color_discrete(name = NULL) +
      scale_linetype_discrete(name = NULL) +
      facet_wrap(~type, nrow = 2, scales = "free_y", labeller = label_parsed) +
      theme_bw() +
      theme(
        # facet panel only label
        strip.background = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA),
        legend.position = c(1, 0),
        legend.justification = c(1, 0)
      )
  } else {
    ggplot(mc_filt, aes(angle, value)) +
      geom_smooth(color = "black") +
      scale_x_continuous(name = expression(paste("Crank Angle ", theta, " [°]")), limits = c(0,360), breaks = seq(0,360,120), expand = c(0,0)) +
      scale_y_continuous(name = expression(paste(theta, " [°]"))) +
      facet_wrap(~type, nrow = 2, scales = "free_y", labeller = label_parsed) +
      theme_bw() +
      theme(
        # facet panel only label
        strip.background = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA)
      )
  }
}

# timecourse plot to compare the three constant load tests
plot_mc_clt <- function(mc_c50, mc_c4, mc_tlim, id, type = "pos") {
  # filter tlim for first quarter
  # to exclude changes in movement patterns due to fatique, only the first
  # quarter of the tlim trial is used in the comparison
  mc_tlim <- mc_tlim[mc_tlim$rep_class == "q1",]
  mc_tlim <- mc_tlim[,-(17:19)]
  mc_combine <- rbind(mc_c4, mc_c50, mc_tlim)
  mc_combine$trial <- c(rep("c4", nrow(mc_c4)), rep("c50", nrow(mc_c50)),rep("tlim", nrow(mc_tlim)))
  # choose between analysis of pos and vel data
  mc_combine <- mc_combine[mc_combine$posvel == type,]
  mc_long <- pivot_longer(mc_combine, 2:14, names_to = "type", values_to = "value")
  # angles of interest
  aoi <- c("s_flex", "s_abad", "s_rotx", "e_flex", "w_flex", "w_udrd", "t_flex")
  aoi_labels <- c(
    expression(SF[theta]), 
    expression(SA[theta]), 
    expression(SR[theta]), 
    expression(EF[theta]), 
    expression(PF[theta]), 
    expression(RD[theta]), 
    expression(TF[theta])
  )
  
  # filter angles of interest
  mc_filt <- mc_long[mc_long$type %in% aoi,]
  mc_filt$type <- factor(mc_filt$type, levels = aoi, labels = aoi_labels)
  mc_filt$trial <- factor(mc_filt$trial, levels = c("c50", "c4", "tlim"), labels = c("low", "medium", "high"))
  
  p1 <- ggplot(mc_filt, aes(angle, value)) +
    geom_smooth(aes(color = trial), se = FALSE) +
    scale_x_continuous(name = expression(paste("Crank Angle ", theta, " [°]")), limits = c(0,360), breaks = seq(0,360,120), expand = c(0,0)) +
    scale_y_continuous(name = expression(paste(theta, " [°]"))) +
    scale_color_manual(name = NULL, values = c("darkgreen", "orange3", "violetred4")) +
    facet_wrap(~type, nrow = 2, scales = "free_y", labeller = label_parsed) +
    theme_bw() +
    theme(
      # facet panel only label
      strip.background = element_blank(),
      panel.border = element_rect(colour = "black", fill = NA),
      legend.position = c(1, 0),
      legend.justification = c(1, 0)
    )
  
  ggsave(
    paste0("plots/mc_timecourse/", id, "-comb.png"),
    plot = p1,
    bg = "white",
    width = 6,
    height = 4.5
  )
  
}
