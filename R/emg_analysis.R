# basic processing of emg data
proc1_emg <- function(data) {
  # rectify data
  emg_rect <- abs(data[,-1])
  # smooth data
  emg_smooth <- as.data.frame(apply(emg_rect, 2, spiro:::mavg, k = 200))
  emg_smooth$frame <- data$frame
  emg_smooth
}

# advanced processing of emg data:
# 1 normalize emg data
# 2 get angle and revolution data for emg based on mc angle data
# 3 cut first and last revolution
# 4 cut additional revs based on emg_check data from visual inspection of 
# emg_check_pre plots
proc2_emg <- function(emg, mc, mvc, sprint = FALSE, tlim = FALSE, remove_more = NULL, id = NULL, trial = NULL) {
  emg <- emg_normalize(emg, mvc)
  mult <- nrow(emg) / nrow(mc)
  idx <- seq_len(nrow(mc))
  
  # special behaviour for p03 sprint-1
  # this is not the way this should be written, as it should be put into a 
  # generalised rule. The problem is that some variation around the 0/360
  # transition occurs before the start of the actual first revolution.
  # There should be a function to check for this implemented in find_revs()
  # TODO: write this function
  # by now this is treated as a special case (and this is not how responsible 
  # programming should look like)
  if (abs(mc$angle[1] - 0.06252706) < 0.00001) {
    mc <- mc[425:nrow(mc),]
    emg <- emg[(424*mult+1):nrow(emg),]
    idx <- seq_len(nrow(mc))
  }
  
  # linear interpolation of angle data
  emg_angle <- purrr::list_c(lapply(idx[-length(idx)], lintp_angle, mc = mc, mult = mult))
  emg$angle <- c(emg_angle, rep(NA, mult))
  
  # find revolutions
  emg_rev <- find_revs(emg)
  
  # get additional revolution to remove based on check_emg data
  if (!is.null(remove_more)) {
    rem_data <- remove_more
    rems <- rem_data[rem_data$id == id & rem_data$trial == trial, "remove_rep"]
    rev_more <- as.vector(strsplit(rems, ";")[[1]])
  } else {
    rev_more <- NULL
  }
  
  if (isTRUE(sprint)) {
    # keep first revolution for sprint and classify revolution to sets
    out <- get_sprint_revs(remove_revs(emg_rev, remove_first = 1, remove_more = rev_more))
  } else {
    out <- remove_revs(emg_rev, remove_more = rev_more)
  }
  
  if (isTRUE(tlim)) {
    out <- proc2_plustlim_emg(out)
  }
  out
}

# additional processing for emg in the tlim trials
# create normalized time data and revolution quartiles
proc2_plustlim_emg <- function(emg) {
  totaltime <- nrow(emg)
  emg$ntime <- seq_len(totaltime) / nrow(emg)
  emg$rep_ntime <- NA
  emg$rep_class <- NA
  for (i in unique(emg$rep)) {
    set <- emg$rep == i
    settime <- max(emg$ntime[set])
    emg$rep_ntime[set] <- settime
    emg$rep_class[set] <- if (settime < 0.25) {
      "q1"
    } else if (settime < 0.5) {
      "q2"
    } else if (settime < 0.75) {
      "q3"      
    } else {
      "q4"
    }
  }
  emg
}

# visual inspection of the results of the proc2_emg function
inspect_emg <- function(emg, id = NA, trial = NA) {
  # plot emg data for a single muscle
  inspect_emg.internal <- function(muscle, emg, id = NA, trial = NA) {
    emg$signal <- emg[[muscle]]
    p <- ggplot(emg, aes(angle, signal)) +
      geom_line() +
      facet_wrap(~rep) +
      labs(x = "Angle", y = "EMG signal (normalized)", title = paste0("EMG --- id: ", id, " --- muscle: ", muscle, " --- trial: ", trial)) +
      theme_bw()
    ggsave(
      paste0("plots/emg_check/", id, "-", trial, "-", muscle, ".png"), 
      plot = p, 
      bg = "white",
      width = 8,
      height = 6.5
    )
  }
  purrr::walk(colnames(emg)[1:8], inspect_emg.internal, emg = emg, id = id, trial = trial) 
}

# further analysis of emg data:
# 1 calculate muscle- and revolution-specific on/offset
# 2 get mean and sd of on/offsets
# 
# get
proc3_emg <- function(emg) {
  sets <- emg_onsets(emg)
  # get mean/sd of on and offsets and calculate iEMG
  merge(get_meansd_rot(sets), calc_iemg(emg))
}

# onset offset calculation  ----

# get specific emg on/offset for all muscles
emg_onsets <- function(emg) {
  # get emg on- offset for a single muscle
  emg_onsets.internal <- function(muscle, emg) {
    out <- purrr::list_rbind(lapply(unique(emg$rep), onoff, muscle = muscle, emg = emg))
    out$muscle <- muscle
    out
  }
  
  muscles <- colnames(emg)[1:8]
  purrr::list_rbind(lapply(muscles, emg_onsets.internal, emg = emg))
  
}

# get on/offset for a single revolution for a single muscle
onoff <- function(irep, muscle, emg, cut = 0.3, min = TRUE) {
  if (cut <= 0 | cut >= 1) stop("`cut` must be between 0 and 1")
  if (!is.logical(min)) stop("`min` must be TRUE or FALSE")
  # get specific muscle and revolution
  d <- emg[emg$rep == irep,]
  s <- d[[muscle]]
  emg_max <- max(s, na.rm = TRUE)
  # threshold relative to local min and max of revolution
  if (isTRUE(min)) {
    emg_min <- min(s, na.rm = TRUE)
    thresh <- (emg_max - emg_min) * cut + emg_min
  } else { # threshold only relative to local max
    thresh <- emg_max * cut
  }
  # create empty vectors for on and offset values
  # this is not good practice and computationally expensive,
  # but should be fine as no large vectors are expected
  onvec <- vector("numeric")
  offvec <- vector("numeric")
  
  for (i in seq_along(s)) {
    if (i != length(s)) {
      if (s[i] < thresh & s[i+1] > thresh) {
        onvec <- c(onvec, d$angle[i])
      } else if (s[i] > thresh & s[i+1] < thresh)
        offvec <- c(offvec, d$angle[i])
    }
  }
  
  # treatment if multiple on and offset are detected
  # currently defaults to NA as result
  # TO-DO: change!
  if (length(onvec) != 1) onvec <- NA
  if (length(offvec) != 1) offvec <- NA
  
  data.frame(rep = irep, onset = onvec, offset = offvec)
}

# get parameters for calculated on/offsets of all muscles
get_meansd_rot <- function(data) {
  purrr::list_rbind(lapply(unique(data$muscle), get_meansd_rot.internal, data = data))
}

# get parameters for calculated on/offsets for a single muscle
get_meansd_rot.internal <- function(data, muscle = "da") {
  d <- data[data$muscle == muscle, ]
  # if no onset/offsets are available
  if (all(is.na(d$onset)) | all(is.na(d$offset))) {
    return(data.frame(muscle = muscle, on_mean = NA, off_mean = NA, on_sd = NA, off_sd = NA, middle = NA))
  }
  # calculate mean with special consideration of turning point at 360/0
  # if on time is over 0 at at least 25% of the time
  overgap <- sum(d$onset > d$offset, na.rm = TRUE)
  total <- sum(!is.na(d$onset) | !is.na(d$offset))
  if (overgap > total * 0.25) {
    #d$onset <- ifelse(d$onset <= 180, d$onset + 360, d$onset)
    d$offset <- ifelse(d$offset <= 180, d$offset + 360, d$offset)
  }
  out <- data.frame(
    muscle = muscle,
    on_mean = mean(d$onset, na.rm = TRUE),
    off_mean = mean(d$offset, na.rm = TRUE),
    on_sd = sd(d$onset, na.rm = TRUE),
    off_sd = sd(d$offset, na.rm = TRUE)
  )
  out$middle <- mean(c(out$on_mean, out$off_mean))
  if (out$off_mean > 360) out$off_mean <- out$off_mean - 360
  if (out$middle > 360) out$middle <- out$middle - 360
  out
}

# calculate iEMG ----
# input emg normalized emg data with revolutions
calc_iemg <- function(emg, summarize = TRUE) {
  # iemg is area under the normalized emg curve, so equal to mean emg

  # calculate mean emg for each revolution for all muscles
  calc_iemg.internal <- function(rep, data) {
    d <- data[data$rep == rep, 1:8]
    as.data.frame(t(colMeans(d, na.rm = TRUE)))
  }
  all_iemg <- purrr::list_rbind(lapply(unique(emg$rep), calc_iemg.internal, data = emg))
  
  # summarize data over revolution or output by each revolution
  if (isTRUE(summarize)) {
    # calculate mean and sd per muscle
    meansd_row <- function(row, data) {
      data.frame(
        muscle = colnames(data)[row],
        iemg_mean = mean(data[,row]),
        iemg_sd = sd(data[,row])
      )
    }
    purrr::list_rbind(lapply(1:8, meansd_row, data = all_iemg))
  } else {
   # output single iEMG for all revolutions
    all_iemg$rep <- unique(emg$rep)
    all_iemg$ntime <- unique(emg$rep_ntime)
    all_iemg
  }
}


# linear interpolation -----
# linear interpolation of angle data
lintp_angle <- function(i, mc, mult) {
  # get two angles to interpolate between
  bound <- c(mc$angle[i], mc$angle[i+1])
  # skip if NAs are present
  if (any(is.na(bound))) {
    res <- rep(NA, mult)
  } else {
    # check if a transition around 0 degree occurs
    is_gap <- bound[1] > 350 & bound[2] < 10
    if (is_gap) {
      bound[2] <- 360 + bound[2]
    }
    # perform the linear interpolation
    res <- approx(
      x = c(0,1), 
      y = bound, 
      xout = seq(from = 0, by = 1/mult, length.out = mult)
    )$y
    # reconvert to original scale if transition present
    if (is_gap) {
      res[res > 360] <- res[res > 360] - 360
    }
  }
  res
}

# mvc analysis and normalization ----
# find mvc for all muscles
find_mvcs <- function(emg1, emg2, emg3, emg4, emg5, emg6, emg7, emg8, id = 1) {
  emg1$type <- 1
  emg2$type <- 2
  emg3$type <- 3
  emg4$type <- 4
  emg5$type <- 5
  emg6$type <- 6
  emg7$type <- 7
  emg8$type <- 8
  # bind all mvc measures
  emg <- rbind(emg1, emg2, emg3, emg4, emg5, emg6, emg7, emg8)
  
  # remove specific mvc trials (or parts of them) from mvc determination
  # this is based on manual detection and not automated
  emg <- remove_mvc(emg, id = id)
  
  purrr::list_rbind(lapply(1:8, find_mvc, emg = emg, id = id))
}

# find mvc for a single muscle
find_mvc <- function(n, emg, create_plot = TRUE, id = NA) {
  # find emg max
  out <- data.frame(
    muscle = colnames(emg)[n],
    max = max(emg[,n], na.rm = TRUE),
    where = emg$type[which.max(emg[,n])] # find max trial (crank angle)
  )
  # optional: create plot of max emg signal for visual validation
  if (create_plot) {
    # show only max trial
    emg_filt <- emg[emg$type == out$where,]
    emg_filt$sig <- emg_filt[[out$muscle]]
    # get crank angle of trial
    angle <- switch(out$where, `1` = "0°", `2` = "90°", `3` = "180°", `4` = "270°", `5` = "EF", `6` = "HG", `7` = "SE", `8` = "TF")
    p <- ggplot(emg_filt, aes(seq_along(sig), sig)) +
      geom_line() +
      geom_hline(yintercept = out$max, colour = "royalblue") +
      labs(x = "Frames", y = "EMG Signal", title = paste0("MVC --- id: ", id, " --- muscle: ", out$muscle, " --- trial: ", angle)) +
      theme_classic()
    ggsave(
      paste0("plots/mvc_check/", id, "-", out$muscle, ".png"), 
      plot = p, 
      bg = "white",
      width = 6,
      height = 4.5
    )
  }
  out
}

# remove specific mvc trials (or parts of them) from mvc determination
# this is based on manual detection and not automated
remove_mvc <- function(emg, id) {
  if (id == 1) {
    emg$pm[emg$type == 3][26000:32000] <- NA
    emg$ra[emg$type == 8][1000:6000] <- NA
    emg$tb[emg$type == 3][26000:30000] <- NA
    # almost completely not useable
    emg$td[emg$type == 3][10000:20000] <- NA
    emg$td[emg$type == 3][40000:50000] <- NA
    emg$td[emg$type == 4][5000:50000] <- NA
    emg$td[emg$type == 2][10000:50000] <- NA
  } else if (id == 2) {
    emg$dp[emg$type == 7][500:4000] <- NA
    emg$ra[emg$type == 8][25000:29000] <- NA
  } else if (id == 3) {
    # no correction needed
  }
  emg
}



# normalize emg data to given mvc data for all muscles
emg_normalize <- function(emg, mvc) {
  out <- purrr::list_cbind(
    lapply(mvc$muscle, emg_normalize.internal, emg = emg, mvc = mvc)
  )
  out$frame <- emg$frame
  out
}

# normalize emg data to given mvc data for a single muscle
emg_normalize.internal <- function(muscle, emg, mvc) {
  maxval <- mvc$max[mvc$muscle == muscle]
  out <- data.frame(
    col = emg[,muscle] / maxval
  )
  colnames(out) <- muscle
  out
}

# create emg plots ----

plot_emg <- function(emg, emgonoff, id = NA, trial = NA, sprint = FALSE, tlim = FALSE, iemg = NULL) {
  p1 <- plot_emg_timecourse(emg = emg, sprint = sprint)
  p2 <- plot_emg_onoff(data = emgonoff)
  ggsave(
    paste0("plots/emg_timecourse/", id, "-", trial, ".png"), 
    plot = p1, 
    bg = "white",
    width = 6,
    height = 4.5
  )
  ggsave(
    paste0("plots/emg_polar/", id, "-", trial, ".png"), 
    plot = p2, 
    bg = "white",
    width = 6,
    height = 4.5
  )
  # plot additional iemg timecourses for tlim trials
  if (isTRUE(tlim)) {
    p3 <- plot_emg_tlim(data = iemg)
    ggsave(
      paste0("plots/emg_tlim/", id, ".png"), 
      plot = p3, 
      bg = "white",
      width = 6,
      height = 4.5
    )
  }
}

## plot emg timecourse ----
# emg signal over crank angle faceted by muscle
plot_emg_timecourse <- function(emg, sprint = FALSE) {
  emg_long <- pivot_longer(emg, 1:8, names_to = "muscle", values_to = "emg")
  emg_long$muscle <- toupper(emg_long$muscle)
  
  if (isTRUE(sprint)) {
    # stratify lines by revolution set for sprints 
    ggplot(emg_long, aes(angle, emg)) +
      geom_smooth(aes(color = rep_class, linetype = rep_class), se = FALSE) +
      scale_x_continuous(name = expression(paste("Crank Angle ", theta, " [°]")), limits = c(0,360), breaks = seq(0,360,120), expand = c(0,0)) +
      scale_y_continuous(name = "EMG (%MVIC)", limits = c(0,1.2), breaks = seq(0,1.2,0.2), expand = c(0,0), labels = \(x) x * 100) +
      scale_color_discrete(name = NULL) +
      scale_linetype_discrete(name = NULL) +
      facet_wrap(~muscle, nrow = 2) +
      theme_bw() +
      theme(
        panel.spacing = unit(1, "lines"),
        strip.background = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA)
      )
  } else {
    ggplot(emg_long, aes(angle, emg)) +
      geom_smooth(color = "black") +
      scale_x_continuous(name = expression(paste("Crank Angle ", theta, " [°]")), limits = c(0,360), breaks = seq(0,360,120), expand = c(0,0)) +
      scale_y_continuous(name = "EMG (%MVIC)", limits = c(0,1), breaks = seq(0,1,0.2), expand = c(0,0), labels = \(x) x * 100) +
      facet_wrap(~muscle, nrow = 2) +
      theme_bw() +
      theme(
        panel.spacing = unit(1, "lines"),
        strip.background = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA)
      )
  }
}

## create on/offset plot ----
# function to create plot
plot_emg_onoff <- function(data) {
  d <- prepare_emgplot(data)
  emg_polar_plot(d)
}

# prepare data for plot
prepare_emgplot <- function(data) {
  out <- purrr::list_rbind(lapply(1:8, prepare_emgplot.internal, data = data))
  # use the following line to order muscles in on/offset polar plot by
  # the mean on/offset
  #out$muscle <- factor(out$muscle, levels = data$muscle[order(data$middle)])
  # use this instead to order by default order as in the Quittmann (2020) 
  # publication
  out$muscle <- factor(out$muscle, levels = c("dp","td","fc","bb","da","pm","tb","ra"))
  out
}

# to get lines in the polar plot over the 360/0 border, two separate data.frames
# for mapping are needed
prepare_emgplot.internal <- function(i, data) {
  d <- data[i,]
  # no calculation if no on/offset data available
  # but return data frame with NA
  if (any(is.na(d[,2:5]))) {
    out <- data.frame(
      muscle = d$muscle,
      on = NA,
      off = NA,
      type = 1,
      sd_start = NA,
      sd_end = NA
    )
    return(out)
  }
  # calculate sd start and end values for whiskers
  sd_start <- ifelse(d$on_mean - d$on_sd >= 0, d$on_mean - d$on_sd, 360 - (d$on_mean - d$on_sd))
  sd_end <- ifelse(d$off_mean + d$off_sd <= 360, d$off_mean + d$off_sd, d$off_mean + d$off_sd - 360)
  # area of activation not over 0 (everything fine)
  if (d$on_mean < d$off_mean) {
    # sd is not over 0 (everything finde)
    if (sd_start < sd_end) {
      out <- data.frame(
        muscle = d$muscle,
        on = d$on_mean,
        off = d$off_mean,
        type = 1,
        sd_start = sd_start,
        sd_end = sd_end
      )
    } else {
      # sd whisker go over 0 (two mappings)
      out <- data.frame(
        muscle = d$muscle,
        on = c(d$on_mean, NA),
        off = c(d$off_mean, NA),
        type = c(1,2),
        sd_start = c(0, sd_start),
        sd_end = c(sd_end, 360)
      )
    }

  } else {
    # area of activation over 0 (two mappings)
    out <- data.frame(
      muscle = d$muscle,
      on = c(0, d$on_mean),
      off = c(d$off_mean, 360),
      type = c(1,2),
      sd_start = c(0, sd_start),
      sd_end = c(sd_end, 360)
    )
  }
  out
}

emg_polar_plot <- function(data) {
  emg_colors <- c("#800080","#e600e6","#0000ff","#00cc00","#ffcc00","#ff8000","#ff3300","#b30000")
  ggplot(data = data[data$type == 1,], aes(y = muscle, color = muscle)) +
    # whisker
    geom_linerange(aes(xmin = sd_start, xmax = sd_end), linewidth = 1) +
    geom_linerange(aes(xmin = sd_start, xmax = sd_end), data = data[data$type == 2,], linewidth = 1) +
    # bars
    geom_linerange(aes(xmin = on, xmax = off), linewidth = 4) +
    geom_linerange(aes(xmin = on, xmax = off), data = data[data$type == 2,], linewidth = 4) +
    # labels
    geom_text(aes(label = toupper(muscle)), x = 270, color = "black") +
    coord_polar(start = 0.5*pi) +
    scale_x_continuous(limits = c(0,360), breaks = seq(0,330, by = 30)) +
    # would be better to get more empty space in the middle without getting the
    # same space on the outer side. expand does not work as expected
    # This is probably a bug of coord_polar() + expand + discrete scale
    scale_y_discrete(name = NULL, labels = NULL, expand = c(0, 1), breaks = 0) +
    scale_color_manual(values = emg_colors, guide = "none") +
    theme_minimal() +
    theme(
      #panel.grid.major.y = element_blank(),
      panel.grid.minor.x = element_blank()
    )
}

plot_emg_tlim <- function(data) {
  if (is.null(data)) stop("iEMG plot needs iEMG source data")
  data_long <- as.data.frame(tidyr::pivot_longer(data, cols = 1:8, names_to = "muscle", values_to = "iemg"))
  data_long$muscle <- factor(data_long$muscle, levels = c("dp","td","fc","bb","da","pm","tb","ra"))
  data_long$muscle <- toupper(data_long$muscle)
  
  ggplot(data_long, aes(ntime, iemg)) +
    geom_point(alpha = 0.2) +
    geom_smooth() +
    facet_wrap(~muscle, nrow = 2) +
    theme_bw() +
    scale_y_continuous(name = "iEMG (%)") +
    scale_x_continuous(name = "tlim (%)") +
    theme(
      panel.spacing = unit(1, "lines"),
      strip.background = element_blank(),
      panel.border = element_rect(colour = "black", fill = NA)
    )
}
