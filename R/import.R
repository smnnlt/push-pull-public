import_excel <- function(path) {
  # read data
  d <- readxl::read_excel(path)
  # search for non-NA data values
  nona <- !is.na(d[,-1])
  if (any(nona)) {
    # get maximum row of non-NA and cut data accordingly
    ni <- which(nona, arr.ind = TRUE)
    d[1:max(ni[,1]),]
  } else{
    # use only first row if all data is NA
    d[1,]
  }
}

import_powerdata <- function(path, id = 1) {
  # read data
  d <- read.csv2(path, skip = 3)
  colnames(d)[1:8] <- c("time", "torque", "ang_vel", "angle", "power", "cadence", "hr", "temp")
  d$time <- sub(",", "", d$time)
  suppressWarnings(out <- as.data.frame(apply(d, 2, as.numeric)))
  # angle data for power meter from p2 and p3 had a wrong offset
  # correct angle data based on id
  if (id == 2) {
    out$angle <- ifelse(out$angle >= 168, out$angle -168, 360 - (168 - out$angle))
  } else if (id == 3) {
    out$angle <- ifelse(out$angle >= 84, out$angle -84, 360 - (84 - out$angle))
  }
  out
}

import_emg <- function(path) {
  d <- read.csv(path)
  # there are currently two different name sets for the EMG channels
  if (any(grepl("Voltage",colnames(d)))) {
    cols <- paste0("Voltage.EMG",1:8)
  } else {
    cols <- paste0("Sensor.",1:8,".EMG",1:8)
  }
  d_cut <- d[, colnames(d) %in% c("frame", cols)]
  colnames(d_cut) <- c("frame", "da", "dp", "bb", "tb", "fc", "pm", "td", "ra")
  d_cut
}

import_motiondata <- function(path, remove_missing = TRUE) {
  d <- read.csv(path)
  
  # for p02 in some trials a different marker set was used resulting in 
  # different marker names (starting with 'P02:'). We rename these markers.
  if (any(grepl("P02",colnames(d)))) {
    colnames(d) <- gsub("P02([^.]*)\\.", "", colnames(d)) 
    colnames(d) <- gsub("AX_MID", "LP_MID", colnames(d))  
  }
  
  cols <- c(
    "RShoAnglesYZY.x", "RShoAnglesYZY.y", "RShoAnglesYZY.z", "RShoAnglesXZY.x", 
    "RShoAnglesXZY.y", "RShoAnglesXZY.z", "RElbAnglesXZY.x", "RElbAnglesXZY.y", 
    "RElbAnglesXZY.z", "RWristAnglesXZY.x", "RWristAnglesXZY.y", 
    "RWristAnglesXZY.z")
  d_cut <- d[, colnames(d) %in% c("frame", cols)]
  colnames(d_cut) <- c("frame", "s_plant", "s_ant", "s_roty","s_flex", "s_abad", "s_rotx", "e_flex", "e_prsu", "e_un", "w_flex", "w_udrd", "w_un")
  
  # RShoAnglesYZY:
  # PlaneAnt # Ant # Rot_Y
  # RShoAnglesXZY:
  # !FlEx # !AbAd # !Rot_X
  # RElbAnglesXZY:
  # !FlEx # ProSup # UN
  # RWristAnglesXZY:
  # !FlEx # ! UdRd # UN
  
  # Calculate trunk flexion angle (TF)
  # defined as angle between horizontal plane and line between midpoints of
  # C7/CLAV and T10/STRN
  c7clav_x <- (d$C7.x + d$CLAV.x) / 2
  c7clav_y <- (d$C7.y + d$CLAV.y) / 2
  c7clav_z <- (d$C7.z + d$CLAV.z) / 2
  t10strn_x <- (d$T10.x + d$STRN.x) / 2
  t10strn_y <- (d$T10.x + d$STRN.y) / 2
  t10strn_z <- (d$T10.x + d$STRN.z) / 2
  
  # unclear, this is probably not the right angle!!!
  trunk_x <- c7clav_x - t10strn_x
  trunk_z <- t10strn_z - c7clav_z
  d_cut$t_flex <- atan2(trunk_z, trunk_x) * -180 / pi
  
  # Calculate Crank angle
  # AX_R Axis of rotation (right)
  # LP_Mid crank
  # due to missing data 
  if (remove_missing) {
    d$LP_MID.x[d$LP_MID.x == 0] <- NA
    d$LP_MID.z[d$LP_MID.z == 0] <- NA
    d$AX_R.x[d$AX_R.x == 0] <- NA
    d$AX_R.z[d$AX_R.z == 0] <- NA
  }
  dx <- d$LP_MID.x - d$AX_R.x
  dz <- d$AX_R.z  - d$LP_MID.z
  # calculate angle in degree: crank to the front 0 degree, to the bottom 90 degree
  angle_degree <- atan2(dz, dx) * 180 / pi
  d_cut$angle <- ifelse(angle_degree < 0, angle_degree + 360, angle_degree)
  
  d_cut
}
