conv_data <- function(data, has_resp = TRUE) {
  out <- jsonlite::fromJSON(data)
  if (has_resp) {
    attr(out$resp, "raw") <- out$resp_attr[["raw"]]
    attr(out$resp, "protocol") <- out$resp_attr[["protocol"]]
    attr(out$resp, "info") <- out$resp_attr[["info"]]
    class(out$resp) <- c("spiro", "data.frame")
  }
  out
}

calc4 <- function(data) {
  d <- data[,-1]
  p <- as.numeric(c(0, colnames(d)[-1]))
  la <- as.numeric(d)
  out <- approx(la, p, 4)
  out$y
}

calc4_add <- function(sdata, p4) {
  list(
    hr4 = approx(sdata$load, sdata$HR, p4)$y,
    vo24 = approx(sdata$load, sdata$VO2, p4)$y,
    vo24rel = approx(sdata$load, sdata$VO2_rel, p4)$y
  )
}

# add tooltip to column names of a data.frame
tooltip_names <- function(dataframe, tiplabel, cols = NULL) {
  if (length(tiplabel) != ncol(dataframe)) stop("tiplabel must be the some length than columns of dataframe", call. = FALSE)
  if (is.null(cols)) cols <- colnames(dataframe)
  out <- vector("character", length(tiplabel))
  for (i in seq_along(dataframe)) {
    if (is.na(tiplabel[i])) { # NA to omit tooltip
      out[i] <- cols[i]
    } else {
      if (is.na(cols[i])) cols[i] <- colnames(dataframe)[i]
      out[i] <- paste0('<span style=" text-decoration: underline;text-decoration-style: dotted; " data-toggle="tooltip" data-container="body" data-placement="right" title="" data-bs-original-title="', tiplabel[i], '">',cols[i],'</span>')
    }
  }
  colnames(dataframe) <- out
  dataframe
}

# add number of revolution for angle data
find_revs <- function(data) {
  data$rep <- NA
  count <- 0
  for (i in seq_along(data$angle)) {
    if (i != 1 & !is.na(data$angle[i])) {
      if (data$angle[i-1] > 350 & data$angle[i] < 10) {
        count <- count + 1
      }
    }
    data$rep[i] <- count
  }
  data
}

# remove certain revolutions in a mc/emg data set
remove_revs <- function(data, remove_first = 2, remove_last = 1, type = NULL, remove_more = NULL) {

  if (remove_first != 0) {
    rem_front <- seq(from = 0, by = 1, length.out = remove_first)
    data <- data[!data$rep %in% rem_front, ]
  }
  if (remove_last != 0) {
    rem_last <- seq(
      from = max(data$rep, na.rm = TRUE), 
      by = -1, 
      length.out = remove_last
    )
    data <- data[!data$rep %in% rem_last, ]
  }
  # remove additional revolution based on revolution numbers
  if (!is.null(remove_more)) {
    data <- data[!data$rep %in% remove_more, ]
  }
  # add posvel (data type: position or velocity) column to data frame
  if (!is.null(type)) {
    data$posvel <- type
  }
  data
}

get_sprint_revs <- function(data) {
  data$rep_class <- NA
  data$rep_class[data$rep == 1] <- "R1"
  data$rep_class[data$rep == 2] <- "R2"
  data$rep_class[data$rep >= 3 & data$rep <= 13] <- "R3"
  data$rep_class[data$rep >= 14] <- "R4"
  data
}

# get mc fps based on ratio of emg to mc frequency
# assuming that emg is sampled at 1000 Hz
get_fps <- function(emg, mc, emg_frequency = 1000) {
  rat <- nrow(emg) / nrow(mc)
  emg_frequency / rat
}