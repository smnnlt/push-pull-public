# # script to create plots combining data from various participants
# # this script currently works outside of the targets environment
# 
# # Combine Push-Pull Data to Plot

# # source("R/pow_analysis.R")
# 
# get_push <- function(id) {
#   d <- list(
#     tar_read(gext_pow_proc)[[id]], tar_read(ramp_pow_proc)[[id]],
#     tar_read(s1_pow_proc)[[id]], tar_read(s2_pow_proc)[[id]],
#     tar_read(c50_pow_proc)[[id]], tar_read(c4_pow_proc)[[id]],
#     tar_read(tlim_pow_proc)[[id]]
#   )
# 
#   get_push <- function(data) {
#     phases <- pow_phase_means(data)
#     sum(phases$rel[4:6]) - 0.5
#   }
# 
#   out <- data.frame(
#     trial = c("gext", "ramp", "sprint-1", "sprint-2", "c50", "c4", "tlim"),
#     value = vapply(d, get_push, FUN.VALUE = 1),
#     id = id
#   )
#   out
# }
# 
# all <- purrr::list_rbind(lapply(1:3, get_push))
# 
# all$trial <- factor(all$trial, levels = c("sprint-2", "sprint-1", "tlim", "c50", "c4", "ramp", "gext"))
# all$id <- factor(all$id, levels = 1:3, labels = c("id = 1", "id = 2", "id = 3"))
# 
# ggplot(all, aes(trial, value + 0.5)) +
#   geom_point() +
#   scale_y_continuous(
#     name = NULL,
#     limits = c(0.3,0.7), breaks = seq(0.3,0.7,0.1),
#     labels = c("\n-20%\nmore pull", "-10%","0%", "10%", "20%\nmore push\n")
#   ) +
#   scale_x_discrete(name = NULL, guide = guide_axis(n.dodge = 2)) +
#   facet_wrap(~id, ncol = 3) +
#   theme_bw()
# 
# ggsave("plots/comb/pp.png", width = 6, height = 4.5, dpi = 300, bg = "white")
# 
# 
# ## Combines tlim emg plots
# 
# get_iemg <- function(i) {
#   d <- tar_read(tlim_emg_iemg)[[i]]
#   d$id <- i
#   d
# }
# data <- purrr::list_rbind(lapply(1:3, get_iemg))
# data$id <- as.factor(data$id)
# 
# data_long <- as.data.frame(tidyr::pivot_longer(data, cols = 1:8, names_to = "muscle", values_to = "iemg"))
# data_long$muscle <- factor(data_long$muscle, levels = c("dp","td","fc","bb","da","pm","tb","ra"))
# data_long$muscle <- toupper(data_long$muscle)
# 
# p_tlimc <- ggplot(data_long, aes(ntime, iemg, color = id, group = id)) +
#   geom_smooth() +
#   facet_wrap(~muscle, nrow = 2) +
#   theme_bw() +
#   scale_y_continuous(name = "iEMG (%)") +
#   scale_x_continuous(name = "tlim (%)") +
#   scale_colour_brewer(palette = "Dark2") +
#   theme(
#     panel.spacing = unit(1, "lines"),
#     strip.background = element_blank(),
#     panel.border = element_rect(colour = "black", fill = NA)
#   )
# 
# ggsave(
#   paste0("plots/comb/tlim_emg.png"), 
#   plot = p_tlimc, 
#   bg = "white",
#   width = 6.5,
#   height = 4.5
# )
# 
