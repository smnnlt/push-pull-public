library(targets)
library(tidyr)
library(ggplot2)


plot_mc_clt(mc_c50 = tar_read("c50_mc_proc")[[1]], mc_c4 = tar_read("c4_mc_proc")[[1]], mc_tlim = tar_read("tlim_mc_proc")[[1]], id = 1)
plot_mc_clt(mc_c50 = tar_read("c50_mc_proc")[[2]], mc_c4 = tar_read("c4_mc_proc")[[2]], mc_tlim = tar_read("tlim_mc_proc")[[2]], id = 2)
plot_mc_clt(mc_c50 = tar_read("c50_mc_proc")[[3]], mc_c4 = tar_read("c4_mc_proc")[[3]], mc_tlim = tar_read("tlim_mc_proc")[[3]], id = 3)
