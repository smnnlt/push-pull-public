# Load packages required to define the pipeline:
library(targets)
library(tarchetypes)

# Set target options:
tar_option_set(
  # packages that your targets need to run
  packages = c("reticulate", "readxl", "spiro", "jsonlite", "purrr", "ggplot2", "tidyr", "patchwork", "dplyr"), 
  format = "rds"
)

# tar_make_clustermq() configuration
options(clustermq.scheduler = "multiprocess")

# suppress warning based on python script in R folder
suppressWarnings(tar_source())

# Create reports
report_targets <- list(
  tar_quarto_rep(
    report,
    "report_template.qmd",
    execute_params = data.frame(
      id = id,
      output_file = paste0("reports/p", id, ".html")
    ),
    quiet = FALSE
  ),
  tar_quarto_rep(
    report_short,
    "report_template_short.qmd",
    execute_params = data.frame(
      id = id,
      output_file = paste0("reports_short/p", id, ".html")
    ),
    quiet = FALSE
  )
)

import_targets <- list(
  
  # gext
  tar_file(gext_rpe_file, "data/gext/rpe.xlsx"),
  tar_target(gext_rpe, import_excel(gext_rpe_file)),
  tar_file(gext_la_file, "data/gext/la.xlsx"),
  tar_target(gext_la, import_excel(gext_la_file)),
  tar_file(gext_offset_file, "data/gext/hr_offset.xlsx"),
  tar_target(gext_offset, import_excel(gext_offset_file)),
  tar_files(gext_pow_file, list.files("data/gext/pow/", full.names = TRUE)),
  tar_target(gext_pow, import_powerdata(gext_pow_file, id = id), pattern = map(gext_pow_file, id), iteration = "list"),
  tar_target(gext_pow_proc, proc_pow(gext_pow), pattern = map(gext_pow), iteration = "list"),
  tar_target(gext_pow_plot, pow_plot_circle(gext_pow_proc, id, trial = "gext"), pattern = map(gext_pow_proc, id), iteration = "list"),
  tar_files(gext_hr_file, list.files("data/gext/hr", full.names = TRUE)),
  tar_files(gext_resp_file, list.files("data/gext/resp", full.names = TRUE)),
  tar_target(
    gext_resp, 
    spiro(
      gext_resp_file, 
      hr_file = gext_hr_file, hr_offset = gext_offset$gext_offset,
      protocol = set_protocol(pt_pre(60), pt_steps(300, 20, 20, 15))
    ), 
    pattern = map(gext_resp_file, gext_hr_file, gext_offset), iteration = "list"
  ),
  tar_target(
    gext_params,
    as.character(jsonlite::toJSON(
      list(resp = gext_resp, resp_attr = attributes(gext_resp), la = gext_la, rpe = gext_rpe),
      na = "string"
    )),
    pattern = map(gext_resp, gext_la, gext_rpe), iteration = "list"
  ),
  
  # ramp
  tar_file(ramp_rpe_file, "data/ramp/rpe.xlsx"),
  tar_target(ramp_rpe, import_excel(ramp_rpe_file)),
  tar_file(ramp_la_file, "data/ramp/la.xlsx"),
  tar_target(ramp_la, import_excel(ramp_la_file)),
  tar_file(ramp_offset_file, "data/ramp/hr_offset.xlsx"),
  tar_target(ramp_offset, import_excel(ramp_offset_file)),
  tar_files(ramp_pow_file, list.files("data/ramp/pow/", full.names = TRUE)),
  tar_target(ramp_pow, import_powerdata(ramp_pow_file, id = id), pattern = map(ramp_pow_file, id), iteration = "list"),
  tar_target(ramp_pow_proc, proc_pow(ramp_pow), pattern = map(ramp_pow), iteration = "list"),
  tar_target(ramp_pow_plot, pow_plot_circle(ramp_pow_proc, id, trial = "ramp"), pattern = map(ramp_pow_proc, id), iteration = "list"),
  tar_files(ramp_hr_file, list.files("data/ramp/hr", full.names = TRUE)),
  tar_files(ramp_resp_file, list.files("data/ramp/resp", full.names = TRUE)),
  tar_target(
    ramp_resp, 
    spiro(
      ramp_resp_file, 
      hr_file = ramp_hr_file, hr_offset = ramp_offset$ramp_offset,
      protocol = set_protocol(pt_pre(60), pt_wu(120, 80), pt_steps(15, 80, 5, 40))
    ), 
    pattern = map(ramp_resp_file, ramp_hr_file, ramp_offset), iteration = "list"
  ),
  tar_target(
    ramp_params,
    as.character(jsonlite::toJSON(
      list(resp = ramp_resp, resp_attr = attributes(ramp_resp), la = ramp_la, rpe = ramp_rpe, pow = ramp_pow),
      na = "string"
    )),
    pattern = map(ramp_resp, ramp_la, ramp_rpe, ramp_pow), iteration = "list"
  ),
  
  # sprint-1
  tar_file(s1_la_file, "data/sprint-1/la.xlsx"),
  tar_target(s1_la, import_excel(s1_la_file)),
  tar_files(s1_emg_files, list.files("data/sprint-1/emg", full.names = TRUE)),
  tar_target(s1_emg_raw, import_emg(s1_emg_files), pattern = map(s1_emg_files)),
  tar_target(s1_emg, proc1_emg(s1_emg_raw), pattern = map(s1_emg_raw), iteration = "list"),
  tar_target(s1_emg_proc, proc2_emg(s1_emg, s1_mc, mvc, sprint = TRUE, remove_more = emg_check, id = id, trial = "s1"), pattern = map(s1_emg, s1_mc, mvc, id), iteration = "list"),
  tar_target(s1_emg_check, inspect_emg(s1_emg_proc, id, "s1"), pattern = map(s1_emg_proc, id), iteration = "list"), # add id later
  tar_target(s1_emg_params, proc3_emg(s1_emg_proc), pattern = map(s1_emg_proc), iteration = "list"),
  tar_target(s1_emg_plot, plot_emg(s1_emg_proc, s1_emg_params, id, "s1", sprint = TRUE), pattern = map(s1_emg_proc, s1_emg_params, id), iteration = "list"), # add id later  
  tar_files(s1_mc_files, list.files("data/sprint-1/mc", full.names = TRUE)),
  tar_target(s1_mc, import_motiondata(s1_mc_files), pattern = map(s1_mc_files), iteration = "list"),
  tar_target(s1_mc_proc, proc_mc(s1_mc, sprint = TRUE, fps = fps), pattern = map(s1_mc, fps), iteration = "list"),
  tar_target(s1_mc_params, mc_params(s1_mc_proc), pattern = map(s1_mc_proc), iteration = "list"),
  tar_target(s1_mc_plot, plot_mc(s1_mc_proc, id, "s1", sprint = TRUE), pattern = map(s1_mc_proc, id), iteration = "list"), # add id later  
  tar_files(s1_pow_file, list.files("data/sprint-1/pow/", full.names = TRUE)),
  tar_target(s1_pow, import_powerdata(s1_pow_file, id = id), pattern = map(s1_pow_file, id), iteration = "list"),
  tar_target(s1_pow_proc, proc_pow(s1_pow), pattern = map(s1_pow), iteration = "list"),
  tar_target(s1_pow_plot, pow_plot_circle(s1_pow_proc, id, trial = "s1"), pattern = map(s1_pow_proc, id), iteration = "list"),
  tar_target(
    s1_params,
    as.character(jsonlite::toJSON(
      list(la = s1_la, pow = s1_pow),
      na = "string"
    )),
    pattern = map(s1_la, s1_pow)
  ),
  tar_target(s1_pow_results, as.character(jsonlite::toJSON(
    sprint_power(s1_pow),
    na = "string"
  )), pattern = map(s1_pow)),
  
  # sprint-2
  tar_files(s2_emg_files, list.files("data/sprint-2/emg", full.names = TRUE)),
  tar_target(s2_emg_raw, import_emg(s2_emg_files), pattern = map(s2_emg_files), iteration = "list"),
  tar_target(s2_emg, proc1_emg(s2_emg_raw), pattern = map(s2_emg_raw), iteration = "list"),
  tar_target(s2_emg_proc, proc2_emg(s2_emg, s2_mc, mvc, sprint = TRUE, remove_more = emg_check, id = id, trial = "s2"), pattern = map(s2_emg, s2_mc, mvc, id), iteration = "list"),
  tar_target(s2_emg_check, inspect_emg(s2_emg_proc, id, "s2"), pattern = map(s2_emg_proc, id), iteration = "list"), # add id later
  tar_target(s2_emg_params, proc3_emg(s2_emg_proc), pattern = map(s2_emg_proc), iteration = "list"),
  tar_target(s2_emg_plot, plot_emg(s2_emg_proc, s2_emg_params, id, "s2", sprint = TRUE), pattern = map(s2_emg_proc, s2_emg_params, id), iteration = "list"), # add id later  
  tar_files(s2_mc_files, list.files("data/sprint-2/mc", full.names = TRUE)),
  tar_target(s2_mc, import_motiondata(s2_mc_files), pattern = map(s2_mc_files), iteration = "list"),
  tar_target(s2_mc_proc, proc_mc(s2_mc, sprint = TRUE, fps = fps), pattern = map(s2_mc, fps), iteration = "list"),
  tar_target(s2_mc_params, mc_params(s2_mc_proc), pattern = map(s2_mc_proc), iteration = "list"),
  tar_target(s2_mc_plot, plot_mc(s2_mc_proc, id, "s2", sprint = TRUE), pattern = map(s2_mc_proc, id), iteration = "list"), # add id later  
  tar_files(s2_pow_file, list.files("data/sprint-2/pow/", full.names = TRUE)),
  tar_target(s2_pow, import_powerdata(s2_pow_file, id = id), pattern = map(s2_pow_file, id), iteration = "list"),
  tar_target(s2_pow_proc, proc_pow(s2_pow), pattern = map(s2_pow), iteration = "list"),
  tar_target(s2_pow_plot, pow_plot_circle(s2_pow_proc, id, trial = "s2"), pattern = map(s2_pow_proc, id), iteration = "list"),
  tar_target(s2_pow_results, as.character(jsonlite::toJSON(
    sprint_power(s2_pow),
    na = "string"
  )), pattern = map(s2_pow)),
  
  # c50
  tar_files(c50_emg_files, list.files("data/c50/emg", full.names = TRUE)),
  tar_target(c50_emg_raw, import_emg(c50_emg_files), pattern = map(c50_emg_files), iteration = "list"),
  tar_target(c50_emg, proc1_emg(c50_emg_raw), pattern = map(c50_emg_raw), iteration = "list"),
  tar_target(c50_emg_proc, proc2_emg(c50_emg, c50_mc, mvc, remove_more = emg_check, id = id, trial = "c50"), pattern = map(c50_emg, c50_mc, mvc, id), iteration = "list"),
  tar_target(c50_emg_check, inspect_emg(c50_emg_proc, id, "c50"), pattern = map(c50_emg_proc, id), iteration = "list"), # add id later
  tar_target(c50_emg_params, proc3_emg(c50_emg_proc), pattern = map(c50_emg_proc), iteration = "list"),
  tar_target(c50_emg_plot, plot_emg(c50_emg_proc, c50_emg_params, id, "c50"), pattern = map(c50_emg_proc, c50_emg_params, id), iteration = "list"), # add id later  
  tar_files(c50_mc_files, list.files("data/c50/mc", full.names = TRUE)),
  tar_target(c50_mc, import_motiondata(c50_mc_files), pattern = map(c50_mc_files), iteration = "list"),
  tar_target(c50_mc_proc, proc_mc(c50_mc, fps = fps), pattern = map(c50_mc, fps), iteration = "list"),
  tar_target(c50_mc_params, mc_params(c50_mc_proc), pattern = map(c50_mc_proc), iteration = "list"),
  tar_target(c50_mc_plot, plot_mc(c50_mc_proc, id, "c50"), pattern = map(c50_mc_proc, id), iteration = "list"), # add id later  
  tar_files(c50_pow_file, list.files("data/c50/pow/", full.names = TRUE)),
  tar_target(c50_pow, import_powerdata(c50_pow_file, id = id), pattern = map(c50_pow_file, id), iteration = "list"),
  tar_target(c50_pow_proc, proc_pow(c50_pow), pattern = map(c50_pow), iteration = "list"),
  tar_target(c50_pow_plot, pow_plot_circle(c50_pow_proc, id, trial = "c50"), pattern = map(c50_pow_proc, id), iteration = "list"),
  
  # c4
  tar_files(c4_emg_files, list.files("data/c4/emg", full.names = TRUE)),
  tar_target(c4_emg_raw, import_emg(c4_emg_files), pattern = map(c4_emg_files), iteration = "list"),
  tar_target(c4_emg, proc1_emg(c4_emg_raw), pattern = map(c4_emg_raw), iteration = "list"),
  tar_target(c4_emg_proc, proc2_emg(c4_emg, c4_mc, mvc, remove_more = emg_check, id = id, trial = "c4"), pattern = map(c4_emg, c4_mc, mvc, id), iteration = "list"),
  tar_target(c4_emg_check, inspect_emg(c4_emg_proc, id, "c4"), pattern = map(c4_emg_proc, id), iteration = "list"), # add id later
  tar_target(c4_emg_params, proc3_emg(c4_emg_proc), pattern = map(c4_emg_proc), iteration = "list"),
  tar_target(c4_emg_plot, plot_emg(c4_emg_proc, c4_emg_params, id, "c4"), pattern = map(c4_emg_proc, c4_emg_params, id), iteration = "list"), # add id later      
  tar_files(c4_mc_files, list.files("data/c4/mc", full.names = TRUE)),
  tar_target(c4_mc, import_motiondata(c4_mc_files), pattern = map(c4_mc_files), iteration = "list"),
  tar_target(c4_mc_proc, proc_mc(c4_mc, fps = fps), pattern = map(c4_mc, fps), iteration = "list"),
  tar_target(c4_mc_params, mc_params(c4_mc_proc), pattern = map(c4_mc_proc), iteration = "list"),
  tar_target(c4_mc_plot, plot_mc(c4_mc_proc, id, "c4"), pattern = map(c4_mc_proc, id), iteration = "list"), # add id later  
  tar_files(c4_pow_file, list.files("data/c4/pow/", full.names = TRUE)),
  tar_target(c4_pow, import_powerdata(c4_pow_file, id = id), pattern = map(c4_pow_file, id), iteration = "list"),
  tar_target(c4_pow_proc, proc_pow(c4_pow), pattern = map(c4_pow), iteration = "list"),
  tar_target(c4_pow_plot, pow_plot_circle(c4_pow_proc, id, trial = "c4"), pattern = map(c4_pow_proc, id), iteration = "list"),
  
  # tlim
  tar_file(tlim_rpe_file, "data/tlim/rpe.xlsx"),
  tar_target(tlim_rpe, import_excel(tlim_rpe_file)),
  tar_file(tlim_la_file, "data/tlim/la.xlsx"),
  tar_target(tlim_la, import_excel(tlim_la_file)),
  tar_files(tlim_emg_files, list.files("data/tlim/emg", full.names = TRUE)),
  tar_target(tlim_emg_raw, import_emg(tlim_emg_files), pattern = map(tlim_emg_files), iteration = "list"),
  tar_target(tlim_emg, proc1_emg(tlim_emg_raw), pattern = map(tlim_emg_raw), iteration = "list"),
  tar_target(tlim_emg_proc, proc2_emg(tlim_emg, tlim_mc, mvc, tlim = TRUE, remove_more = emg_check, id = id, trial = "tlim"), pattern = map(tlim_emg, tlim_mc, mvc, id), iteration = "list"),
  tar_target(tlim_emg_check, inspect_emg(tlim_emg_proc, id, "tlim"), pattern = map(tlim_emg_proc, id), iteration = "list"), # add id later
  tar_target(tlim_emg_params, proc3_emg(tlim_emg_proc), pattern = map(tlim_emg_proc), iteration = "list"),
  tar_target(tlim_emg_iemg, calc_iemg(tlim_emg_proc, summarize = FALSE), pattern = map(tlim_emg_proc), iteration = "list"),
  tar_target(tlim_emg_plot, plot_emg(tlim_emg_proc, tlim_emg_params, id, "tlim", sprint = TRUE, tlim = TRUE, iemg = tlim_emg_iemg), pattern = map(tlim_emg_proc, tlim_emg_params, id, tlim_emg_iemg), iteration = "list"), # add id later     
  tar_files(tlim_mc_files, list.files("data/tlim/mc", full.names = TRUE)),
  tar_target(tlim_mc, import_motiondata(tlim_mc_files), pattern = map(tlim_mc_files), iteration = "list"),
  tar_target(tlim_mc_proc, proc_mc(tlim_mc, tlim = TRUE, fps = fps), pattern = map(tlim_mc, fps), iteration = "list"),
  tar_target(tlim_mc_params, mc_params(tlim_mc_proc), pattern = map(tlim_mc_proc), iteration = "list"),
  tar_target(tlim_mc_plot, plot_mc(tlim_mc_proc, id, "tlim"), pattern = map(tlim_mc_proc, id), iteration = "list"), # add id later  
  tar_files(tlim_pow_file, list.files("data/tlim/pow/", full.names = TRUE)),
  tar_target(tlim_pow, import_powerdata(tlim_pow_file, id = id), pattern = map(tlim_pow_file, id), iteration = "list"),
  tar_target(tlim_pow_proc, proc_pow(tlim_pow), pattern = map(tlim_pow), iteration = "list"),
  tar_target(tlim_pow_plot, pow_plot_circle(tlim_pow_proc, id, trial = "tlim", tlim = TRUE), pattern = map(tlim_pow_proc, id), iteration = "list"),
  
  # combined clt plot
  tar_target(clt_mc_plot, plot_mc_clt(c50_mc_proc, c4_mc_proc, tlim_mc_proc, id), pattern = map(c50_mc_proc, c4_mc_proc, tlim_mc_proc, id), iteration = "list"), # add id later  
  
  # id
  tar_target(id, gext_la$id, pattern = map(gext_la)),
  
  # get mc fps based on emg to mc ratio (assuming emg frequency 1000 Hz)
  tar_target(fps, get_fps(s1_emg_raw, s1_mc), map(s1_emg_raw, s1_mc), iteration = "list"),
  
  # emg_check
  tar_target(emg_check , read.csv("data/emg_check.csv")),
  
  # mvc
  tar_files(mvc_emg_files1, list.files("data/mvc/0/emg", full.names = TRUE)),
  tar_target(mvc_emg_raw1, import_emg(mvc_emg_files1), pattern = map(mvc_emg_files1), iteration = "list"),
  tar_target(mvc_emg1, proc1_emg(mvc_emg_raw1), pattern = map(mvc_emg_raw1), iteration = "list"),
  tar_files(mvc_pow_file1, list.files("data/mvc/0/pow", full.names = TRUE)),
  tar_target(mvc_pow1, import_powerdata(mvc_pow_file1, id = id), pattern = map(mvc_pow_file1, id), iteration = "list"),
  tar_target(mvc_pow_plot1, mvc_pow_proc(mvc_pow1, id = id, trial = 1), pattern = map(mvc_pow1, id), iteration = "list"),
  tar_files(mvc_emg_files2, list.files("data/mvc/90/emg", full.names = TRUE)),
  tar_target(mvc_emg_raw2, import_emg(mvc_emg_files2), pattern = map(mvc_emg_files2), iteration = "list"),
  tar_target(mvc_emg2, proc1_emg(mvc_emg_raw2), pattern = map(mvc_emg_raw2), iteration = "list"),
  tar_files(mvc_pow_file2, list.files("data/mvc/90/pow", full.names = TRUE)),
  tar_target(mvc_pow2, import_powerdata(mvc_pow_file2, id = id), pattern = map(mvc_pow_file2, id), iteration = "list"),
  tar_target(mvc_pow_plot2, mvc_pow_proc(mvc_pow2, id = id, trial = 2), pattern = map(mvc_pow2, id), iteration = "list"),
  tar_files(mvc_emg_files3, list.files("data/mvc/180/emg", full.names = TRUE)),
  tar_target(mvc_emg_raw3, import_emg(mvc_emg_files3), pattern = map(mvc_emg_files3), iteration = "list"),
  tar_target(mvc_emg3, proc1_emg(mvc_emg_raw3), pattern = map(mvc_emg_raw3), iteration = "list"),
  tar_files(mvc_pow_file3, list.files("data/mvc/180/pow", full.names = TRUE)),
  tar_target(mvc_pow3, import_powerdata(mvc_pow_file3, id = id), pattern = map(mvc_pow_file3, id), iteration = "list"),
  tar_target(mvc_pow_plot3, mvc_pow_proc(mvc_pow3, id = id, trial = 3), pattern = map(mvc_pow3, id), iteration = "list"),
  tar_files(mvc_emg_files4, list.files("data/mvc/270/emg", full.names = TRUE)),
  tar_target(mvc_emg_raw4, import_emg(mvc_emg_files4), pattern = map(mvc_emg_files4), iteration = "list"),
  tar_target(mvc_emg4, proc1_emg(mvc_emg_raw4), pattern = map(mvc_emg_raw4), iteration = "list"),
  tar_files(mvc_pow_file4, list.files("data/mvc/270/pow", full.names = TRUE)),
  tar_target(mvc_pow4, import_powerdata(mvc_pow_file4, id = id), pattern = map(mvc_pow_file4, id), iteration = "list"),
  tar_target(mvc_pow_plot4, mvc_pow_proc(mvc_pow4, id = id, trial = 4), pattern = map(mvc_pow4, id), iteration = "list"),
  tar_files(mvc_emg_files5, list.files("data/mvc/ef/emg", full.names = TRUE)),
  tar_target(mvc_emg_raw5, import_emg(mvc_emg_files5), pattern = map(mvc_emg_files5), iteration = "list"),
  tar_target(mvc_emg5, proc1_emg(mvc_emg_raw5), pattern = map(mvc_emg_raw5), iteration = "list"),
  tar_files(mvc_emg_files6, list.files("data/mvc/hg/emg", full.names = TRUE)),
  tar_target(mvc_emg_raw6, import_emg(mvc_emg_files6), pattern = map(mvc_emg_files6), iteration = "list"),
  tar_target(mvc_emg6, proc1_emg(mvc_emg_raw6), pattern = map(mvc_emg_raw6), iteration = "list"),
  tar_files(mvc_emg_files7, list.files("data/mvc/se/emg", full.names = TRUE)),
  tar_target(mvc_emg_raw7, import_emg(mvc_emg_files7), pattern = map(mvc_emg_files7), iteration = "list"),
  tar_target(mvc_emg7, proc1_emg(mvc_emg_raw7), pattern = map(mvc_emg_raw7), iteration = "list"),
  tar_files(mvc_emg_files8, list.files("data/mvc/tf/emg", full.names = TRUE)),
  tar_target(mvc_emg_raw8, import_emg(mvc_emg_files8), pattern = map(mvc_emg_files8), iteration = "list"),
  tar_target(mvc_emg8, proc1_emg(mvc_emg_raw8), pattern = map(mvc_emg_raw8), iteration = "list"),
  # create mvc power polar plots
  tar_target(mvc_pow_cross, mvc_cross_proc(mvc_pow1, mvc_pow2, mvc_pow3, mvc_pow4, id = id), pattern = map(mvc_pow1, mvc_pow2, mvc_pow3, mvc_pow4, id), iteration = "list"),
  # calc emg
  tar_target(
    mvc, 
    find_mvcs(mvc_emg1, mvc_emg2, mvc_emg3, mvc_emg4, mvc_emg5, mvc_emg6, mvc_emg7, mvc_emg8, id), 
    pattern = map(mvc_emg1, mvc_emg2, mvc_emg3, mvc_emg4, mvc_emg5, mvc_emg6, mvc_emg7, mvc_emg8, id),
    iteration = "list"
  )
)

convert_targets <- list(
  tar_files(pathraw1, list.files("data/tlim/mc-raw", full.names = TRUE)),
  tar_target(conv1, convert(pathraw1), pattern = map(pathraw1), iteration = "list"),
  tar_files(pathraw2, list.files("data/sprint-1/mc-raw", full.names = TRUE)),
  tar_target(conv2, convert(pathraw2), pattern = map(pathraw2), iteration = "list"),
  tar_files(pathraw3, list.files("data/sprint-2/mc-raw", full.names = TRUE)),
  tar_target(conv3, convert(pathraw3), pattern = map(pathraw3), iteration = "list"),
  tar_files(pathraw4, list.files("data/c4/mc-raw", full.names = TRUE)),
  tar_target(conv4, convert(pathraw4), pattern = map(pathraw4), iteration = "list"),
  tar_files(pathraw5, list.files("data/c50/mc-raw", full.names = TRUE)),
  tar_target(conv5, convert(pathraw5), pattern = map(pathraw5), iteration = "list"),
  tar_files(pathraw_mvc1, list.files("data/mvc/0/mc-raw", full.names = TRUE)),
  tar_target(conv_mvc1, convert(pathraw_mvc1), pattern = map(pathraw_mvc1), iteration = "list"),
  tar_files(pathraw_mvc2, list.files("data/mvc/90/mc-raw", full.names = TRUE)),
  tar_target(conv_mvc2, convert(pathraw_mvc2), pattern = map(pathraw_mvc2), iteration = "list"),
  tar_files(pathraw_mvc3, list.files("data/mvc/180/mc-raw", full.names = TRUE)),
  tar_target(conv_mvc3, convert(pathraw_mvc3), pattern = map(pathraw_mvc3), iteration = "list"),
  tar_files(pathraw_mvc4, list.files("data/mvc/270/mc-raw", full.names = TRUE)),
  tar_target(conv_mvc4, convert(pathraw_mvc4), pattern = map(pathraw_mvc4), iteration = "list"),
  tar_files(pathraw_mvc5, list.files("data/mvc/ef/mc-raw", full.names = TRUE)),
  tar_target(conv_mvc5, convert(pathraw_mvc5), pattern = map(pathraw_mvc5), iteration = "list"),
  tar_files(pathraw_mvc6, list.files("data/mvc/hg/mc-raw", full.names = TRUE)),
  tar_target(conv_mvc6, convert(pathraw_mvc6), pattern = map(pathraw_mvc6), iteration = "list"),
  tar_files(pathraw_mvc7, list.files("data/mvc/se/mc-raw", full.names = TRUE)),
  tar_target(conv_mvc7, convert(pathraw_mvc7), pattern = map(pathraw_mvc7), iteration = "list"),
  tar_files(pathraw_mvc8, list.files("data/mvc/tf/mc-raw", full.names = TRUE)),
  tar_target(conv_mvc8, convert(pathraw_mvc8), pattern = map(pathraw_mvc8), iteration = "list")
)

c(
  convert_targets,
  import_targets 
  ,report_targets # comment to exclude during development
)
