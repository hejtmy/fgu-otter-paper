library(fgu.avoidance)
library(ggplot2)
library(dplyr)
library(tidyr)
library(stringr)
source("functions/visualisations.R")

anim_arena <- load_data("data/OTTER_raw_runs 43_44_final_normative_data/arena size_contextB.CSV")
anim_dead <- load_data("data/OTTER_raw_runs 43_44_final_normative_data/blue_box_freezing_imtation of dead animal.CSV")

plot_path(anim_arena$animal_3)
summary(anim_arena$animal_3$position$data)

anim_round <- load_data("data/OTTER_raw_runs 43_44_final_normative_data/round arena size_contextA.CSV")
summary(anim_round$animal_8888$position$data)


anim_round <- load_data("data/OTTER_raw_runs 43_44_final_normative_data/hab2 control - 15min - average heatmap  for each context separately only/run43_hab2_rat04_contextA.CSV")
summary(anim_round$animal_4$position$data)

anim_arena$animal_3$position$area_boundaries

anim_arena <- load_data("data/OTTER_raw_runs 43_44_final_normative_data/arena size_contextB.CSV")
anim_arena