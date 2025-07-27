library(readxl)
library(tidyverse)

#Nothing crazy here, there are no missing values so its mainly extracting treatments from the beaker columns and getting the right data types


#Calyx Data
calyx_data <- read_excel("Project3_S25_Data (1).xlsx", 
                         sheet = "CalyxLongData")
calyx_data <- calyx_data |> 
  separate_wider_position(
    beaker,
    widths = c(temperature = 2, water = 1, replicate = 1),
    cols_remove = FALSE
  ) |> 
  mutate(temperature = factor(temperature, c("20","28","33")),
         water = factor(water, c("R","A")), time = factor(time, c(0,34))) |> 
  mutate(treatment = str_c(temperature, water, sep = " "))

save(calyx_data, file = "calyx_data.RData")


#Count Data
count_data <- read_excel("Project3_S25_Data (1).xlsx", 
                         sheet = "ABS_LongCountData")

count_data <- count_data |> 
  separate_wider_position(
    beaker,
    widths = c(temperature = 2, water = 1, replicate = 1),
    cols_remove = FALSE
  ) |> 
  pivot_wider(names_from = stage, values_from = count) |> 
  mutate(temperature = factor(temperature, c("20","28","33")),
         water = factor(water, c("R","A")))
save(count_data, file = "count_data.RData")

#DNA Data
DNA_data <- read_excel("Project3_S25_Data (1).xlsx", 
                       sheet = "Samples_qPCR")

DNA_data <- DNA_data |>
  mutate(flag = substr(Treatment_Group, 1, 2) == "T0") |>
  mutate(time = ifelse(flag,substr(Treatment_Group, 2, 2),
                       substr(Treatment_Group, 2, 3))) |> 
  mutate(temperature = ifelse(flag, substr(Treatment_Group,6,7),
                              substr(Treatment_Group,7,8))) |> 
  mutate(water = ifelse(flag, substr(Treatment_Group,9,9),
                        substr(Treatment_Group,9,9))) |> 
  mutate(rep = ifelse(flag, "NONE", substr(Treatment_Group,10,10)))|> 
  mutate(temperature = factor(temperature, c("20","28","33")),
         water = factor(water, c("R","A")),
         time = factor(time, c(0,35))) |>
  mutate(treatment = str_c(temperature, water, sep = " ")) |> 
  select(-flag)
save(DNA_data, file = "DNA_data.RData")

#Ephyra Data

ephyra_data <- read_excel("Project3_S25_Data (1).xlsx", 
                          sheet = "EphyraLongData")

ephyra_data <- ephyra_data |> 
  separate_wider_position(
    beaker,
    widths = c(temperature = 2, water = 1, replicate = 1),
    cols_remove = FALSE
  ) |> 
  mutate(temperature = factor(temperature, c("20","28","33")),
         water = factor(water, c("R","A"))) |> 
  mutate(treatment = str_c(temperature, water, sep = " "))


save(ephyra_data, file = "ephyra_data.RData")
