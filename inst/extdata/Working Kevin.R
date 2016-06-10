library("ggplot2")
library("ggtern")
library("MuscleTernary")
library("readxl")
library("dplyr")

source("coords_process.R")

coords_infile <- "Chicken coords.xlsx"
force_infile <- "Chicken forces.xlsx"
force_factor <- 1

# Read force data
M_extras <- read_excel(force_infile) %>% as.data.frame()

# Normalize force to 1 or something
M_extras$force_norm <- M_extras$force / max(M_extras$force)
M_extras$force_norm <- M_extras$force_norm * force_factor

# Process coordinates files to get LR means
df_means <- coords_process(coords_infile, M_extras)

#####################################################################################################
ggtern(df_means, aes(x = x, y = y, z = z, color = muscle, size = force_norm)) +
  geom_point() +
  scale_size_area(max_size = 10) +
  theme_showarrows() +
  ggtitle(coords_infile)

ggtern(df_means, aes(x = x, y = y, z = z, color = muscle, size = force_norm)) +
  geom_point() +
  theme_showarrows() +
  guides(shape = FALSE) +
  muscle_color_map() +
  scale_size_continuous(range = c(3, 10), name = "Force (N)") +
  guides(colour = guide_legend(override.aes = list(size=5))) +
  Tlab("DV") +
  Rlab("RC") +
  Llab("ML") +
  theme(axis.tern.text = element_text(size = 14))
