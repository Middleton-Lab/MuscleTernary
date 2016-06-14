library("MuscleTernary")

coords_infile <- "Chicken_coords.xlsx"
force_infile <- "Chicken_forces.xlsx"
force_factor <- 1

# Read force data
M_extras <- read_excel(force_infile) %>% as.data.frame()

# Normalize force to 1 or something
M_extras$force_norm <- M_extras$force / max(M_extras$force)
M_extras$force_norm <- M_extras$force_norm * force_factor

# Process coordinates files to get LR means
df_means <- coords_process(coords_infile, M_extras)

#####################################################################################################

ggtern(df_means, aes(x = x, y = y, z = z,
                     color = muscle,
                     size = force_norm)) +
  geom_point() +
  guides(shape = FALSE) +
  muscle_color_map() +
  scale_size_continuous(range = c(3, 10), name = "Force (N)") +
  guides(colour = guide_legend(override.aes = list(size = 5))) +
  labs( x       = "ML",
        xarrow  = "Mediolateral",
        y       = "DV",
        yarrow  = "Dorsoventral",
        z       = "RC",
        zarrow  = "Rostrocaudal") +
  theme_bw(base_size = 16) +
  theme_showarrows()
