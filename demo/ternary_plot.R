library("MuscleTernary")

## Load AL008 and AL031 data, calculate means, and merge into one
## data.frame.

AL_008 <- read_coords(system.file("extdata",
                                  "AL_008_coords.xlsx",
                                  package = "MuscleTernary"),
                      system.file("extdata",
                                  "AL_008_forces.xlsx",
                                  package = "MuscleTernary"),
                      L_R_means = TRUE)

ggtern(AL_008, aes(x = x, y = y, z = z,
               color = muscle,
               size = force)) +
  geom_point() +
  muscle_color_map() +
  labs( x       = "ML",
        xarrow  = "Mediolateral",
        y       = "DV",
        yarrow  = "Dorsoventral",
        z       = "RC",
        zarrow  = "Rostrocaudal") +
  theme_bw(base_size = 16) +
  theme_showarrows() +
  scale_size_continuous(range = c(5, 15), name = "Force (N)") +
  guides(colour = guide_legend(override.aes = list(size = 6),
                               ncol = 2, byrow = TRUE))