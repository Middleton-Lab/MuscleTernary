library(MuscleTernary)

read_csv(system.file("extdata",
                     "AL_008_data.csv",
                     package = "MuscleTernary")) %>%
  dplyr::select(-side) %>%
  coords_to_ternary(., grouping = c("muscle")) %>%
  ggtern(aes(x = x, y = y, z = z,
             color = muscle,
             size = force)) +
  geom_point() +
  muscle_color_map() +
  labs(x       = "ML",
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

read_csv(system.file("extdata",
                     "AL_031_data.csv",
                     package = "MuscleTernary")) %>%
  dplyr::select(-side) %>%
  coords_to_ternary(., grouping = c("muscle")) %>%
  ggtern(aes(x = x, y = y, z = z,
             color = muscle,
             size = force)) +
  geom_point() +
  muscle_color_map() +
  labs(x       = "ML",
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

read_csv(system.file("extdata",
                     "Chicken_data.csv",
                     package = "MuscleTernary")) %>%
  dplyr::select(-side) %>%
  coords_to_ternary(., grouping = c("muscle")) %>%
  ggtern(aes(x = x, y = y, z = z,
             color = muscle,
             size = force)) +
  geom_point() +
  muscle_color_map() +
  labs(x       = "ML",
       xarrow  = "Mediolateral",
       y       = "DV",
       yarrow  = "Dorsoventral",
       z       = "RC",
       zarrow  = "Rostrocaudal") +
  theme_bw(base_size = 16) +
  theme_showarrows() +
  scale_size_continuous(range = c(5, 15), name = "Force (N)") +
  guides(colour = guide_legend(override.aes = list(size = 6),
                               ncol = 2, byrow = TRUE),
         size = guide_legend(override.aes = list(size = 6),
                             ncol = 2, byrow = TRUE))
