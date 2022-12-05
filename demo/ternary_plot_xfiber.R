library(MuscleTernary)

D <- read_xfiber_xml(system.file("extdata",
                                 "AV069_SC.xml",
                                 package = "MuscleTernary")) |>
  mutate(muscle = "SC")

coords_to_ternary(D) |>
  ggtern(aes(x = x, y = y, z = z)) +
  geom_point(size = 1, alpha = 0.25, pch = 16) +
  labs(x       = "ML",
       xarrow  = "Mediolateral",
       y       = "DV",
       yarrow  = "Dorsoventral",
       z       = "RC",
       zarrow  = "Rostrocaudal") +
  theme_bw(base_size = 16) +
  theme_showarrows()
