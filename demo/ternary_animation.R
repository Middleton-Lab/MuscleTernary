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

AL_031 <- read_coords(system.file("extdata",
                                  "AL_008_coords.xlsx",
                                  package = "MuscleTernary"),
                      system.file("extdata",
                                  "AL_008_forces.xlsx",
                                  package = "MuscleTernary"),
                      L_R_means = FALSE)

M <- merge_coords(AL_031, AL_008)

## Interpolate each row into length_out new rows.
length_out <- 100
D <- list()
for (i in 1:nrow(M)) {
  D[[i]] <- interpolate_ternary(M[i, ],
                                length_out = length_out)
}
D <- do.call(rbind, D)

## Iterate through .frame and make a plot for each. Add to a list
## of plots.
P <- list()

for (i in 1:length_out) {
  d <- D %>% filter(.frame == i)
  P[[i]] <- ggtern(d, aes(x = x, y = y, z = z,
                          color = muscle_1)) +
    geom_point(size = 5) +
    muscle_color_map() +
    labs( x       = "ML",
          xarrow  = "Mediolateral",
          y       = "DV",
          yarrow  = "Dorsoventral",
          z       = "RC",
          zarrow  = "Rostrocaudal") +
    theme_bw(base_size = 16) +
    theme_showarrows() +
    guides(colour = guide_legend(override.aes = list(size = 5),
                                 ncol = 2, byrow = TRUE))
}

# Set interval to 1/24 s.
ani.options(interval = 1/24)

# For OS X with ImageMagick installed somewhere on the path
# e.g., using homebrew.
ani.options(convert = "convert")

# For windows, install the ImageMagick standalone release:
# (http://www.imagemagick.org/script/binary-releases.php) Use a
# variation of the next line to set the absolute path to convert.exe.
# ani.options(convert = 'C:\\Program Files\\Image Magic\\convert.exe')

saveGIF({for (i in 1:length_out) print(P[[i]])},
        movie.name = "ternary_animation.gif",
        ani.width = 800, ani.height = 600)
