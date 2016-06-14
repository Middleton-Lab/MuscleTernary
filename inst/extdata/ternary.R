# install_github("thomasp85/tweenr")
# install_github("dgrtwo/gganimate")
# install_github("thomasp85/ggforce")

# library("tweenr")
# library("ggforce")

# library("gganimate")
library("MuscleTernary")
# library("tidyr")
library("animation")
library("ggrepel")
source(system.file("extdata", "coords_process.R", package = "MuscleTernary"))

interp_ternary <- function(start, end, length_out){
  out <- matrix(NA, ncol = 3, nrow = length_out)
  out[, 1] <- seq(start[1, 1], end[1, 1], length.out = length_out)
  out[, 2] <- seq(start[1, 2], end[1, 2], length.out = length_out)
  out[, 3] <- seq(start[1, 3], end[1, 3], length.out = length_out)
  out <- as.data.frame(out)
  names(out) <- c("x", "y", "z")
  out$.frame <- 1:length_out
  return(out)
}

row_to_interp <- function(x, length_out = 200){
  interp_pts <- interp_ternary(start = x[, c("x_1", "y_1", "z_1")],
                               end = x[, c("x_2", "y_2", "z_2")],
                               length_out = length_out)
  interp_pts <- cbind(x, interp_pts, row.names = NULL)
  return(interp_pts)
}

merge_coords <- function(M1, M2){
  names(M1) <- paste0(names(M1), "_1")
  names(M2) <- paste0(names(M2), "_2")
  M <- cbind(M1, M2)
  return(M)
}

#Change the directory to wherever your data Excel sheet is.
# setwd("C:\\Users\\labuser\\Google Drive\\backup of things\\shiznit stuff\\ternary_data")

coords_file_low <- "AL_008_coords.xlsx"
force_file_low <- "AL_008_forces.xlsx"
coords_file_high <- "AL_031_coords.xlsx"
force_file_high <- "AL_031_forces.xlsx"

AL_008 <- read_coords(coords_file_low, force_file_low)
AL_031 <- read_coords(coords_file_high, force_file_high)

## Take means of L and R
AL_008_mean <- AL_008 %>% select(muscle, x, y, z, force) %>%
  group_by(muscle) %>% summarise_each(funs(mean))
AL_008_mean <- cbind(AL_008_mean, AL_008[seq(1, nrow(AL_008), 2), 6:8])

AL_031_mean <- AL_031 %>% select(muscle, x, y, z, force) %>%
  group_by(muscle) %>% summarise_each(funs(mean))
AL_031_mean <- cbind(AL_031_mean, AL_031[seq(1, nrow(AL_031), 2), 6:8])

M <- merge_coords(AL_031_mean, AL_008_mean)

## Interpolate
length_out <- 100
D <- list()
for (i in 1:nrow(M)) {
  D[[i]] <- row_to_interp(M[i, ], length_out = length_out)
}
D <- do.call(rbind, D)

## manually
P <- list()

for (i in 1:length_out) {
  d <- D %>% filter(.frame == i)
  P[[i]] <- ggtern(d, aes(x = x, y = y, z = z,
                          color = muscle_1)) +
    geom_point(size = 5) +
    muscle_color_map() +
    theme_bw() +
    theme_showarrows() +
    Tlab("DV") +
    Rlab("RC") +
    Llab("ML") +
    guides(colour = guide_legend(override.aes = list(size = 5),
                                 ncol = 2, byrow = TRUE))
}

animation::ani.options(interval = 1/24)
saveGIF({for (i in 1:length_out) print(P[[i]])},
        movie.name = "ternary_manual.gif",
        ani.width = 800, ani.height = 600)

## with gganimate
# p <- ggtern(D, aes(x = x, y = y, z = z,
#                    color = muscle_1,
#                    frame = .frame)) +
#   geom_point() +
#   muscle_color_map() +
#   theme_showarrows() +
#   Tlab("DV")+
#   Rlab("RC")+
#   Llab("ML")+
#   guides(colour = guide_legend(override.aes = list(size = 5),
#                                ncol = 2, byrow = TRUE))
#
# animation::ani.options(interval = 1/24)
# gg_animate(p, 'ternary.gif', title_frame = F, ani.width = 800,
#            ani.height = 600)
