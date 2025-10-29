# Load in packages

library(MuscleTernary)

#Import Dataset
Varanus_mPPT <- read_xfiber_xml(
  system.file(
    "extdata",
    "Varanus_mPPT.xml",
    package = "MuscleTernary"
  )
) |>
  mutate(muscle = "Varanus_mPPT")

#Find track ends
Varanus_Ends <- Varanus_mPPT |> find_track_ends()

#Import origin and insertion attachment surfaces for central force axis and find centroids
Varanus_mPPT_O <- centroid_location(
  read_xfiber_xml(
    system.file(
      "extdata",
      "Varanus_mPPT_Origin_Smoothed.stl",
      package = "MuscleTernary"
    )
  )
)

Varanus_mPPT_I <- centroid_location(
  read_xfiber_xml(
    system.file(
      "extdata",
      "Varanus_mPPT_Insertion_Smoothed.stl",
      package = "MuscleTernary"
    )
  )
)

Varanus_mPPT <- tibble(
  muscle = "Varanus_mPPT",
  x_origin = Varanus_mPPT_O[1],
  y_origin = Varanus_mPPT_O[2],
  z_origin = Varanus_mPPT_O[3],
  x_insertion = Varanus_mPPT_I[1],
  y_insertion = Varanus_mPPT_I[2],
  z_insertion = Varanus_mPPT_I[3]
)

#Calculate central force axis from attachment centroids
Varanus_mPPT_Axis <- get_Central_Axis(Varanus_mPPT_O, Varanus_mPPT_I)

#Convert fiber track ends to unit vectors
Varanus_mPPT_Vectors <- ends_to_vectors(Varanus_Ends)

#Measure pennaåΩtion angles around central axis
Varanus_mPPT_Pennation <- pennation_angle(
  Varanus_mPPT_Vectors,
  Varanus_mPPT_Axis
)

View(Varanus_mPPT_Vectors)

ggplot(Varanus_mPPT_Pennation, aes(x = PennationAngle)) +
  geom_density()

mean(Varanus_mPPT_Pennation$PennationAngle)
