coords_process <- function(coords_infile, Force_info) {
  require(readxl)
  require(dplyr)
  library(MuscleTernary)
  
#   coords_infile <- coords_infile_neut
#   Force_info <- M_extras
  
  # Read coordinate data
  M <- read_excel(coords_infile)
  
  #Calculating vector from origin to insertion.
  vectors <- as.matrix(M[, 2:4]) - as.matrix(M[, 5:7])
  
  # Pass rows sequentially to make_unit_vector and relative proportion
  unit_vectors <- t(apply(vectors, 1, make_unit_vector))
  prop_vectors <- t(apply(vectors, 1, relative_proportion))
  
  # Need a data.frame for ggplot.
  prop_vectors <- as.data.frame(prop_vectors * 100)
  
  # Rename columns to x, y, z
  # Change unit vector function to do this
  names(prop_vectors) <- c("x", "y", "z")
  
  # Put Muscle column back on
  prop_vectors$muscle <- M$muscle
  
  df_to_plot <- merge(prop_vectors, Force_info)
  
  # Make Left_Right column
  df_to_plot$Left_Right <- ifelse(grepl("^L", df_to_plot$muscle), "L", "R")
  
  # Drop L and R
  df_to_plot$muscle <- gsub("L ", "", df_to_plot$muscle)
  df_to_plot$muscle <- gsub("R ", "", df_to_plot$muscle)
    
  ###################################################################################
  
  # Means by Left_Right
  df_means <- df_to_plot %>% select(-c(Muscle_Group, Left_Right)) %>%
    group_by(muscle) %>% summarise_each(funs(mean))
  
  # Put Muscle_Group back on. Note: Assumes alternating rows
  df_means$Muscle_Group <- Force_info$Muscle_Group[seq(1, nrow(df_to_plot), by = 2)]
  
  return(df_means)
}
