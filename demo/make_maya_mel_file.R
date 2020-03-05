library(MuscleTernary)

d <- read_csv(system.file("extdata",
                     "AL_008_data.csv",
                     package = "MuscleTernary")) %>%
  dplyr::select(-side)

make_mel(stl = "MyStlFile.stl",
         data = d)

make_mel(stl = "My_Stl_File.stl",
         data = d,
         outfile = "Maya_File.mel")

