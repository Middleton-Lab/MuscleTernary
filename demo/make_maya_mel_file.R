library(MuscleTernary)

data <- read_csv(system.file("extdata",
                             "AL_008_data.csv",
                             package = "MuscleTernary"))

make_mel(stl = "AL_008.stl",
         shader_file = "default",
         data = data)
