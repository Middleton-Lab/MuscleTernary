generate_shader <- function(shader, outfile) {
  # Checks on columns names
  if (!(muscle %in% names(shader))) {
    stop("Shader should have a 'muscle' column.")
  }

  # Lambert counter
  l <- 2

  # Bone shader
  write('\n// Bone shader', file = outfile, append = TRUE)
  write('shadingNode -asShader lambert;', file = outfile, append = TRUE)
  write(paste0('sets -renderable true -noSurfaceShader true -empty -name lambert', l, 'SG;'),
        file = outfile, append = TRUE)
  write(paste0('connectAttr -f lambert', l, '.outColor lambert', l, 'SG.surfaceShader;'),
        file = outfile, append = TRUE)
  write(paste0('rename lambert', l, ' "Bone" ;'),
        file = outfile, append = TRUE)
  write('setAttr "Bone.color" -type double3 0.804 0.798 0.599 ;',
        file = outfile, append = TRUE)
  write('setAttr "Bone.transparency" -type double3 0.5 0.5 0.5 ;',
        file = outfile, append = TRUE)
  write('rename lambert2SG "BoneSG" ;', file = outfile, append = TRUE)

  # Iterate through shader file
  nul <- purrr::map(.x = 1:nrow(shader),
                    .f = function(ii, shader, outfile) {
    r <- shader %>% slice(ii) %>% as.data.frame()
    write(paste0('\n// ', r$muscle, ' shader'),
          file = outfile, append = TRUE)
    write('shadingNode -asShader lambert;', file = outfile, append = TRUE)
    write('sets -renderable true -noSurfaceShader true -empty -name lambert2SG;',
          file = outfile, append = TRUE)
    write('connectAttr -f lambert2.outColor lambert2SG.surfaceShader;',
          file = outfile, append = TRUE)
    write(paste0('rename lambert2 ', r$muscle, ' ;'),
          file = outfile, append = TRUE)
    write(paste0('setAttr "', r$muscle, '.color" -type double3 ', r$R1, ' ', r$G1, ' ',  r$B1, ' ;'),
          file = outfile, append = TRUE)
    write(paste0('rename lambert2SG ', r$muscle, 'SG ;'),
          file = outfile, append = TRUE)
  },
  shader = shader,
  outfile = outfile)
}
