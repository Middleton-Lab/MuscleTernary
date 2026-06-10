# write_arrows output is stable (rev_arrows = TRUE)

    Code
      writeLines(mel_round(readLines(tmpf)))
    Output
      /////////////////////////////////////
      // mPTd_L
      curve -n curve1 -d 1 -p 10 20 30 -p 5 15 25 -k 0 -k 1;
      circle -n circ -ch on -o on -c 10 20 30 -nrx 0 -nry 1 -nrz 0 -radius 2;
      rotate -r -pivot 10 20 30 -xyz -69.8960 52.0618 69.8960 circ;
      extrude -n mPTd_Lcyl -et 1 -po 0 circ curve1;
      cone -n mPTd_LCone -po 0 -axis 0 1 0 -r 4 -hr 2;
      rotate -r -xyz -69.8960 52.0618 69.8960 mPTd_LCone;
      move 5 15 25 mPTd_LCone;
      select -r curve1;
      doDelete;
      select -r circ;
      doDelete;
      select -r mPTd_LCone mPTd_Lcyl;
      sets -e -forceElement mPTdSG;
      reverseSurface -ch on -rpo on -d 3 mPTd_Lcyl;
      
      

# write_arrows output is stable (rev_arrows = FALSE)

    Code
      writeLines(mel_round(readLines(tmpf)))
    Output
      /////////////////////////////////////
      // mPTd_L
      curve -n curve1 -d 1 -p 5 15 25 -p 10 20 30 -k 0 -k 1;
      circle -n circ -ch on -o on -c 5 15 25 -nrx 0 -nry 1 -nrz 0 -radius 2;
      rotate -r -pivot 5 15 25 -xyz 36.2060 12.2000 -36.2060 circ;
      extrude -n mPTd_Lcyl -et 1 -po 0 circ curve1;
      cone -n mPTd_LCone -po 0 -axis 0 1 0 -r 4 -hr 2;
      rotate -r -xyz 36.2060 12.2000 -36.2060 mPTd_LCone;
      move 10 20 30 mPTd_LCone;
      select -r curve1;
      doDelete;
      select -r circ;
      doDelete;
      select -r mPTd_LCone mPTd_Lcyl;
      sets -e -forceElement mPTdSG;
      reverseSurface -ch on -rpo on -d 3 mPTd_Lcyl;
      
      

# write_segment output is stable

    Code
      writeLines(mel_round(readLines(tmpf)))
    Output
      curve -n curve1 -d 1 -p 10 20 30 -p 5 15 25 -k 0 -k 1;
      circle -n circ -ch on -o on -c 10 20 30 -nrx 0 -nry 1 -nrz 0 -radius 8;
      rotate -r -pivot 10 20 30 -xyz -69.8960 52.0618 69.8960 circ;
      extrude -n tr_1_2cyl -et 1 -po 0 circ curve1;
      select -r curve1;
      doDelete;
      select -r circ;
      doDelete;
      
      
      

# generate_shader output is stable

    Code
      writeLines(readLines(tmpf))
    Output
      
      // Bone shader
      shadingNode -asShader lambert;
      sets -renderable true -noSurfaceShader true -empty -name lambert2SG;
      connectAttr -f lambert2.outColor lambert2SG.surfaceShader;
      rename lambert2 "Bone" ;
      setAttr "Bone.color" -type double3 0.804 0.798 0.599 ;
      setAttr "Bone.transparency" -type double3 0.5 0.5 0.5 ;
      rename lambert2SG "BoneSG" ;
      
      // mPTd shader
      shadingNode -asShader lambert;
      sets -renderable true -noSurfaceShader true -empty -name lambert2SG;
      connectAttr -f lambert2.outColor lambert2SG.surfaceShader;
      rename lambert2 mPTd ;
      setAttr "mPTd.color" -type double3 0.1 0.3 0.5 ;
      rename lambert2SG mPTdSG ;
      
      // mPTv shader
      shadingNode -asShader lambert;
      sets -renderable true -noSurfaceShader true -empty -name lambert2SG;
      connectAttr -f lambert2.outColor lambert2SG.surfaceShader;
      rename lambert2 mPTv ;
      setAttr "mPTv.color" -type double3 0.2 0.4 0.6 ;
      rename lambert2SG mPTvSG ;

# .write_stl_import output is stable

    Code
      writeLines(readLines(tmpf))
    Output
      
      // Import stl model
      file -import -type "STLImport" -ignoreVersion -ra true -mergeNamespacesOnClash false -namespace "model" -pr "/models/model.stl";
      select -r model;
      sets -e -forceElement BoneSG;
      

# .write_mel_header note line is stable

    Code
      writeLines(lines)
    Output
      // Note: the ratio of max to min forces is 4
      

