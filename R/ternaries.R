#Reads data from Excel. Change the following lines to match your data.
data <- read_excel("inst/extdata/AL_700_attachments.xlsx", sheet = "Sheet1")
colnames(data) <- c("muscle", "x_origin", "y_origin", "z_origin", "x_insertion",
                    "y_insertion", "z_insertion")
rownames(data)<-data[,1]
data<-data[,2:7]

#Creates empty matrices to store coordinates of origins and insertions.
coords_or <- matrix(NA, 18, 3)
coords_ins <- matrix(NA , 18, 3)

for (i in 1:nrow(data)) {
  coords_or[i,] <- c(data[i,"x_origin"], data[i,"y_origin"], data[i,"z_origin"])
}

for (j in 1:nrow(data)) {
  coords_ins[j,]<-c(data[j,"x_insertion"], data[j,"y_insertion"], data[j,"z_insertion"])
}


#Calculating vectfor from origin to insertion.
vectors <- coords_or - coords_ins
rownames(vectors) <- rownames(data)
colnames(vectors) <- c("x","y","z")


#Set up empty matrix to store unit vectors.
unit_vectors<-matrix(NA, 18, 3)
rownames(unit_vectors) <- rownames(data)
colnames(unit_vectors) <- c("x","y","z")

#Calculate unit vectors.
for (k in 1:nrow(vectors)) {
  #p<-c(vectors[k,1], vectors[k,2], vectors[k,3])
  p <- t(matrix(vectors[k,]))
  q <- (c(p[1,1],p[1,2],p[1,3]))
  unit_vectors[k,] <- make_unit_vector(q)
}

#Set up empty matrix to store proportional vectors.
prop_vectors <- matrix(NA, nrow(data), ncol(vectors))
rownames(prop_vectors) <- rownames(data)
colnames(prop_vectors) <- c("x", "y", "z")

#Calculate proportional vectors.
for (l in 1:nrow(vectors)) {
  #p<-c(vectors[k,1], vectors[k,2], vectors[k,3])
  m <- t(matrix(vectors[l,]))
  n <- (c(m[1,1],m[1,2],m[1,3]))
  prop_vectors[l,] <- relative_proportion(n)
}

df_to_plot <- as.data.frame(prop_vectors)
df_to_plot <- df_to_plot * 100

ggtern(df_to_plot, aes(x, y, z)) +
  geom_point() +
  theme_showarrows()
