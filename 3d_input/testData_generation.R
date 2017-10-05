




frame2D_set1 <- as.data.frame(matrix(0, nrow = 100, ncol = 93))

for (i in seq(1, nrow(frame2D_set1), 1)) {
  frame2D_set1[i, ] <- runif(ncol(frame2D_set1), 0, 1)
}

write.table(frame2D_set1, file = "~/R/_workingDirectory/bagOfDrugs/3d_input/frame2D_set1.csv", sep=",", row.names = FALSE)

frame2D_set2 <- as.data.frame(matrix(0, nrow = 100, ncol = 93))

for (i in seq(1, nrow(frame2D_set2), 1)) {
  frame2D_set2[i, ] <- runif(ncol(frame2D_set2), 0, 1)
}

write.table(frame2D_set2, file = "~/R/_workingDirectory/bagOfDrugs/3d_input/frame2D_set2.csv", sep=",", row.names = FALSE)


frame2D_set3 <- as.data.frame(matrix(0, nrow = 100, ncol = 93))

for (i in seq(1, nrow(frame2D_set3), 1)) {
  frame2D_set3[i, ] <- runif(ncol(frame2D_set3), 0, 1)
}

write.table(frame2D_set3, file = "~/R/_workingDirectory/bagOfDrugs/3d_input/frame2D_set3.csv", sep=",", row.names = FALSE)

boolean_y_set <- as.data.frame(matrix(0, nrow = 100, ncol = 93))

for (i in seq(1, nrow(boolean_y_set), 1)) {
  boolean_y_set[i, ] <- sample(c(0, 1), ncol(boolean_y_set), replace = T)
}
write.table(boolean_y_set, file = "~/R/_workingDirectory/bagOfDrugs/3d_input/boolean_y.csv", sep=",", row.names = FALSE)


gen_y <- sample(c(0, 1), nrow(frame2D_set1), replace = T)
write.table(gen_y, file = "~/R/_workingDirectory/bagOfDrugs/3d_input/gen_y.csv", sep=",", row.names = FALSE)

##
# try building 3d array in R
# 
# frame2D_set1$bind = 1
# frame2D_set2$bind = 2
# frame2D_set3$bind = 3
# 
# frame2D <- rbind(frame2D_set1, frame2D_set2, frame2D_set3)
# frame3D <- split(frame2D, frame2D$bind)
# frame3D[[1]]$bind <- NULL
# frame3D[[2]]$bind <- NULL
# frame3D[[3]]$bind <- NULL
# 
# write.table(frame3D, file = "~/R/_workingDirectory/bagOfDrugs/3d_input/frame3D.csv", sep=",", row.names = FALSE)

