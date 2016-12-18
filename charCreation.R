getStats <- function(method = c("roll", "pointbuy"), disp = TRUE) {
     if (method=="roll") {
          rolls <- matrix(sample.int(6, size = 24, replace = TRUE), nrow = 6, ncol = 4)
          rollsSorted <- apply(rolls, 1, sort)
          stats <- colSums(rollsSorted[2:4,])
          if (disp) {
               for (i in 1:6) {
                    cat("Stat ", i, ": ", rolls[i,1], ", ", rolls[i,2], ", ", rolls[i,3], ", ", rolls[i,4], "\n", sep = "")
               }
          }
          return(stats)
     }
}