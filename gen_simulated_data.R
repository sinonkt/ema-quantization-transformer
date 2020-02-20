# install.packages("zeallot")
library(zeallot)

simulate <- function (binStep) {
  startBinPos=140000
  endBinPos=200000
  sims_data_dists <- rbind(
    c(startBinPos, 150000, -0.1, 0.9),
    c(150000, 165000, -0.2, 0.5),
    c(165000, 172000, -2.8, 0.4),
    c(172000, 176000, -2.0, 0.4),
    c(176000, 185000, -2.3, 0.4),
    c(185000, endBinPos, -0.3, 0.8)
  )

  sims_data <- apply(sims_data_dists, 1, function(x) {
    c(start, end, mu, sd) %<-% x
    rnorm((end-start)/binStep, mean=mu, sd=sd)
  })

  x <- seq(startBinPos, endBinPos-1, by=binStep)
  y <- unlist(as.vector(sims_data), recursive=TRUE)
  list("x"=x , "y"=y)
}