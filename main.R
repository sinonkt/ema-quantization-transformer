# install.packages("QuantTools")
# install.packages("lambda.tools")
library(zeallot)
library(QuantTools)
library(lambda.tools)
source('./gen_simulated_data.R')
############################# Knobs #########################################
binStep=40
emaWindowSize=5
numLevels=4

############################# Get Simulated Data ############################
simulated_data <- simulate(binStep) 

############################# Transforms ############################
simulated_data$ema <- ema(simulated_data$y, emaWindowSize)
maxEma <- max(simulated_data$ema, na.rm = TRUE)
minEma <- min(simulated_data$ema, na.rm = TRUE)
distance = (maxEma-minEma)/numLevels
bins=rev(seq(minEma, maxEma, by=distance))
simulated_data$quantized <- quantize(simulated_data$ema, bins=bins)

############################# Visualize #############################
par(mfrow=c(2,1))
emaLabel <- paste("EMA (", emaWindowSize * binStep, ")")
plot(simulated_data$x, simulated_data$y, type='l', col='grey', ylim=c(-4,4),
  main=paste("Transfromed with ", emaLabel),
  ylab=emaLabel,
  panel.first=c(abline(0, -1, h=0, lty=1, col='black'))
)
lines(simulated_data$x, simulated_data$ema, col='red')

quantizedLabel <- paste("Quantized (", numLevels, ")")
plot(simulated_data$x, simulated_data$y, type='l', col='grey', ylim=c(-4,4),
  main=paste("Transformed with ", emaLabel, " Then ", quantizedLabel),
  ylab=quantizedLabel,
  panel.first=c(abline(0, -1, h=0, lty=1, col='black'))
)
for (level in 0:numLevels+1) {
  abline(0, -1, h=bins[level], lty=2, col='grey')
}
lines(simulated_data$x, simulated_data$quantized, col='red')
