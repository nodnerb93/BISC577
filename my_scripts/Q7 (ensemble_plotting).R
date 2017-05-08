######################################
# 04.30.2017
# Emsemble plots for CTCF
# BISC 577
######################################

# Initialization
library(DNAshapeR)
library(fields)

# Change legend.shrink to 0.5
fix(heatShape)

# Extract sample sequences
bound <- "C:/Users/nodne/Desktop/BISC577/CTCF/bound_500.fa"
unbound <- "C:/Users/nodne/Desktop/BISC577/CTCF/unbound_500.fa"

# Predict DNA shapes
bound_pred <- getShape(bound)
unbound_pred <- getShape(unbound)

# Generate ensemble plots
heatShape(bound_pred$MGW, 20, axis.args=list(cex.axis = 0.9)) + title("MGW - Bound")
ticks <- c(-3, -6, -9, -12, -15)
heatShape(bound_pred$ProT, 20, axis.args=list(cex.axis = 0.9, at = ticks, labels = ticks)) + 
  title ("ProT - Bound")
plotShape(bound_pred$Roll) + title("Roll - Bound")
plotShape(bound_pred$HelT) + title("HelT - Bound")

heatShape(unbound_pred$MGW, 20, axis.args = list(cex.axis = 0.9)) + title("MGW - Unbound")
heatShape(unbound_pred$ProT, 20, axis.args = list(cex.axis = 0.9, at = ticks, labels = ticks)) + 
  title("ProT - Unbound")
plotShape(unbound_pred$Roll) + title("Roll - Unbound")
plotShape(unbound_pred$HelT) + title("HelT - Unbound")

