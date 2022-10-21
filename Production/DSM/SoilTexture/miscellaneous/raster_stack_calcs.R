library(raster)
set.seed(123)
r <- raster(ncol=10, nrow=10)
r_brick <- brick(sapply(1:45, function(i) setValues(r, rnorm(ncell(r), i, 3))))
r_stack<- stack(r_brick)

# Calculate mean
r_mean <- calc(r_brick, mean)
# Calculate median
r_median <- calc(r_brick, median)
# Calculate sd
r_sd <- calc(r_brick, sd)
# Calculate sd
r_var <- calc(r_brick, var)
plot(r_var)
