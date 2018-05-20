library(epitrix)
library(distcrete)

mu <- 15.3 # mean in days days
sigma <- 9.3 # standard deviation in days
cv <- sigma / mu # coefficient of variation
cv
## [1] 0.6078431
param <- gamma_mucv2shapescale(mu, cv) # convertion to Gamma parameters
param
## $shape
## [1] 2.706556
## 
## $scale
## [1] 5.652941

si <- distcrete("gamma", interval = 1,
                shape = param$shape,
                scale = param$scale, w = 0)
si
## A discrete distribution
##   name: gamma
##   parameters:
##     shape: 2.70655567117586
##     scale: 5.65294117647059

plot(0:60, si$d(0:60), type = "h", xlab = "Days after onset",
     ylab = "P(secondary symptoms)", lend = 2, lwd = 4, col = "#666699",
     main = "Serial interval distribution")
library(incidence)
library(projections)

make_simul <- function(n_initial_cases = 1, time_period = 30, n_rep = 100, ...) {
  out <- vector(length = n_rep, mode = "list")
  for (i in seq_len(n_rep)) {
    fake_data <- c(0, # index case
                   sample(seq_len(time_period),
                          n_initial_cases - 1,
                          replace = TRUE)
    )
    fake_epicurve <- incidence::incidence(fake_data)
    out[[i]] <- project(fake_epicurve, ...)
  }
  
  do.call(cbind, out)
}

## Assuming 20 initial cases distributed over a month#### 

R_values <- seq(1.3, 4.7, length = 1000)

res_20 <- make_simul(n_initial_cases = 20, time_period = 30, n_rep = 1000,
                     si = si, R = R_values, n_days = 30)
dim(res_20)
## [1]     30 100000

## function to count cumulative number of cases
count_cumul_cases <- function(x) {
  out <- apply(x, 2, cumsum)
  class(out) <- c("cumul_cases", "matrix")
  out
}

## colors for quantiles
quantiles_pal <- colorRampPalette(c("#cc6666", "#4d1919"))

## function to plot outputs
plot.cumul_cases <- function(x, levels = c(.9, .95, .99), ...) {
  
  coords <- boxplot(t(x), col = "#862d59", xlab = "Days from onset of index case",
                    ylab = "Cumulative number of cases", outline = FALSE, range = Inf,
                    ...)
  
  abline(h = seq(100, 1e4, by = 100), col = "#BEBEBE80")
  quantiles <- apply(x, 1, quantile, levels)
  quantiles_col <- quantiles_pal(length(levels))
  for (i in 1:nrow(quantiles)) {
    points(1:ncol(quantiles), quantiles[i, ], type = "l", lty = 2, lwd = 2,
           col = quantiles_col[i])
  }
  
  legend("topleft", lty = 2, lwd = 2, col = quantiles_col,
         legend = paste(100*levels, "%"), title = "Quantiles", inset = .02)
}

## compute cumulative incidence
cumul_cases_20 <- count_cumul_cases(res_20)
dim(cumul_cases_20)
## [1]     30 100000

## plot results
par(mar = c(5.1, 4.1, 4.1, 0.1))

plot(cumul_cases_20, main = "Projected cumulative incidence")
mtext(side = 3, "Assuming 20 initial cases over 1 month")

res_40 <- make_simul(n_initial_cases = 40, time_period = 30, n_rep = 1000,
                     si = si, R = R_values, n_days = 30)

cumul_cases_40 <- count_cumul_cases(res_40)

plot(cumul_cases_40, main = "Projected cumulative incidence")
mtext(side = 3, "Assuming 40 initial cases over 1 month")









