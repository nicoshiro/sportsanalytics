library("TDA")
X <- circleUnif(400)
Xlim <- c(-1.6, 1.6); Ylim <- c(-1.7, 1.7); by <- 0.065
Xseq <- seq(Xlim[1], Xlim[2], by = by)
Yseq <- seq(Ylim[1], Ylim[2], by = by)
Grid <- expand.grid(Xseq, Yseq)

distance <- distFct(X = X, Grid = Grid)

m0 <- 0.1
DTM <- dtm(X = X, Grid = Grid, m0 = m0)

k <- 60
kNN <- knnDE(X = X, Grid = Grid, k = k)

h <- 0.3
KDE <- kde(X = X, Grid = Grid, h = h)

Kdist <- kernelDist(X = X, Grid = Grid, h = h)


persp(Xseq, Yseq, matrix(KDE, ncol = length(Yseq), nrow = length(Xseq)), xlab = "", ylab = "", zlab = "", theta = -20, phi = 35, ltheta = 50, col = 2, border = NA, main = "KDE", d = 0.5, scale = FALSE, expand = 3, shade = 0.9)

persp(Xseq, Yseq, matrix(kNN, ncol = length(Yseq), nrow = length(Xseq)), xlab = "", ylab = "", zlab = "", theta = -20, phi = 35, ltheta = 50, col = 2, border = NA, main = "KNN", d = 0.5, scale = FALSE, expand = 3, shade = 0.9)

band <- bootstrapBand(X = X, FUN = kde, Grid = Grid, B = 100,parallel = FALSE, alpha = 0.1, h = h)


DiagGrid <- gridDiag(X = X, FUN = kde, h = 0.3, lim = cbind(Xlim, Ylim), by = by, sublevel = FALSE, library = "Dionysus", location = TRUE, printProgress = FALSE)
plot(DiagGrid[["diagram"]], band = 2 * band[["width"]], main = "KDE Diagram")


par(mfrow = c(1, 2), mai = c(0.8, 0.8, 0.3, 0.1))
plot(DiagGrid[["diagram"]], rotated = TRUE, band = band[["width"]], main = "Rotated Diagram")
plot(DiagGrid[["diagram"]], barcode = TRUE, main = "Barcode")


Circle1 <- circleUnif(60)
Circle2 <- circleUnif(60, r = 2) + 3
Circles <- rbind(Circle1, Circle2)

maxscale <- 5 # limit of the filtration
maxdimension <- 1 # components and loops

DiagRips <- ripsDiag(X = Circles, maxdimension, maxscale, library = c("GUDHI", "Dionysus"), location = TRUE, printProgress = FALSE)
plot(DiagRips[["diagram"]])
