# curves <- matrix(nrow = ncol(Teasel), ncol = ncol(Teasel), 0)
# curves[3, 1] <- curves[1, 6] <- -0.35
# curves[4, 6] <- curves[6, 4] <- curves[5, 6] <- curves[6, 5] <- 0.08
# curves[3, 6] <- 0.35
# plotmat(Teasel, pos = c(3, 2, 1), curve = curves,
#         name = colnames(Teasel), lwd = 1, box.lwd = 2,
#         cex.txt = 0.8, box.cex = 0.8, box.size = 0.08,
#         arr.length = 0.5, box.type = "circle", box.prop = 1,
#         shadow.size = 0.01, self.cex = 0.6, my = -0.075, mx = -0.01,
#         relsize = 0.9, self.shiftx = c(0, 0, 0.125, -0.12, 0.125, 0),
#         self.shifty = 0, main = "Teasel population model")
# 
# ####################################################################################################
# ####################################################################################################
# Numgenerations <- 6
# DiffMat <- matrix(data = 0, nrow = Numgenerations, ncol = Numgenerations)
# AA <- as.data.frame(DiffMat)
# AA[[1,4]] <- "f[3]"
# AA[[1,5]] <- "f[4]"
# AA[[1,6]] <- "f[5]"
# #
#   AA[[2,1]] <- "s[list(0,1)]"
# AA[[3,2]] <- "s[list(1,2)]"
# AA[[4,3]] <- "s[list(2,3)]"
# AA[[5,4]] <- "s[list(3,4)]"
# AA[[6,5]] <- "s[list(4,5)]"
# #
# name <- c(expression(Age[0]), expression(Age[1]), expression(Age[2]),
#           expression(Age[3]), expression(Age[4]), expression(Age[5]))
# #
# plotmat(A = AA, pos = c(1, 2, 2, 1), curve = 0.7, name = name, lwd = 2,
#         arr.len = 0.6, arr.width = 0.25, my = -0.2,
#         box.size = 0.05, arr.type = "triangle", dtext = 0.95,
#         main = "Age-structured population model 1")
# 
# ####################################################################################################
# ####################################################################################################
# N <- 7
# BB <- matrix(nrow = N, ncol = N, 1:N)
# diag(BB) <- 0
# 
# Col <- BB
# Col[] <- "black"
# Col[BB<10]<- "red"
# plotweb(BB, legend = TRUE, maxarrow = 3, arr.col = Col)
# par(mfrow = c(1, 1))
# 
####################################################################################################
####################################################################################################

ptsOnCirc <- function(x, r) {
  N <- length(x)
  if (N==1) {
    x <- rev(seq(x))
    N <- length(x)
    x <- c(x[N], x[-N])
  }
  cbind(r*cos(rad(360/N * x + (360/N/2))),
        r*sin(rad(360/N * x + (360/N/2))))
}

nams <- c("Problem", "Data", "Analysis", "Forecast", "Risk", "Decisions")

N <- 6
M  <- matrix(nrow = N, ncol = N, byrow = TRUE, data = 0)
curves  <- matrix(nrow = N, ncol = N, byrow = TRUE, data = 0)
col  <- matrix(nrow = N, ncol = N, byrow = TRUE, data = "black")

odd <- seq(1, N, by = 2)
even <- seq(2, N, by = 2)
a1 <- c(rev(even), odd )
a2 <- c(odd , rev(even))
a2 <- order(a2)

b <- ""
library(animation)
saveGIF({
for (m in 1:6) {
  vec <- 1:N
  v1 <- cbind(vec, c(vec[-1], vec[1]))
  M[v1] <- b
  v2 <- matrix(nrow = 0, ncol = 2)
  for (i in 1:m) {
    if (i > 1)
    v2 <- rbind(v2, cbind(i, seq(i-1, 1, by = -1)))
  }
  M[v2] <- "2"
  M[v1] <- "1"
  #curves[v2] <- 0.3
  #curves[v1] <- -0.1
  # diag(curves) <- 0
  diag(M) <- 0
  (M1 <- t(M)[a2,a2])
  (curves1 <- t(curves)[,a1])
  (curves[M1=="1"] <- -0.15)
  (curves[M1=="2"] <- -0.3)
  cols1 <- RColorBrewer::brewer.pal(N, "Pastel2")
  cols <- rep(rgb(0.2,0.5,0.5,0.9), 6)
  col <- matrix(rep(cols, each = N), ncol = 6)
  
  col <- col[,a2]
  #png(filename = paste0("plot", m, ".png"))
  pp <- plotmat(M1, pos = (ptsOnCirc(N, r = 0.4) + 0.5)[a2,], 
                curve = curves, name = nams, 
                box.lwd = 2, box.cex = 2, cex.txt = 0.8, 
                arr.lcol = col, arr.col = col, arr.pos = 0.55,
                box.size = nchar(nams)/N/10, box.col = cols1,
                arr.type = "curved", lwd = 3, box.type = "ellipse",
                box.prop = 1/nchar(nams)*2.5, #main = "plotmat", 
                arr.len = 0.5,
                segment.from = 0.2, segment.to = 0.8)
  #dev.off()
}
})