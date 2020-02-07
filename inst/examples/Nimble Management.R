reproducible::Require(c("diagram", "CircStats"))

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
N <- 7


#nams <- unlist(lapply(LETTERS[1:N][a2], function(x) paste0(rep(x, 3), collapse = "")))

library(animation)


nimbleManagement(7, cols = "BrBG", allToAll = TRUE)
saveGIF(movie.name = "Nimble Management.gif", 
        nimbleManagement(7, cols = "Accent", allToAll = TRUE),
        ani.options = ani.options(
          ani.height = 1200, 
          ani.width = 1200, ani.res = 200), {

})
