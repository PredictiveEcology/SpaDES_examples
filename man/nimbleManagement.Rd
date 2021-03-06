% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Nimble Management.R
\name{nimbleManagement}
\alias{nimbleManagement}
\title{Nimble management visualization}
\usage{
nimbleManagement(
  N,
  allToAll = FALSE,
  levels = N,
  nams = c("Problem", "Data", "Models", "Analyses", "Forecast", "Assessment",
    "Decisions"),
  cols = "RdYlBu",
  box.scale = 10,
  ...
)
}
\arguments{
\item{...}{passed to \code{diagram::plotmat}}
}
\description{
Nimble management visualization
}
\examples{
if (require(animation) && require(quickPlot)) {
dev(width = 11, height = 11)
nimbleManagement(7, cols = "Set2", allToAll = TRUE)
saveGIF(movie.name = "Nimble Management.gif", 
        nimbleManagement(7, cols = "Set2", allToAll = TRUE),
        ani.options = ani.options(
          ani.height = 1200, 
          ani.width = 1200, ani.res = 200), {
            
          })
}

# Do one level of arrows at a time, to make incremental pngs, say
N <- 7
for (i in 0:N) {
  png(filename = file.path(".", paste0("nimble_level",i,".png")),
      width = 11, height = 11, units = "in", res = 200)
  nimbleManagement(N, cols = "Set2", allToAll = TRUE, levels = i)
  dev.off()
}

# More linear
N <- 7; nimbleManagement(7, cols = "Set2", allToAll = TRUE, 
                         pos = cbind(1:N/(N*1.1), N:1/(N*2) + 0.2), 
                         levels = 0, curve = -0.5, 
                         box.scale = 20, arr.pos = 0.5, 
                         segment.from = 0, segment.to = 1)
}
