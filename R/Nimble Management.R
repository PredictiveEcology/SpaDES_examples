
#' @importFrom CircStats rad
#' @importFrom fpCompare %==%
ptsOnCirc <- function(x, r = 1) {
  N <- length(x)
  if (N==1) {
    x <- rev(seq(x))
    N <- length(x)
    x <- c(x[N], x[-N])
  }
  for (i in 1:2) {
    aa <- if (N %% 2 == 0) {
      cbind(r*cos(rad(360/N * x + (360/N/2))),
            r*sin(rad(360/N * x + (360/N/2))))
    } else {
      cbind(r*sin(rad(360/N * x + (360/N/2)  )),
            -r*cos(rad(360/N * x + (360/N/2)  ))); 
    }
    addToX <- N - which(aa[,2] %==% r) + 1
    x <- x + addToX
  }
  return(aa)
  
}

#' Nimble management visualization
#' @importFrom diagram plotmat
#' @rdname nimbleManagement
#' @export
#' @param ... passed to \code{diagram::plotmat}
#' @examples 
#' if (require(animation) && require(quickPlot)) {
#' dev(width = 11, height = 11)
#' nimbleManagement(7, cols = "Set2", allToAll = TRUE)
#' saveGIF(movie.name = "Nimble Management.gif", 
#'         nimbleManagement(7, cols = "Set2", allToAll = TRUE),
#'         ani.options = ani.options(
#'           ani.height = 1200, 
#'           ani.width = 1200, ani.res = 200), {
#'             
#'           })
#' }
#' 
#' # Do one level of arrows at a time, to make incremental pngs, say
#' N <- 7
#' for (i in 0:N) {
#'   png(filename = file.path(".", paste0("nimble_level",i,".png")),
#'       width = 11, height = 11, units = "in", res = 200)
#'   nimbleManagement(N, cols = "Set2", allToAll = TRUE, levels = i)
#'   dev.off()
#' }
#' 
#' # More linear
#' N <- 7; nimbleManagement(7, cols = "Set2", allToAll = TRUE, 
#'                          pos = cbind(1:N/(N*1.1), N:1/(N*2) + 0.2), 
#'                          levels = 0, curve = -0.5, 
#'                          box.scale = 20, arr.pos = 0.5, 
#'                          segment.from = 0, segment.to = 1)
nimbleManagement <- function(N, allToAll = FALSE, levels = N,
                             nams = c("Problem", "Data", "Models", "Analyses", "Forecast", "Assessment", "Decisions"),
                             cols = "RdYlBu", box.scale = 10, ...) {
  if (isTRUE(cols %in% rownames(RColorBrewer::brewer.pal.info)))
    cols <- RColorBrewer::brewer.pal(N, name = cols)
  cols1 <- cols
  rm(cols)
  odd <- seq(1, N, by = 2)
  even <- seq(2, N, by = 2)
  a1 <- c(rev(even), odd )
  dots <- list(...)
  if (is.null(dots$pos)) {
    a2 <- c(odd , rev(even))
    a2 <- order(a2)
  } else {
    a2 <- seq(1, N)
  }
  if (length(nams) != N)
    nams <- unlist(lapply(LETTERS[1:N][a2], 
                          function(x) paste0(rep(x, 3), 
                                             collapse = "")))
  nams <- nams[a2]
  
  M  <- matrix(nrow = N, ncol = N, byrow = TRUE, data = 0)
  MOrig <- M
  curves  <- matrix(nrow = N, ncol = N, byrow = TRUE, data = 0)
  col  <- matrix(nrow = N, ncol = N, byrow = TRUE, data = "black")
  
  b <- ""
  doAllToAll <- isTRUE(allToAll)
  for (m in ifelse(doAllToAll, 0, 1):levels) {
    vec <- 1:N
    v1 <- cbind(vec, c(vec[-1], vec[1]))
    M[v1] <- b
    v2 <- matrix(nrow = 0, ncol = 2)
    if (m > 0) {
      for (i in 1:m) {
        if (i > ifelse(doAllToAll,0,1)) {
          lowBound <- if (doAllToAll) N else i - 1
          v2 <- rbind(v2, cbind(i, seq(lowBound, 1, by = -1)))
        }
      }
    }
    MOrig[v2] <- "2"
    MOrig[v1] <- "1"
    M[v2] <- ""
    M[v1] <- ""
    #curves[v2] <- 0.3
    #curves[v1] <- -0.1
    # diag(curves) <- 0
    diag(M) <- 0
    (M1 <- t(M)[a2,a2])
    (M1Orig <- t(MOrig)[a2,a2])
    (curves1 <- t(curves)[,a1])
    (curves[M1Orig=="1"] <- -0.15)
    (curves[M1Orig=="2"] <- -0.25)
    #cols1 <- RColorBrewer::brewer.pal(N, "RdYlBu")
    cols <- rep(rgb(0.2,0.5,0.5,0.9), N)
    col <- matrix(rep(cols, each = N), ncol = N)
    col[] <- rep(cols1, each = N)
    col <- col[,a2]
    
    #png(filename = paste0("plot", m, ".png"))
    defaults <- list(pos = (ptsOnCirc(N, r = 0.3) + 0.5)[a2,], 
                     curve = curves, name = nams, 
                     box.lwd = 2, box.cex = 1, cex.txt = 0.6, 
                     arr.lcol = col, arr.col = col, arr.pos = 0.7,
                     box.size = nchar(nams)/N/box.scale, box.col = cols1[a2],
                     arr.type = "curved", lwd = 3, box.type = "ellipse",
                     box.prop = 1/nchar(nams)*box.scale/(box.scale^0.65), #main = "plotmat", 
                     arr.len = 0.8,
                     segment.from = 0.2, segment.to = 0.8)
    defaults[names(dots)] <- dots
    do.call(plotmat, append(list(M1), defaults))
    #dev.off()
  }
}

