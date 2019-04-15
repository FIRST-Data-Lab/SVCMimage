#' Produces coefficient function plots for a fitted "SVCMimage" object.
#'
#' This function produces the images of the estimated coefficient function for multivariate varying coefficient models.
#'
#' @importFrom Triangulation TriPlot
#'
#' @param mfit Fitted ``SVCMimage" object.
#' \cr
#' @return None
#'
#' @export
#'
plot.SVCMimage <- function(mfit){
  triplot <- TriPlot(mfit$V,mfit$Tr)
  readline(prompt="Press [enter] to continue")
  np <- ncol(mfit$X)
  Z <- matrix(mfit$Z,ncol=2)
  z1 <- unique(Z[,1]); z2 <- unique(Z[,2]);
  n1 <- length(z1); n2 <- length(z2);
  ind.inside <- mfit$ind.inside
  beta <- matrix(NA,n1*n2,np)
  beta[ind.inside,] <- as.matrix(mfit$beta)
  for(ii in 1:ncol(beta)){
    betai <- beta[,ii]
    betai.mtx <- matrix(betai,ncol=n1,nrow=n2)
    image(z2,z1,betai.mtx)
    contour(betai.mtx,add=TRUE,method="edge",vfont=c("sans serif","plain"))
    readline(prompt="Press [enter] to continue")
  }
}
