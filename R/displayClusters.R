displayClusters <- function(W, group, xlabel = 'samples', ylabel = 'samples', main.title, ...) {
    # Visualizes the specified clusters in an affinity matrix 
    #
    # Args:
    #   W: Affinity matrix 
    #   group: labels for each row/column in W
    #
    # Returns:
    #   NULL - Plots the image
    
    col.arg <- list(...)
    
    
    normalize <- function(X){
        return( X / rowSums(X))
    }

    ind <- sort(as.vector(group),index.return=TRUE)
    ind <- ind$ix
    diag(W) <- 0
    W <- normalize(W)
    W <- W + t(W)
    
    W <- W *1000
    
    image(1:ncol(W),1:nrow(W),W[ind,ind],
          xlab = xlabel,ylab=ylabel,
          useRaster = TRUE,
          col = col.arg[[1]])
    title(main.title)
}
