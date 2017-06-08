#'
#' Extract terminal posterior sample from mcmc chain(s)
#' 
#' This can be used for restarting another chain
#' 
#' @param object \code{stanOutput} class object containing MCMC samples stored in \code{object@mcmc[['parameters']]}.
#' 
#' @importClassesFrom rstan stanfit
#' 
#' @export
# generic function
"restart" <- function(object, ...) UseMethod("restart")
#' @export
"restart.stanOutput" <- function(object)
{
    
    mcmc <- object@mcmc[['parameters']]
    
    return(lapply(mcmc, function(x) { d <- as.character(length(dim(x))); switch(d, "0" = x[length(x)], "2" = x[dim(x)[1],], "3" = x[dim(x)[1],,])} ))
    
}
