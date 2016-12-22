#'
#' Extract permuted posterior sample from mcmc chain
#' 
#' This can be used for restarting another chain
#' 
#' @param object \code{stanOutput} class object containing MCMC samples
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
    
    return(lapply(mcmc, function(x) { d <- as.character(length(dim(x))); switch(d, "2" = x[dim(x)[1],], "3" = x[dim(x)[1],,])} ))
    
}
