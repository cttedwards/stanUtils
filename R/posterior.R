#'
#' Extract permuted posterior distributions for selected model outputs
#' 
#' These functions can be used to extract a list of output values from a \code{stanOutput-class} object in the current workspace.
#' If supplied with a character vector will search for saved \code{*.rds} objects and load them. 
#' 
#' @include stanOutput-class.R stanPosterior-class.R stanPosteriors-class.R
#' 
#' @export
#  generic function
"posterior" <- function(x, ...) UseMethod("posterior")
#' @rdname posterior
#' @export
#  method
"posterior.stanOutput" <- function(object, pars = object@outputs, model = object@model) {
              
              parnames <- names(object@mcmc[['outputs']])
              
              pars.in  <- pars[pars %in% parnames]
              pars.out <- pars[!(pars %in% parnames)]
              
              if (!(length(pars.in) > 0)) stop("specified pars not contained in model outputs")
              if (length(pars.out) > 0) warning("dropped: ", paste(pars.out, collapse = ', '))
              
              stan.list <- new("stanPosterior", pars.in, model)
              
              for (i in 1:length(pars.in)) {
                stan.list[[pars.in[i]]] <- object@mcmc[['outputs']][[pars.in[i]]]
              }
              
              return(stan.list)
}
#' @rdname posterior
#' @export
#  method
"posterior.list" <- function(object, pars = names(object), model) {
    
    stan.list <- new("stanPosterior", pars, model)
    
    for (i in 1:length(pars)) {
        stan.list[[pars[i]]] <- object[[pars[i]]]
    }
    
    return(stan.list)
}
#}}}



