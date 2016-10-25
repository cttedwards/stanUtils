#'
#' Extract posterior distributions for selected parameters
#' 
#' These functions can be used to extract a list of output values from a \code{stanOutput-class} object in the current workspace.
#' If supplied with a character vector will search for saved \code{*.rda} objects and load them. 
#' 
#' @include stanOutput-class.R stanPosterior-class.R stanPosteriors-class.R
#' 
#' @export
#  generic function
"posterior" <- function(x, ...) UseMethod("posterior")
#' @rdname posterior
#' @export
#  method
"posterior.stanOutput" <- function(object, pars) {
              
              parnames <- names(object@mcmc)
              
              if (missing(pars)) {
                  warning("no pars")
              }
              
              pars.in  <- pars[pars %in% parnames]
              pars.out <- pars[!(pars %in% parnames)]
              
              stan.list <- new("stanPosterior", pars.in, object@model)
              
              for (i in 1:length(pars.in)) {
                stan.list[[pars.in[i]]] <- object@mcmc[[pars.in[i]]]
              }
              
              if (length(pars.out) > 0) {
                  message("no ", paste(pars.out, collapse = ", "), " in ", deparse(substitute(object)))
              }
              
              return(stan.list)
          }
#' @rdname posterior
#' @export
#  method
"posterior.character" <- function(models, pars, path = rep(".", length(models)), ...) {
              
              stan.list <- new("stanPosteriors", pars, models)
              
              for (i in 1:length(models)) {
                  stan.object <- file.path(path[i], paste0(models[i], ".rds"))
                  if (file.exists(stan.object)) {
                        stan.model <- readRDS(stan.object)
                        message("loading model:", stan.model@model)
                        stan.list[[stan.model@model]] <- posterior(stan.model, pars)
                  }
              }
              message("done")
              
              return(stan.list)
          }
#}}}



