#'
#' Extract permuted posterior distributions for selected parameters
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
"posterior.stanOutput" <- function(object, pars = unique(c(object@parameters, object@outputs))) {
              
              parnames <- names(object@mcmc[['permute_TRUE']])
              
              pars.in  <- pars[pars %in% parnames]
              pars.out <- pars[!(pars %in% parnames)]
              
              stan.list <- new("stanPosterior", pars.in, object@model)
              
              for (i in 1:length(pars.in)) {
                stan.list[[pars.in[i]]] <- object@mcmc[['permute_TRUE']][[pars.in[i]]]
              }
              
              if (length(pars.out) > 0) {
                  message("no ", paste(pars.out, collapse = ", "), " in '", deparse(substitute(object)), "' model_outputs (see *.par file)")
              }
              
              return(stan.list)
          }
#' @rdname posterior
#' @export
#  method
"posterior.character" <- function(models, pars, path = rep(".", length(models)), ...) {
    
                if (missing(pars)) {
                    
                    parfiles <- list.files(pattern = "[.]par")
                    parfiles <- parfiles[apply(vapply(models, function(x) grepl(x, parfiles), vector('logical', length(parfiles))), 2, sum)]
                    pars    <- vapply(parfiles, function(x) rstan::read_rdump(x), vector('list', 2))
                    pars    <- c("lp__", unique(unlist(pars)))
                    
                }
                
                stan.list <- new("stanPosteriors", pars, models)
                
                for (i in 1:length(models)) {
                  stan.object <- file.path(path[i], paste0(models[i], ".rds"))
                  if (file.exists(stan.object)) {
                        stan.model <- readRDS(stan.object)
                        message("loading model: '", stan.model@model,"'")
                        stan.list[[stan.model@model]] <- posterior(stan.model, pars)
                  }
                }
                
                message("done")
                
                return(stan.list)
          }
#}}}



