#'
#' stanOutput object class for storing outputs from a particular stan model run
#' 
#' This is the central class for the \code{stanUtils} package, from which all other functionality is derived. It contains
#' the data and outputs from a particular model run, plus the name of the model run, a character vector of parameters and a 
#' character vector of model outputs.
#' 
#' Estimated \code{parameters} are stored as separate MCMC chains which can be overlaid during calls to \code{traceplot} etc. and are
#' useful for checking model convergence. Model \code{outputs} contain permuted MCMC outputs (all chains combined). These can be used for comparing
#' different model runs, but are primarily intended to provide useable model outputs. Estimated parameters should contain all estimated 
#' parameters in the \code{parameters {}} block of the stan model. The specification of odel ouputs is not restrictive and 
#' can contain any combination of variables specified anywhere in the model.
#' 
#' When initialising a \code{stanOutput} object, parameters and outputs must be specified as input character vectors. When using
#' \code{stan_extract} they can also be provided as a \code{*.par} file.
#' 
#' @export
#'
setClass("stanOutput", slots = list(
						  parameters = "character",
						  outputs = "character",
                          data = "list",            # data object or list
                          map = "list",
                          mcmc = "list",
                          variational = "list",
                          inits = "list",           # initial values populated by initialisation function
                          model = "character"         # optional label for this particular run
                      )
         )

#' @export
#' 
setMethod("initialize", signature = "stanOutput", definition = function(.Object, model.name, parameters, outputs) {
    
	.Object@data <- list()
	.Object@map  <- list()
	.Object@mcmc <- list()
	.Object@mcmc[['parameters']] <- list()
	.Object@mcmc[['outputs']]    <- list()
	.Object@variational <- list()
	.Object@inits <- list()
	
	if (!missing(parameters)) {
        .Object@parameters <- as.character(parameters)
    } else .Object@parameters <- character()
	
	if (!missing(outputs)) {
	    .Object@outputs <- as.character(outputs)
	} else .Object@outputs <- character()
	
    if (!missing(model.name)) {
        .Object@model <- as.character(model.name)
    } else .Object@model <- character()
    
    return(.Object)
    
})

#' @export
#' 
setMethod("show", "stanOutput",
          function(object) {
            cat("stanOutput S4 object class for model '", object@model, "'" , sep = "") 
          })
