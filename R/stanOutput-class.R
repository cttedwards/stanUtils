#'
#' stanOutput object class for storing outputs from a particular stan model run
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
setMethod("initialize", signature = "stanOutput", definition = function(.Object, model.name, pars, outputs) {
    
	.Object@data <- list()
	.Object@map  <- list()
	.Object@mcmc <- list()
	.Object@mcmc[['permute_FALSE']] <- data.frame()
	.Object@mcmc[['permute_TRUE']]  <- list()
	.Object@variational <- list()
	.Object@inits <- list()
	
	if (!missing(pars)) {
        .Object@parameters <- as.character(pars)
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
            cat("stanOutput S4 object class for model '", object@model, "'" ,sep = '') 
          })
