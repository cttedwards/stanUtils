#'
#' @title stanPosterior object class
#' @description Contains MCMC samples from a subset of parameters extracted from an \code{stanOutput} object.
#'
#' @export
setClass("stanPosterior", contains = "list", slots = list(pars = "character", model = "character"))
#'
#' @export
setMethod("initialize", signature = "stanPosterior", definition = function(.Object, pars, model) {
    
    if (!missing(pars)) {
        .Object@.Data <- vector("list", length(pars))
        names(.Object@.Data) <- pars
        .Object@pars <- pars
    }
    
    if (!missing(model)) {
        .Object@model <- as.character(model)
    }
    
    return(.Object)
    
})
#'
#' @export
setMethod("show", "stanPosterior",
          function(object) {
              cat("stanPosterior for model: '", object@model, "' , containing pars: ", paste(object@pars, collapse = ', '))
          }
)

