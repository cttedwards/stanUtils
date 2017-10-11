#' 
#' @title stanPosteriors object class
#'
#' @include stanPosterior-class.R
#' 
#' @export
setClass("stanPosteriors", contains = "list")
#'
#' @export
setMethod("initialize", signature = "stanPosteriors", definition = function(.Object, pars, models) {
    
    .Object@.Data <- vector("list", length(models))
    for (i in 1:length(models))
        .Object@.Data[[i]] <- new("stanPosterior", pars, models[i])
    names(.Object@.Data) <- models
    
    return(.Object)
})
#'
#' @export
setMethod("show", "stanPosteriors",
          function(object) {
              if (length(object) > 1) {
                message(c("stanPosterior object list for models: ", paste0(names(object), c(rep(', ', length(object) - 1), ''))))
              } else {
                message(c("stanPosterior object list for model: ", names(object)))
              }
          }
)
