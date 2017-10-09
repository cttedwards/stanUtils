#'
#' @include stanOutput-class.R stanPosterior-class.R stanPosteriors-class.R posterior.R
#' 
#' @export
#  generic function
"posteriors" <- function(x, ...) UseMethod("posteriors")
#' @rdname posterior
#' @export
#  method
"posteriors.stanPosterior" <- function(object, ...) {
    
    object.list <- list(object, ...)
    
    model.names <- unlist(lapply(object.list, function(x) slot(x, 'model')))
    
    stan.list <- new("stanPosteriors", models = model.names)
        
    pars <- lapply(object.list, function(x) slot(x, 'pars'))
        
    pars.common <- Reduce(intersect, pars)
    
    for (i in 1:length(object.list))
        stan.list[[i]] <- posterior(object.list[[i]], pars = pars.common, model = model.names[i])
    
    #if (common.pars) {
    #    
    #    pars.list <- lapply(stan.list, function(x) slot(x, 'pars'))
    #    
    #    pars.list.common <- Reduce(intersect, pars.list)
    #    
    #    stan.list <- lapply(stan.list, "[", pars.list.common)
    #
    #}
    
    return(stan.list)
}
#' @rdname posterior
#' @export
#  method
"posteriors.character" <- function(models, pars, path = rep(".", length(models)), ...) {
    
    if (missing(pars)) {
        
        stop("must specify pars")
        
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



