#'
#' Flatten stanPosterior object
#' 
#' Converts \code{stanPosterior-class} object into a labelled data.frame suitable for plotting or printing
#'
#' @include stanPosterior-class.R
#' @export
# generic function
"flatten" <- function(object, ...) UseMethod("flatten")
#' @rdname flatten
#' @export
# method
"flatten.stanPosterior" <- function(object) {
    
    lst <- list()
    
    # convert array to data.frame for
    # each parameter in the list
    for (i in 1:length(object)) {
        lst[[i]] <- flatten(object[[i]], names(object)[i])
    }
    
    # merge into single data.frame
    dfr <- plyr::ldply(lst)
    
	# add class attribute
	class(dfr) <- c("data.frame", "stan.data.frame")
	
    # return flattened data.frame
    return(dfr)
    
}
#' @rdname flatten
#' @export
# method
"flatten.array" <- function(object, par = character(), dim.map = list()) {
    
    # convert array to data.frame for
    #dfr <- plyr::adply(object, 1:length(dim(object)))
    #colnames(dfr)[match("V1", colnames(dfr))] <- 'value'
    #dfr[, 'par'] <- par
    
    # seems faster
    dfr <- reshape2::melt(object)
    #dfr[, 'par'] <- par
    
    # create label from parameter name and dimensions
    dimension.columns <- regexpr("Var", colnames(dfr)) > 0
    colnames(dfr)[dimension.columns] <- paste0('dim', 1:sum(dimension.columns))
    
    if (length(par) > 0) {
        dfr$label <- apply(dfr, 1, function(x) {
            dimensions <- x[dimension.columns]
            if (any(!is.na(dimensions))) {
                dimensions <- paste(dimensions[!is.na(dimensions)], collapse = ",")
                label <- paste0(par, "[", dimensions, "]")
            } else {
                label <- par
            }
            return(label)}
        )
    }
    
    if (length(dim.map) > 0) {
        loc <- which(dimension.columns)
        for (i in 1:length(loc))
            dfr[loc[i]] <- names(dim.map[[i]][dfr[,loc[i]]])
    }
    
    # tidy up
    #dfr <- dfr[, c('iterations', 'value', 'label')]
    dfr$iterations <- as.integer(dfr$iterations)
	
    # return flattened data.frame
    return(dfr)
    
}

#' @rdname flatten
#' @export
# method
"flatten.matrix" <- function(object, par = character(), dim.map = list()) {
    
    dfr <- flatten.array(object, par, dim.map)
    
    # return flattened data.frame
    return(dfr)
    
}