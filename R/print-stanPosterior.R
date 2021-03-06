#'
#' @title Print functions for stanPosterior objects
#' 
#' @description Print table of MCMC model estimates as a median plus 95% credibility intervals. 
#' 
#' @include stanPosterior-class.R flatten-stanPosterior.R utils.R
#' @importFrom pander pandoc.table pandoc.table.return Pandoc
#' @export
#  method
"print.stanPosterior" <- function(object, write.to.word = FALSE) {
    
    dfr <- flatten(object)

    dfr <- plyr::ddply(dfr, "label", plyr::summarize, value = .quantile(value, .dpselect(unique(label))))
    
    if (write.to.word) {
        
        tab <- Pandoc$new(paste0('stan model: ', object@model), 'Summary outputs')
        tab$add(pandoc.table.return(dfr, style = 'simple', justify = c('left', 'right')))
        tab$format <- 'docx'
        tab$export(paste0('stan_', object@model, '_summary_table'))
        
    } else pandoc.table(dfr, style = 'simple', justify = c('left', 'right'))
}
#'
#' @export
#  method
"print.stanPosteriors" <- function(object, write.to.word = FALSE) {
    
    #requireNamespace("pander", quietly = TRUE)
    
    #if (length(object) > 1) {
        
    # flatten and extract quantiles
    object <- lapply(object, flatten)
    for (i in 1:length(object)) {
        object[[i]] <- plyr::ddply(object[[i]], "label", plyr::summarize, value = .quantile(value, .dpselect(unique(label))))
        colnames(object[[i]])[match("value", colnames(object[[i]]))] <- paste(c(names(object)[i], "value"), collapse = ".")
    }
    
    # merge
    dfr <- object[[1]]
    if (length(object) > 1) 
        for (i in 2:length(object)) 
            dfr <- merge(dfr, object[[i]], all = TRUE)
    
        
    #} else {
    #    
    #    dfr <- flatten(object[[1]])
    #    dfr <- plyr::ddply(dfr, "label", plyr::summarize, value = .quantile(value, .dpselect(unique(label))))
    #    
    #}
    
    if (write.to.word) {
        
        tab <- Pandoc$new(paste0('stan models: ', paste(names(object), collapse = ', ')),'Summary outputs')
        tab$add(pandoc.table.return(dfr, style = 'simple', justify = c('left', rep('right', length(object)))))
        tab$format <- 'docx'
        tab$export('stan_comparative_summary_table')
        
    } else pandoc.table(dfr, style = 'simple', justify = c('left', rep('right', length(object))))
}
