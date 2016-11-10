#'
#' Density plot function
#' 
#' Plots posterior densities for \code{stanOutput} (all estimated parameters with chains), \code{stanPosterior} 
#' (all parameters in object) and \code{stanPosteriors} (all parameters with alternate models) objects.
#'
#' @include stanOutput-class.R stanPosterior-class.R flatten-stanPosterior.R
#' @export
# generic function
"histplot" <- function(x, ...) UseMethod("histplot")
#' @rdname traceplot
#' @export
# method
"histplot.stanPosterior" <- function(object, bins = 20) {
    
    dfr <- flatten(object)
    
    gg <- ggplot(dfr) + 
        geom_histogram(aes(x = value), bins = bins) +
        facet_wrap(~label, scales = "free") +
        labs(x = "Value", y = NULL)
    
    return(gg)
    
}
#' @rdname traceplot
#' @export
# method
"histplot.stanPosteriors" <- function(object, bins = 20) {
    
    dfr <- data.frame()
    for (i in 1:length(object))
        dfr <- rbind(dfr, data.frame(flatten(object[[i]]), model = names(object)[i]))
    
    gg <- ggplot(dfr) + 
        geom_histogram(aes(x = value, fill = model), bins = bins) +
        facet_wrap(~label, scales = "free") +
        labs(x = "Value", y = NULL, fill = "Model")
    
    return(gg)
    
}
#' @rdname traceplot
#' @export
# method
"histplot.stanOutput" <- function(object, pars = object@parameters, bins = 20) {
    
    dfr <- object@mcmc[['permute_FALSE']]
    dfr <- dfr %>% dplyr::filter(startsWith(as.character(par), pars))
    
    gg <- ggplot(dfr) + 
        geom_histogram(aes(x = value, fill = chain), bins = bins) +
        facet_wrap(~par, scales = "free") +
        labs(x = "Value", y = NULL, fill = "Chain")
    
    return(gg)
}




#"histplot.stanOutput" <- function(object, bins = 20) {
#    
#    message("plotting estimated parameters")
#    
#    n_panel <- 12
#    sq <- seq(1, length(unique(object@mcmc_pars$par)), n_panel)
#    dd <- object@mcmc_pars %>%
#        dplyr::mutate(par = as.character(par))
#    
#    # MCMC histogram
#    for (i in 1:length(sq)) {
#        pq <- sq[i]:(sq[i] + n_panel - 1)
#        d <- dd %>%
#            dplyr::filter(par %in% unique(dd$par)[pq])
#        npar <- length(unique(d$par))
#        p <- ggplot(data = d, aes(x = value, fill = chain)) +
#            geom_histogram(aes(x = value), bins = bins) +
#            facet_wrap(~par, scales = "free", ncol = 2) +
#            labs(x = "Value", y = NULL, fill = "Chain")
#        ggsave(paste0(figure_dir, "par_histogram_", i, ".png"), p, width = ifelse(npar > 1, 7, 3.5), height = npar + (npar %% 2))
#    }
#    
#}


