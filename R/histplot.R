#'
#' Density plot function
#' 
#' Plots posterior densities for \code{stanOutput} (all estimated parameters with chains), \code{stanPosterior} 
#' (all outputs in object) and \code{stanPosteriors} (all outputs from alternate models) objects.
#'
#' @include stanOutput-class.R stanPosterior-class.R flatten-stanPosterior.R
#' @export
# generic function
"histplot" <- function(x, ...) UseMethod("histplot")
#' @rdname histplot
#' @export
# method
"histplot.stanPosterior" <- function(object, pars = names(object), bins = 20) {
    
    message("plotting model outputs")
    
    dfr <- flatten(object)
    
    dfr <- dfr %>% dplyr::filter(startsWith(as.character(label), pars))
    
    gg <- ggplot(dfr) + 
        geom_histogram(aes(x = value), bins = bins) +
        facet_wrap(~label, scales = "free") +
        labs(x = "Value", y = NULL)
    
    return(gg)
    
}
#' @rdname histplot
#' @export
# method
"histplot.stanPosteriors" <- function(object, pars, bins = 20) {
    
    if (missing(pars)) stop("must specify pars")
        
    message("plotting comparative model outputs")
    
    dfr <- data.frame()
    for (i in 1:length(object))
        dfr <- rbind(dfr, data.frame(flatten(object[[i]]), model = names(object)[i]))
    
    dfr <- dfr %>% dplyr::filter(startsWith(as.character(label), pars))
    
    gg <- ggplot(dfr) + 
        geom_histogram(aes(x = value, fill = model), bins = bins) +
        facet_wrap(~label, scales = "free") +
        labs(x = "Value", y = NULL, fill = "Model")
    
    return(gg)
    
}
#' @rdname histplot
#' @export
# method
"histplot.stanOutput" <- function(object, pars = object@parameters, bins = 20) {
    
    message("plotting estimated parameters")
    
    mcmc <- object@mcmc[['parameters']]
    mcmc <- lapply(mcmc, melt)
    mcmc <- lapply(mcmc, function(x) {
        if(!("chains" %in% colnames(x))) x$chains <- 1
        if(!("iterations" %in% colnames(x))) x$iterations <- 1:nrow(x)
        x
    })
    
    dfr  <- plyr::ldply(mcmc)
    
    dfr$chains     <- as.character(dfr$chains)
    dfr$iterations <- as.integer(dfr$iterations)
    dfr$.id        <- as.character(dfr$.id)
    
    if("parameters" %in% colnames(dfr)) {
        dfr$parameters <- as.character(dfr$parameters)
        dfr$parameters[is.na(dfr$parameters)] <- dfr$.id[is.na(dfr$parameters)]
    } else {
        dfr$parameters <- dfr$.id
    }
    
    dfr <- dfr %>% dplyr::filter(startsWith(as.character(.id), pars))
    
    gg <- ggplot(dfr) + 
        geom_histogram(aes(x = value, fill = as.factor(chains)), bins = bins) +
        facet_wrap(~parameters, scales = "free") +
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


