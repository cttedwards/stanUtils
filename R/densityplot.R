#'
#' Density plot function
#' 
#' Plots posterior densities for \code{stanOutput} (all estimated parameters with chains), \code{stanPosterior} 
#' (all parameters in object) and \code{stanPosteriors} (all parameters with alternate models) objects.
#'
#' @include stanOutput-class.R stanPosterior-class.R flatten-stanPosterior.R
#' @export
# generic function
"densityplot" <- function(x, ...) UseMethod("densityplot")
#' @rdname traceplot
#' @export
# method
"densityplot.stanPosterior" <- function(object) {
    
    dfr <- flatten(object)
    
    gg <- ggplot(dfr) + 
        geom_density(aes(x = value)) +
        facet_wrap(~label, scales = "free") +
        labs(x = "Value", y = NULL)
    
    return(gg)
    
}
#' @rdname traceplot
#' @export
# method
"densityplot.stanPosteriors" <- function(object) {
    
    dfr <- data.frame()
    for (i in 1:length(object))
        dfr <- rbind(dfr, data.frame(flatten(object[[i]]), model = names(object)[i]))
    
    gg <- ggplot(dfr) + 
        geom_density(aes(x = value, col = model)) +
        facet_wrap(~label, scales = "free") +
        labs(x = "Value", y = NULL, col = "Model")
    
    return(gg)
    
}
#' @rdname traceplot
#' @export
# method
"densityplot.stanOutput" <- function(object, figure_dir = "./") {
    
    message("plotting estimated parameters")
    
    n_panel <- 12
    sq <- seq(1, length(unique(object@mcmc_pars$par)), n_panel)
    dd <- object@mcmc_pars %>%
        dplyr::mutate(par = as.character(par))
    
    # MCMC density
    for (i in 1:length(sq)) {
        pq <- sq[i]:(sq[i] + n_panel - 1)
        d <- dd %>%
            dplyr::filter(par %in% unique(dd$par)[pq])
        npar <- length(unique(d$par))
        p <- ggplot(d) +
            geom_density(aes(x = value)) +
            facet_wrap(~par, scales = "free", ncol = 2) +
            labs(x = "Value", y = NULL)
        ggsave(paste0(figure_dir, "par_density_", i, ".png"), p, width = ifelse(npar > 1, 7, 3.5), height = npar + (npar %% 2))
    }
    
}


