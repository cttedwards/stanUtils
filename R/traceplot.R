#'
#' Traceplot function
#' 
#' Plots traces for \code{stanOutput} (all estimated parameters with chains), \code{stanPosterior} 
#' (all parameters in object) and \code{stanPosteriors} (all parameters with alternate models) objects.
#'
#' @include stanOutput-class.R stanPosterior-class.R flatten-stanPosterior.R
#' @export
# generic function
"traceplot" <- function(x, ...) UseMethod("traceplot")
#' @rdname traceplot
#' @export
# method
"traceplot.stanPosterior" <- function(object) {
    
    dfr <- flatten(object)
    
    gg <- ggplot(dfr) + 
        geom_line(aes(as.integer(iterations), value)) + facet_wrap(~label, scales = "free_y") +
        labs(x = "Iteration", y = NULL)
    
    return(gg)
    
}
#' @rdname traceplot
#' @export
# method
"traceplot.stanPosteriors" <- function(object) {
    
    dfr <- data.frame()
    for (i in 1:length(object))
        dfr <- rbind(dfr, data.frame(flatten(object[[i]]), model = names(object)[i]))
    
    gg <- ggplot(dfr) + 
        geom_line(aes(as.integer(iterations), value, col = model)) + facet_wrap(~label, scales = "free_y") +
        labs(x = "Iteration", y = NULL, col = "Model")
    
    return(gg)
    
}
#' @rdname traceplot
#' @export
# method
"traceplot.stanOutput" <- function(object, pars = object@parameters) {
    
    dfr <- object@mcmc[['permute_FALSE']]
    dfr <- dfr %>% dplyr::filter(startsWith(as.character(par), pars))
    
    gg <- ggplot(dfr) + 
        geom_line(aes(as.integer(iteration), value, col = chain)) + facet_wrap(~par, scales = "free_y") +
        labs(x = "Iteration", y = NULL, col = "Chain")
    
    return(gg)
}


#"traceplot.stanOutput" <- function(object, figure_dir = "./") {
#    
#    message("plotting estimated parameters")
#    
#    n_panel <- 12
#    sq <- seq(1, length(unique(object@mcmc_pars$par)), n_panel)
#    dd <- object@mcmc_pars %>%
#        dplyr::mutate(par = as.character(par))
#    
#    # MCMC trace plot
#    for (i in 1:length(sq)) {
#        pq <- sq[i]:(sq[i] + n_panel - 1)
#        d <- dd %>%
#            dplyr::filter(par %in% unique(dd$par)[pq])
#        npar <- length(unique(d$par))
#        p <- ggplot(d) +
#            geom_line(aes(x = as.integer(iteration), y = value, col = chain)) +
#            facet_wrap(~par, scales = "free_y", ncol = 2) +
#            labs(x = "Iteration", y = NULL, col = "Chain")
#        ggsave(paste0(figure_dir, "par_trace_", i, ".png"), p, width = ifelse(npar > 1, 7, 3.5), height = npar + (npar %% 2))
#    }
#}


