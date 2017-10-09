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
"traceplot.data.frame" <- function(object, pars = names(object)) {
    
	# check that data.frame was created from a flattened
	# stanPosterior object
	stopifnot("stan.data.frame" %in% class(object))
	
    message("plotting model outputs")
    
    dfr <- dfr %>% dplyr::filter(startsWith(as.character(label), pars))
    
    gg <- ggplot(dfr) + 
        geom_line(aes(as.integer(iterations), value)) + facet_wrap(~label, scales = "free_y") +
        labs(x = "Iteration", y = NULL)
    
    return(gg)
    
}

#' @rdname traceplot
#' @export
# method
"traceplot.stanPosterior" <- function(object, pars = names(object)) {
    
    message("plotting model outputs")
    
    dfr <- flatten(object)
    
    dfr <- dfr %>% dplyr::filter(startsWith(as.character(label), pars))
    
    gg <- ggplot(dfr) + 
        geom_line(aes(as.integer(iterations), value)) + facet_wrap(~label, scales = "free_y") +
        labs(x = "Iteration", y = NULL)
    
    return(gg)
    
}
#' @rdname traceplot
#' @export
# method
"traceplot.stanPosteriors" <- function(object, pars) {
    
    if (missing(pars)) {
		pars <- lapply(object, function(x) slot(x, 'pars'))
		pars <- Reduce(intersect, pars)
	}
    
    message("plotting comparative model outputs")
    
    dfr <- data.frame()
    for (i in 1:length(object))
        dfr <- rbind(dfr, data.frame(flatten(object[[i]]), model = names(object)[i]))
    
    dfr <- dfr %>% dplyr::filter(startsWith(as.character(label), pars))
    
    gg <- ggplot(dfr) + 
        geom_line(aes(as.integer(iterations), value, col = model)) + facet_wrap(~label, scales = "free_y") +
        labs(x = "Iteration", y = NULL, col = "Model")
    
    return(gg)
    
}
#' @rdname traceplot
#' @export
# method
"traceplot.stanOutput" <- function(object, pars = object@parameters) {
    
	stopifnot(pars %in% object@parameters)
	
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
        geom_line(aes(as.integer(iterations), value, col = as.factor(chains))) + facet_wrap(~parameters, scales = "free_y") +
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


