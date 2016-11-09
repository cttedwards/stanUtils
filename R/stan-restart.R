#'
#' Extract permuted posterior sample from mcmc chain
#' 
#' This can be used for restarting another chain
#' 
#' @param path location of mcmc sample files
#' @importFrom lsd read_stan_map
#' @importClassesFrom rstan stanfit
#' @export
#'
stan_restart <- function(path = ".")
{
    current.dir <- getwd()
    setwd(path)
    
    # get estimated parameter and model output specifications
    parfile <- list.files(pattern = "[.]par")[grepl("*", list.files(pattern = "[.]par"))]
    pars    <- rstan::read_rdump(parfile)
    pars    <- pars[['estimated_parameters']]
    
    mcmcfiles <- list.files(pattern = "[.]mcmc")[grepl("*", list.files(pattern = "[.]mcmc"))]
    
    if (length(mcmcfiles) > 0) {
        
        # create stanfit object from mcmc outputs
        mcmc <- rstan::read_stan_csv(mcmcfiles)
        mcmc <- rstan::extract(mcmc, pars = pars, permuted = TRUE, inc_warmup = FALSE)
        
        terminal_sample <- function(x) { d <- length(dim(x)); switch(d, 
                                                        "1" = apply(as.matrix(x), 2, function(y) y[length(y)]),
                                                        "2" = apply(x, 2, function(y) y[length(y)]),
                                                        "3" = apply(x, 2:3, function(y) y[length(y)]),
                                                        "4" = apply(x, 2:4, function(y) y[length(y)]))}
        
        return(lapply(mcmc, terminal_sample))
        
    } else warning("no 'mcmc' files")
}
