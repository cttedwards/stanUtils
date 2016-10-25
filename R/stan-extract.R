#'
#' Extract model fits and load into stanOutput object class for plotting
#' 
#' @param model a name or label for the model being extracted
#' @include stanOutput-class.R
#' @importFrom lsd read_stan_map
#' @importClassesFrom rstan stanfit
#' @export
#'
stan_extract <- function(dir = ".", data = TRUE, map = FALSE, mcmc = FALSE, variational = FALSE, model = character(), pars = character())
{
    current.dir <- getwd()
    setwd(dir)
    
    # find initialisation file
    inifile <- list.files(pattern = "[.]ini")[grepl("*", list.files(pattern = "[.]ini"))]
    
    # by default select estimated pars for diagnostics
    permute_FALSE <- c("lp__", names(rstan::read_rdump(inifile)))
    
    # opportunity to specify output pars (estimated and derived)
    permute_TRUE <- NULL
    if (length(pars) > 0) {
        permute_TRUE <- pars
    }
    
    # initialise object
    dS4 <- new("stanOutput", model.name = model, pars = names(rstan::read_rdump(inifile)))
    
    if (data) {
        
        datfile <- list.files(pattern = "[.]dat")[grepl("*", list.files(pattern = "[.]dat"))]
        
        if (length(datfile) > 0) {
            dS4@data <- rstan::read_rdump(datfile)
        } else warning("no 'dat' file")
    }

    if (map) {
        
        mapfile <- list.files(pattern = "[.]map")[grepl("*", list.files(pattern = "[.]map"))]
        
        if (length(mapfile) > 0) {
            
            map <- lsd::read_stan_map(mapfile)
            
            if (!is.null(permute_TRUE))
                permute_TRUE <- map@model_pars
            
            dS4@map <- rstan::extract(map, pars = permute_TRUE, permuted = TRUE, inc_warmup = FALSE, include = FALSE)
            
        } else warning("no 'map' file")
    }
    
    if (mcmc) {
        
        mcmcfiles <- list.files(pattern = "[.]mcmc")[grepl("*", list.files(pattern = "[.]mcmc"))]
        
        if (length(mcmcfiles) > 0) {
            
            # create stanfit object from mcmc outputs
            mcmc <- rstan::read_stan_csv(mcmcfiles)
            
            if (!is.null(permute_TRUE))
                permute_TRUE <- mcmc@model_pars
            
            # create list object containing all model outputs
            dS4@mcmc[['permute_TRUE']] <- rstan::extract(mcmc, pars = permute_TRUE, permuted = TRUE, inc_warmup = FALSE, include = FALSE)
            
            # extract pars (with chains) for diagnostic plots
            mcmc <- rstan::extract(mcmc, pars = permute_FALSE, permuted = FALSE, inc_warmup = FALSE, include = TRUE)
            dimnames(mcmc)[[1]] <- 1:dim(mcmc)[1]
            dimnames(mcmc)[[2]] <- 1:dim(mcmc)[2]
            mcmc <- plyr::adply(mcmc, 1:3)
            colnames(mcmc) <- c('iteration', 'chain', 'par', 'value')
            dS4@mcmc[['permute_FALSE']] <- mcmc
            
        } else warning("no 'mcmc' files")
    }
    
    if (variational) {
        
        varfiles <- list.files(pattern = "[.]var")[grepl("*", list.files(pattern = "[.]var"))]
        
        if (length(varfiles) > 0) {
            
            warning("'var' files cannot be read in yet!")
            #vari <- lsd::read_stan_var(varfiles) # not yet working
            #dS4@variational <- rstan::extract(vari, pars = dont_extract, permuted = TRUE, inc_warmup = FALSE, include = FALSE)
        } else warning("no 'var' files")
    }
    
    setwd(current.dir)
    
    return(dS4)
}
