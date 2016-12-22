#'
#' Extract model fits and load into stanOutput object class
#' 
#' @param model a name or label for the model being extracted
#' @param data read data into object?
#' @param mcmc read mcmc chains into object?
#' @param map read map results into object?
#' @param map read variational results into object?
#' 
#' @include stanOutput-class.R
#' @importFrom rstan extract read_stan_csv
#' @importClassesFrom rstan stanfit
#' @export
#'
stan_extract <- function(data = TRUE, map = FALSE, mcmc = FALSE, variational = FALSE, parameters = character(), outputs = character(), model = character(), mcmc_files = "all", path = ".")
{
    current.dir <- getwd()
    setwd(path)
    
    # get estimated parameter and model output specifications
    if (length(parameters) > 0) {
        
        permute_FALSE <- parameters
        
        if (length(outputs) > 0) {
            permute_TRUE <- outputs
        } else permute_TRUE <- NULL
    } else {
    
        parfile <- list.files(pattern = "[.]par")[grepl(model, list.files(pattern = "[.]par"))]
        
        if (length(parfile) > 0) {
            
            pars <- rstan::read_rdump(parfile)
            
            permute_FALSE <- unique(pars[['parameters']])
            permute_TRUE  <- unique(pars[['outputs']])
            
        } else stop("no 'par' file")
    }
    
    # initialise object
    dS4 <- new("stanOutput", model.name = model, parameters = permute_FALSE, outputs = permute_TRUE)
    
    if (data) {
        
        datfile <- list.files(pattern = "[.]dat")[grepl(model, list.files(pattern = "[.]dat"))]
        
        if (length(datfile) > 0) {
            
            dS4@data <- rstan::read_rdump(datfile)
            
        } else warning("no 'dat' file")
    }

    if (map) {
        
        mapfiles <- list.files(pattern = "[.]map")[grepl(model, list.files(pattern = "[.]map"))]
        
        if (length(mapfiles) > 0) {
            
            map <- read_stan_map(mapfiles)
                
            dS4@map[['parameters']] <- lapply(permute_FALSE, function(x) return(map$sims[[x]]))
            dS4@map[['outputs']]    <- lapply(permute_TRUE,  function(x) return(map$sims[[x]]))
            
            names(dS4@map[['parameters']]) <- permute_FALSE
            names(dS4@map[['outputs']]) <- permute_TRUE
            
        } else warning("no 'map' file")
    }
    
    if (mcmc) {
        
        mcmcfiles <- list.files(pattern = "[.]mcmc")[grepl(model, list.files(pattern = "[.]mcmc"))]
        
        if (!all(mcmc_files == "all")) {
            mcmcfiles <- mcmcfiles[mcmcfiles %in% mcmc_files]
        } 
        
        if (length(mcmcfiles) > 0) {
            
            # create stanfit object from mcmc outputs
            mcmc <- rstan::read_stan_csv(mcmcfiles)
            
            # create list object containing model outputs
            if (!is.null(permute_TRUE))
                dS4@mcmc[['outputs']] <- rstan::extract(mcmc, pars = permute_TRUE, permuted = TRUE, inc_warmup = FALSE)
            
            # extract pars (with chains) for diagnostic plots
            mcmc <- rstan::extract(mcmc, pars = permute_FALSE, permuted = FALSE, inc_warmup = FALSE)
            dimnames(mcmc)[[1]] <- 1:dim(mcmc)[1]
            if (length(mcmcfiles) > 1) dimnames(mcmc)[[2]] <- 1:dim(mcmc)[2]
            
            dS4@mcmc[['parameters']] <- lapply(permute_FALSE, function(x) mcmc[,,regexpr(x, dimnames(mcmc)[[3]]) > 0])
            names(dS4@mcmc[['parameters']]) <- permute_FALSE
            
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
