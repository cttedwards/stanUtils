#'
#' Extract model fits and load into stanOutput object class for plotting
#' 
#' @param model a name or label for the model being extracted
#' @include stanOutput-class.R
#' @importFrom lsd read_stan_map
#' @importClassesFrom rstan stanfie
#' @export
#'
stan_extract <- function(dir = ".", data = TRUE, map = FALSE, mcmc = FALSE, variational = FALSE, model = character(), pars = character())
{
    current.dir <- getwd()
    setwd(dir)
    
    # model outputs that we don't want
    permute_TRUE <- c("proj_numbers_ytrsl","data_lf_all_isl","resid_lf_i","par_M_r","par_grow_ip",
                      "vuln_selectivity_ytrsl")
    
    
    
    # find initialisation file
    inifile <- list.files(pattern = "[.]ini")[grepl("*", list.files(pattern = "[.]ini"))]
    
    # by default select estimated pars for diagnostics
    permute_FALSE <- c("lp__", names(rstan::read_rdump(inifile)))
    
    # initialise object
    dS4 <- new("stanOutput", model.name = model, pars = names(rstan::read_rdump(inifile)))
    
    # list all output pars (estimated and derived)
    if (length(pars) > 0) {
        
    } else {
        
        if (mcmc)  
    }
    
    
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
            dS4@map <- rstan::extract(map, pars = dont_extract, permuted = TRUE, inc_warmup = FALSE, include = FALSE)
            
            # trim estimated pars
            i <- apply(as.matrix(do_extract), 1, FUN = function(x) {any(grepl(x, names(map)))})
            do_extract_map <- do_extract[i]
            
            # write to csv
            map_tmp <- rstan::extract(map, pars = do_extract_map, permuted = FALSE, inc_warmup = FALSE, include = TRUE)
            map_tmp <- plyr::adply(map_tmp, 3)
            colnames(map_tmp) <- c("par", "value")
            write.csv(map_tmp, file = "key_parameters_map.csv", quote = FALSE, row.names = FALSE)
        } else warning("no 'map' file")
    }
    
    if (mcmc) {
        mcmcfiles <- list.files(pattern = "[.]mcmc")[grepl("*", list.files(pattern = "[.]mcmc"))]
        if (length(mcmcfiles) > 0) {
            
            # create stanfit object from mcmc outputs
            mcmc_raw <- rstan::read_stan_csv(mcmcfiles)
            
            # create list object containing all model outputs
            dS4@mcmc <- rstan::extract(mcmc_raw, pars = dont_extract, permuted = TRUE, inc_warmup = FALSE, include = FALSE)
            
            # trim estimated pars
            i <- apply(as.matrix(do_extract), 1, FUN = function(x) {any(grepl(x, names(mcmc_raw)))})
            do_extract_mcmc <- do_extract[i]
            
            # extract pars (with chains) for diagnostic plots
            mcmc_tmp <- rstan::extract(mcmc_raw, pars = do_extract_mcmc, permuted = FALSE, inc_warmup = FALSE, include = TRUE)
            dimnames(mcmc_tmp)[[1]] <- 1:dim(mcmc_tmp)[1]
            dimnames(mcmc_tmp)[[2]] <- 1:dim(mcmc_tmp)[2]
            mcmc_tmp <- plyr::adply(mcmc_tmp, 1:3)
            colnames(mcmc_tmp) <- c('iteration', 'chain', 'par', 'value')
            dS4@mcmc_pars <- mcmc_tmp

            # write to csv
            mcmc_out <- mcmc_tmp %>%
                tidyr::spread(par, value)
            write.csv(mcmc_out, file = "key_parameters_mcmc.csv", quote = FALSE, row.names = FALSE)
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
