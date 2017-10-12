#'
#' @title Extract model fits and load into \code{stanOutput} object class
#' 
#' @description This function will implement the \code{read_stan_mcmc} and \code{extract} functions to produce the primary \code{stanOutput} object class.
#' 
#' @param model character string a name or label for the model being extracted
#' @param data logical read data into object?
#' @param mcmc logical read mcmc chains into object?
#' @param parameters character vector model parameters are extracted as separate chains for diagnostic purposes
#' @param outputs character vector model outputs are extracted as a single permuted posterior sample (i.e. with chains combined) 
#' 
#' @include stanOutput-class.R read-stan-mcmc.R stan-extract-list.R
#' #@importFrom rstan extract read_stan_csv
#' #@importClassesFrom rstan stanfit
#' @export
#'
stan_extract <- function(parameters = NULL, outputs = NULL, model = character(), path = "./")
{
    #current.dir <- getwd()
    #setwd(path)
	
	# check that path ends with "/"
	stopifnot(identical(unlist(strsplit(path, ""))[nchar(path)], "/"))
    
    # get file names
	inifile   <- list.files(path, pattern = "[.]ini")[grepl(model, list.files(path, pattern = "[.]ini"))]
	datfile   <- list.files(path, pattern = "[.]dat")[grepl(model, list.files(path, pattern = "[.]dat"))]
	mcmcfiles <- list.files(path, pattern = "[.]mcmc")[grepl(model, list.files(path, pattern = "[.]mcmc"))]

	# if no parameters specified get complete list from .ini file
    if (is.null(parameters)) {
        
        if (length(inifile) > 0) {
		
			if (length(inifile) > 1) {
			
				stop("model name specifies >1 model\n") 
				
			} else {
            
				permute_FALSE <- names(rstan::read_rdump(paste0(path, inifile)))
			}
            
        } else {
            
            permute_FALSE <- NULL
        }
        
    } else {
    
        permute_FALSE <- unique(parameters)
    }
    
    if (is.null(outputs)) {
        
        permute_TRUE <- NULL
            
    } else {
        
        permute_TRUE <- unique(outputs)
    }
    
    # initialise object
    dS4 <- new("stanOutput", model.name = model, parameters = permute_FALSE, outputs = permute_TRUE)
    
    # assign initial values
    if (length(inifile) > 0) {
        
        dS4@inits <- rstan::read_rdump(paste0(path, inifile))
    }
    
    # get data	
	if (length(datfile) > 0) {
	
		if (length(datfile) > 1) {
		
			stop("model name specifies >1 model\n") 
			
		} else {
		
			dS4@data <- rstan::read_rdump(paste0(path, datfile))
			
		}
		
	} else warning("no 'dat' file")
    
    # get mcmc runs		
	if (length(mcmcfiles) > 0) {
		
		# create stanfit object from mcmc outputs
		#mcmc_stan <- rstan::read_stan_csv(paste0(path, mcmcfiles))
		
		mcmc <- read_stan_mcmc(paste0(path, mcmcfiles))
		
		# create list object containing model outputs
		if (!is.null(permute_TRUE)) {
		    
		    loc <- permute_TRUE %in% mcmc$pars_oi
		    
		    if (!all(loc)) {
		        
		        if (all(!loc)) stop("no 'outputs' in mcmc files\n") else warning(paste0("deleted [", paste(permute_TRUE[!loc], collapse = ", "), "] from 'outputs'\n"))
		        
		        dS4@outputs <- permute_TRUE <- permute_TRUE[loc]    
		        
		    }
			
		    #outputs_stan <- rstan::extract(mcmc_stan, pars = permute_TRUE, permuted = TRUE, inc_warmup = FALSE)
		    outputs <- extract(mcmc, pars = permute_TRUE, permuted = TRUE)
		    
		    #identical(mean(outputs_stan[[1]]), mean(outputs[[1]]))
		    #identical(mean(outputs_stan[[2]]), mean(outputs[[2]]))
		    
			dS4@mcmc[['outputs']] <- outputs
		}
		
		# extract pars (with chains) for diagnostic plots
		if (!is.null(permute_FALSE)) {
		    
		    loc <- permute_FALSE %in% mcmc$pars_oi
		    
		    if (!all(loc)) {
		        
		        if (all(!loc)) stop("no 'parameters' in mcmc files\n") else warning(paste0("deleted [", paste(permute_FALSE[!loc], collapse = ", "), "] from 'parameters'\n"))
		        
		        dS4@parameters <- permute_FALSE <- permute_FALSE[loc]    
		    }
			
		    #parameters_stan <- rstan::extract(mcmc_stan, pars = permute_FALSE, permuted = FALSE, inc_warmup = FALSE)
		    
		    # extract array
		    parameters <- extract(mcmc, pars = permute_FALSE, permuted = FALSE)
			
			# convert to list of arrays
			dS4@mcmc[['parameters']] <- lapply(permute_FALSE, function(x) parameters[,,regexpr(x, dimnames(parameters)[[3]]) > 0])
			names(dS4@mcmc[['parameters']]) <- permute_FALSE
		
		}
		
	} else warning("no 'mcmc' files")
    
    #setwd(current.dir)
    
    return(dS4)
}
