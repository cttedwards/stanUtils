## ---- echo = FALSE-------------------------------------------------------
knitr::opts_chunk$set(tidy = TRUE, tidy.opts = list(blank = TRUE, width.cutoff = 75), message = FALSE, warning = FALSE, collapse = TRUE, comment = "# >")

## ---- results='hide'-----------------------------------------------------
library(stanUtils)

## ------------------------------------------------------------------------
# write example models
stan_codeA <- "data {int N; vector[N] y;} parameters {real mu;} model {y ~ normal(mu,1);} generated quantities {real mse; mse = mean((y - mu) .* (y - mu));}"
stan_codeB <- "data {int N; vector[N] y;} parameters {real mu; real<lower=0> sigma;} model {y ~ normal(mu,sigma);} generated quantities {real mse; mse = mean((y - mu) .* (y - mu));}"

cat(stan_codeA, file = "modelA.stan")
cat(stan_codeB, file = "modelB.stan")

# write *.ini file
inis <- list(mu = 10)
rstan::stan_rdump(list = ls(inis), file = "modelA.ini",  envir = list2env(inis))

inis <- list(mu = 10, sigma = 1)
rstan::stan_rdump(list = ls(inis), file = "modelB.ini",  envir = list2env(inis))

# write *.dat file
dat <- list(N = 100, y = rnorm(100, 10, 1))
rstan::stan_rdump(list = ls(dat), file = "modelA.dat",  envir = list2env(dat))
rstan::stan_rdump(list = ls(dat), file = "modelB.dat",  envir = list2env(dat))

# create simple makefile
cat("SRC_FILE=$(NAME).stan\n", file = "Makefile")
cat("MODEL_HOME=C:/Rpackages/stanUtils/vignettes\n", file = "Makefile", append = TRUE)
cat("CMDSTAN_HOME=C:/cmdstan\n", file = "Makefile", append = TRUE)
cat("\n", file = "Makefile", append = TRUE)
cat("build: $(SRC_FILE)\n", file = "Makefile", append = TRUE)
cat("	cd $(shell cygpath -u ${CMDSTAN_HOME}) && make $(MODEL_HOME)/$(NAME).exe\n", file = "Makefile", append = TRUE)
cat("	rm -f $(NAME).hpp\n", file = "Makefile", append = TRUE)
cat("\n", file = "Makefile", append = TRUE)
cat("mcmc:\n", file = "Makefile", append = TRUE)
cat("	./$(NAME).exe sample algorithm=hmc num_samples=1000 num_warmup=1000 thin=1 init=$(NAME).ini data file=$(NAME).dat output file=$(NAME)$(chain).mcmc\n", file = "Makefile", append = TRUE)
cat("	$(RM) .RData\n", file = "Makefile", append = TRUE)

## ------------------------------------------------------------------------
# build the model from the command line
system("make build NAME=modelA")
system("make build NAME=modelB")

# run the model
system("make mcmc NAME=modelA chain=1")
system("make mcmc NAME=modelA chain=2")
system("make mcmc NAME=modelB chain=1")
system("make mcmc NAME=modelB chain=2")

## ---- echo = TRUE--------------------------------------------------------
# extract and save mcmc outputs as *.rds file
(outA <- stan_extract(model = "modelA", parameters = "mu", outputs = c("mu", "mse")))
stanSave(outA)

(outB <- stan_extract(model = "modelB", parameters = c("mu", "sigma"), outputs = c("mu", "mse")))
stanSave(outB)


## ------------------------------------------------------------------------
slotNames(outA)

## ------------------------------------------------------------------------
# examine multiple chains in stanOutput object
traceplot(outA)
histplot(outA)

## ------------------------------------------------------------------------
# examine permutted chains
(outpostA <- posterior(outA))
traceplot(outpostA)
histplot(outpostA)
print(outpostA)

## ------------------------------------------------------------------------
# examine permutted chains
(outpostAB <- posterior(models = c("modelA", "modelB"), pars = c("mu")))
traceplot(outpostAB, pars = c("mu"))
histplot(outpostAB, pars = c("mu"))
print(outpostAB)

## ---- echo = FALSE, results = 'hide'-------------------------------------
files <- list.files(pattern = "[.]par");  file.remove(files)
files <- list.files(pattern = "[.]dat");  file.remove(files)
files <- list.files(pattern = "[.]ini");  file.remove(files)
files <- list.files(pattern = "[.]stan"); file.remove(files)
files <- list.files(pattern = "[.]exe");  file.remove(files)
files <- list.files(pattern = "[.]rds");  file.remove(files)
files <- list.files(pattern = "[.]mcmc"); file.remove(files)
file.remove("USER_HEADER.hpp")
file.remove("Makefile")

