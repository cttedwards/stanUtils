---
title: "How to build, run and interrogate `cmdStan` models using the `stanUtils` package"
author: "Charles T T Edwards (NIWA, Wellington, New Zealand)"
date: "2016-12-24"
output:
  rmarkdown::html_vignette:
    fig_caption: yes
    toc: yes
vignette: >
  %\VignetteIndexEntry{stanUtils}
  %\VignetteEngine{knitr::rmarkdown}
  \usepackage[utf8]{inputenc}
---



This package is designed to make it easier to run and interrogate models using `cmdStan`, and the steps involved are detailed here. First ensure that `cmdStan` is installed on your computer, and then load the `stanUtils` package.


```r
library(stanUtils)
```

## Set up

Write the stan model code, data file, initial value file and (if desired) a make file


```r
# write example models
stan_codeA <- "data {int N; vector[N] y;} parameters {real mu;} model {y ~ normal(mu,1);} generated quantities {real mse; mse = mean((y - mu) .* (y - mu));}"
stan_codeB <- "data {int N; vector[N] y;} parameters {real mu; real<lower=0> sigma;} model {y ~ normal(mu,sigma);} generated quantities {real mse; mse = mean((y - mu) .* (y - mu));}"

cat(stan_codeA, file = "modelA.stan")
cat(stan_codeB, file = "modelB.stan")

# write *.ini file
inis <- list(mu = 10)
rstan::stan_rdump(list = ls(inis), file = "modelA.ini", envir = list2env(inis))

inis <- list(mu = 10, sigma = 1)
rstan::stan_rdump(list = ls(inis), file = "modelB.ini", envir = list2env(inis))

# write *.dat file
dat <- list(N = 100, y = rnorm(100, 10, 1))
rstan::stan_rdump(list = ls(dat), file = "modelA.dat", envir = list2env(dat))
rstan::stan_rdump(list = ls(dat), file = "modelB.dat", envir = list2env(dat))

# create simple makefile
cat("SRC_FILE=$(NAME).stan\n", file = "Makefile")
cat("MODEL_HOME=H:/CODE/packages/stanUtils/vignettes\n", file = "Makefile", 
    append = TRUE)
cat("CMDSTAN_HOME=C:/cmdstan\n", file = "Makefile", append = TRUE)
cat("\n", file = "Makefile", append = TRUE)
cat("build: $(SRC_FILE)\n", file = "Makefile", append = TRUE)
cat("\tcd $(shell cygpath -u ${CMDSTAN_HOME}) && make $(MODEL_HOME)/$(NAME).exe\n", 
    file = "Makefile", append = TRUE)
cat("\trm -f $(NAME).hpp\n", file = "Makefile", append = TRUE)
cat("\n", file = "Makefile", append = TRUE)
cat("mcmc:\n", file = "Makefile", append = TRUE)
cat("\t./$(NAME).exe sample algorithm=hmc num_samples=1000 num_warmup=1000 thin=1 init=$(NAME).ini data file=$(NAME).dat output file=$(NAME)$(chain).mcmc\n", 
    file = "Makefile", append = TRUE)
cat("\t$(RM) .RData\n", file = "Makefile", append = TRUE)
```

Build and run the model or models from the command line.


```r
# build the model from the command line
system("make build NAME=modelA")
system("make build NAME=modelB")

# run the model
system("make mcmc NAME=modelA chain=1")
system("make mcmc NAME=modelA chain=2")
system("make mcmc NAME=modelB chain=1")
system("make mcmc NAME=modelB chain=2")
```

## Save model parameters and outputs

For each model, extract the MCMC outputs into a `stanOuput` object using `stan_extract`, specifiying whether to treat the outputs as `parameters` (for which chain identities are retained) or `outputs` (for which the chains are permutted and combined). The specification of `parameters` and `outputs` can overlap, depending on downstream analyses. This flexibility is designed to keep the `stanOutput` object small, particularly for large model applications.

A call to `stanSave` will save the `stanOutput` object using `base::saveRDS`, with a file name that matches the model name. 


```r
# extract and save mcmc outputs as *.rds file
(outA <- stan_extract(data = TRUE, mcmc = TRUE, model = "modelA", parameters = "mu", 
    outputs = c("mu", "mse")))
#> stanOutput S4 object class for model 'modelA'
stanSave(outA)

(outB <- stan_extract(data = TRUE, mcmc = TRUE, model = "modelB", parameters = c("mu", 
    "sigma"), outputs = c("mu", "mse")))
#> stanOutput S4 object class for model 'modelB'
stanSave(outB)
```
The `stanOuput` class object contains a variety of slots that are populated during a call to `stan_extract`:

```r
slotNames(outA)
#> [1] "parameters"  "outputs"     "data"        "map"         "mcmc"       
#> [6] "variational" "inits"       "model"
```
In the current example, because it is of primary interest to users of `cmdStan`, we are concerned only with Bayesian outputs stored in `<object>@mcmc`.

## Interrogate single model

For each model, produce diagnostic plots through calls to `traceplot` and `histplot` which will plot `parameters`, keeping chain separate.


```r
# examine multiple chains in stanOutput object
traceplot(outA)
```

![plot of chunk unnamed-chunk-7](figure/unnamed-chunk-7-1.png)

```r
histplot(outA)
```

![plot of chunk unnamed-chunk-7](figure/unnamed-chunk-7-2.png)
Alternatively, extract a posterior sample through a call to `posterior` which will extract the permutted chains for the specificed model `outputs` (provided they are stored in the `stanOutput` object). by default all `outputs` are extracted. These can also be plotted.

```r
# examine permutted chains
(outpostA <- posterior(outA))
#> stanPosterior for model: 'modelA', containing pars: mu, mse
traceplot(outpostA)
```

![plot of chunk unnamed-chunk-8](figure/unnamed-chunk-8-1.png)

```r
histplot(outpostA)
```

![plot of chunk unnamed-chunk-8](figure/unnamed-chunk-8-2.png)

## Interrogate multiple models

The `posterior` function, when provided with a character vector of model names, can also be used to combined outputs from multple models. In this case the `pars` argument must be specified. These can then also be plotted for model comparison purposes.


```r
# examine permutted chains
(outpostAB <- posterior(models = c("modelA", "modelB"), pars = c("mu")))
#> stanPosterior objects for models: modelA, modelB
traceplot(outpostAB, pars = c("mu"))
```

![plot of chunk unnamed-chunk-9](figure/unnamed-chunk-9-1.png)

```r
histplot(outpostAB, pars = c("mu"))
```

![plot of chunk unnamed-chunk-9](figure/unnamed-chunk-9-2.png)



