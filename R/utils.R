#
# Utility functions that are not exported (should not be)
#
#  quantile helper function
#' 
#' @export
.quantile <- function(x, dp = 2) {
    
    y <- quantile(x, c(0.5, 0.025, 0.975))
    
    if(y[1] < 10) z <- signif(y, 3) else z <- round(y, dp)
    
    paste0(z[1], " (", z[2], "-", z[3], ")")
}

#' @export
.dpselect <- function(x) {
    
    y <- unlist(strsplit(x, split = "[", fixed = TRUE))[1]
    
    switch(y,
           "lp__" = 2,
           2
           )
}
