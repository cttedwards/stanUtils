#
# Utility functions that are not exported (should not be)
#
#  quantile helper function
#' @export
.quantile <- function(x, dp = 2) {
    
    y <- round(quantile(x, c(0.5, 0.025, 0.975)), dp)
    
    paste0(y[1], " (", y[2], "-", y[3], ")")
}

#' @export
.dpselect <- function(x) {
    
    y <- unlist(strsplit(x, split = "[", fixed = TRUE))[1]
    
    switch(y,
           "lp__" = 2,
           2
           )
}
