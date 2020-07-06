#' This is a straight copy of the function trimws that exists in version 4.0 and being. Its here just in case the R version goes screwy
trimws <- function (x, which = c("both", "left", "right"), whitespace = "[ \t\r\n]") {
  which <- match.arg(which)
  mysub <- function(re, x) sub(re, "", x, perl = TRUE)
  switch(which, left = mysub(paste0("^", whitespace, "+"), 
                             x), right = mysub(paste0(whitespace, "+$"), x), both = mysub(paste0(whitespace, 
                                                                                                 "+$"), mysub(paste0("^", whitespace, "+"), x)))
}