## Print this on start so I know it's loaded.
cat("Loading custom .Rprofile \n")

## Nice option for local work. I keep it commented out so my code can remain portable.
options(stringsAsFactors = FALSE)

## Increase the size of my Rhistory file, becasue I like to use the up arrow!
Sys.setenv(R_HISTSIZE='100000')

# `local` creates a new, empty environment
# This avoids polluting .GlobalEnv with the object r
local({
  r = getOption("repos")             
  r["CRAN"] = "https://cran.rstudio.com/"
  options(repos = r)
})

## Load basic packages automatically
# if (!require("pacman")) install.packages("pacman")

## Create invisible environment ot hold all your custom functions
.env <- new.env()

## Single character shortcuts for summary() and head().
.env$s <- base::summary
.env$h <- utils::head

.env$v <- utils::View

#ht==headtail, i.e., show the first and last 10 items of an object.
.env$ht <- function(d, n = 6) rbind(head(d, n), tail(d, n))

## Attach all the variables above
attach(.env)
