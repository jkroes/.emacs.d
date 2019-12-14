# Textual menu selection (for pkg installation if a CRAN mirror isn't set)
options(menu.graphics=FALSE)

# set a CRAN mirror
local({r <- getOption("repos")
       r["CRAN"] <- "http://cran.cnr.berkeley.edu"
       options(repos=r)})

# Open help within emacs buffer, not browser
options(help_type = "text")

# Clear environment
rm(list=ls())