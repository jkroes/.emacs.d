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

################################# Startup tutorial ################################

# ?Startup
# Make sure R_HOME/etc/Rprofile.site does not exist, or .Rprofile will not be read:
## site_path = R.home(component = "home")
## fname = file.path(site_path, "etc", "Rprofile.site")
## file.exists(fname)
# Unset R_ENVIRON if it is set, for the same reason.
# R looks for .Rprofile in getwd(), then HOME, then possibly R_HOME (see below)

# R_HOME is auto-set to the parent directory (...) of the interpreter's path (.../bin/R).
# In RStudio, go to Tools>Global Options>General>R Sessions and set "R version:" to 
# [64-bit] C:\Users\jkroes\Documents\R\R-3.6.1 
# In emacs, 

# https://cran.r-project.org/bin/windows/base/rw-FAQ.html#What-are-HOME-and-working-directories_003f
# If the "home directory" (not to be confused with R.home(), R_HOME, or HOME) is unset and 
# R_USER is set when invoking the interpreter:
## "D:\R-3.6.2\bin\R.exe" R_USER=C:
# or as an environment variable in the shell or through Windows' environment variable editor GUI,
# then the "home directory" will use R_USER's value. If R_USER is unset in this way, 
# the home directory is set to the first of HOME,
# C:\Users\<username>\Documents, <HOMEDRIVE><HOMEPATH>, and getwd(). 
# R_USER and HOME will be set to the home directory if either are unset. 

# https://cran.r-project.org/bin/windows/base/rw-FAQ.html#I-don_0027t-have-permission-to-write-to-the-
# R_LIBS_USER uses the value of the "home directory" as its prefix if unset.
# This value is prepended to .libPaths() if it exists, or if you set it rather than let it inherit 
# default values. I am not sure which, as this just works for me currently.

# TL;DR You can set R_USER or R_LIBS_USER to specify a custom library location for R packages. 