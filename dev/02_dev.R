# Building a Prod-Ready, Robust Shiny Application.
# 
# Each step is optional. 
# 
stop("Don't source this file !")

# 2. All along your project

## 2.1 Add modules
## 
golem::add_module( name = "swiss_votes" ) # Name of the module

## 2.2 Add dependencies

usethis::use_package( "rnoaa" ) # To call each time you need a new package

## 2.3 Add tests

usethis::use_test( "app" )

## 2.4 Add a browser button

golem::browser_button()

## 2.5 Add external files

golem::add_js_file( "resizecreate" )
golem::add_js_handler( "handlers" )
golem::add_css_file( "custom" )

# 3. Documentation

## 3.1 Vignette
#usethis::use_vignette("GEOView")
#devtools::build_vignettes()

## 3.2 Code coverage
## You'll need GitHub there
#usethis::use_github()
#usethis::use_travis()
#usethis::use_appveyor()

# You're now set! 
# go to dev/03_deploy.R
rstudioapi::navigateToFile("dev/03_deploy.R")
