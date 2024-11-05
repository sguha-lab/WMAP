
if ("package:WMAP" %in% search()){
  ## unload a loaded package without restarting R
  detach("package:WMAP", unload=TRUE)
}

#library(roxygen2)
#roxygen2::roxygenise()


devtools::build()

install.packages("../WMAP_1.0.0.tar.gz")


