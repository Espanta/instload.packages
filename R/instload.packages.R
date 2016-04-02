#' Install and Load R Packages
#' 
#' This function receives a list of packages to install and load.
#' For each of the package names, it does:
#'       
#'       1- checks local repository of installed packages. If the given package is not installed
#'          it installs it otherwise it throws warning message and skip to the next package
#'       2- Once all packages installed successfully, it load them into workspace using 'require' function
#' If at all you give a package that cannot be installed, it ignores its loading and throws a warning message
#' about its failure to install the package.
#' 
#' Note 1: Functional Modes: By default the function reports successfull installation of those packages successfully installed.
#' However, if reporting is not desirable, can turn it off by 'silent=FALSE'
#' 
#' Note 2: instload.packages does accept only list of characters. It does not accept integer or decimal values
#' 
#' instload.packages("pkg1","pkg2",....,"pkgn")
#' 
#' example:
#' instload.packages("caret","tm","ts")
#' instload.packages("caret","tm","ts",silent=TRUE)
#' ==============================================
#' Please do not hesiatate to report errors or exceptions to my email @ Abolfazli.s@gmail.com

instload.packages<- function(...,silent=FALSE){
  
  #check names and run 'require' function over if the given package is installed
  requirePkg<- function(pkg){if(length(setdiff(pkg,rownames(installed.packages())))==0)
                                    require(pkg, quietly = TRUE,character.only = TRUE)
                            }

  # list of packages to install and load
  packages <- as.vector(unlist(list(...)))
  if(!is.character(packages))stop("No numeric allowed! Input must contain package names to install and load")
  
  if (length(setdiff(packages,rownames(installed.packages()))) > 0 )
                      install.packages(setdiff(packages,rownames(installed.packages())),
                                       repos = c("https://cran.revolutionanalytics.com/", "http://owi.usgs.gov/R/"))

  res<- unlist(sapply(packages, requirePkg))
  
  if(Silent == FALSE && !is.null(res)) {cat("\nBellow Packages Successfully Installed:\n\n")
                                        print(res)
                                        }
}