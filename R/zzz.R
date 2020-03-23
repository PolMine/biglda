#' @importFrom rJava .jpackage
.onAttach <- function(libname, pkgname){
  if (mallet_is_installed()){
    .jpackage(
      pkgname, jars = "mallet-2.0.8/lib/mallet-deps.jar",
      morePaths = c(
        system.file(package = pkgname, lib.loc = libname, "java"),
        system.file(package = pkgname, lib.loc = libname, "java", "mallet-2.0.8", "class")
      ),
      lib.loc = libname,
    )
  } else {
    .jpackage(pkgname, lib.loc = libname) # Nothing will be added to classpath
    packageStartupMessage("No mallet installation found. Use mallet_install() for installation!")
  }

  if ("mallet.jar" %in% basename(rJava::.jclassPath())){
    packageStartupMessage(
      "The Java Archive 'mallet.jar' included in the R package 'mallet' is on the classpath. ", 
      "It includes a ParallelTopicModel class not compatible with the functionality of the biglda packaage. ",
      "To avoid error messages, avoid loading the 'mallet' and the 'biglda' package at the same time."
    )
  }
}