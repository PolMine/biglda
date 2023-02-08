#' @importFrom rJava .jpackage
.onAttach <- function(libname, pkgname){
  if (mallet_is_installed()){
    mallet_dir <- get_mallet_dir()
    mallet_deps_jar <- file.path(mallet_dir, "lib", "mallet-deps.jar")
    mallet_class_dir <- file.path(mallet_dir, "class")
    .jpackage(pkgname, lib.loc = libname,)
    .jaddClassPath(path = c(mallet_deps_jar, mallet_class_dir))
    packageStartupMessage(sprintf("Mallet version: v%s", mallet_get_version()))
  } else {
    .jpackage(pkgname, lib.loc = libname) # Nothing will be added to classpath
    packageStartupMessage(
      "No mallet installation found. Use mallet_install() for installation!"
    )
  }
  
  jvm_mem <- J("java/lang/Runtime")$getRuntime()$maxMemory()
  class(jvm_mem) <- "object_size"

  packageStartupMessage(sprintf(
    "JVM memory allocated: %s",
    format(jvm_mem, units = "GB")
  ))

  if ("mallet.jar" %in% basename(rJava::.jclassPath())){
    packageStartupMessage(
      "The Java Archive 'mallet.jar' included in the R package 'mallet' is on the classpath. ", 
      "It includes a ParallelTopicModel class not compatible with the functionality of the biglda packaage. ",
      "To avoid error messages, avoid loading the 'mallet' and the 'biglda' package at the same time."
    )
  }
}