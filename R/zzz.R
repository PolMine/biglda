.onLoad <- function(libname, pkgname) {
  if (mallet_is_installed()){
    rJava::.jpackage(
      pkgname,
      jars = "mallet-2.0.8/lib/mallet-deps.jar",
      morePaths = system.file(package = pkgname, lib.loc = libname, "java", "mallet-2.0.8", "class"),
      lib.loc = libname
    )
    if ("mallet.jar" %in% basename(rJava::.jclassPath())){
      packageStartupMessage("Presumably mallet is loaded, slightly diffent, potential problem")
    }
  } else {
    packageStartupMessage("mallet not present, call biglda::mallet_install()")
  }
}