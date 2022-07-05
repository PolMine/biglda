#' Functions to manage mallet installation
#' 
#' @param url url of mallet
#' @export mallet_install
#' @rdname mallet_utils
#' @importFrom rJava .jinit
#' @importFrom utils download.file untar
#' @examples
#' if (isFALSE(mallet_is_installed())) mallet_install()
#' mallet_get_version()
#' 
#' @details The \code{mallet_install} function will downlpad the mallet Java package
#'   and put it into the java directory of the biglda R package.
mallet_install <- function(url = "http://mallet.cs.umass.edu/dist/mallet-2.0.8.tar.gz"){
  # "https://github.com/mimno/Mallet/releases/download/v202108/Mallet-202108-bin.tar.gz"

  message("... downloading mallet")
  mallet_dir <- system.file(package = "biglda", "java")
  mallet_tarball <- file.path(mallet_dir, basename(url))
  download.file(url = url, destfile = mallet_tarball)
  untar(tarfile = mallet_tarball, verbose = FALSE, exdir = mallet_dir)
  unlink(x = mallet_tarball)
  message("... initializing JVM")
  .jinit()
  message("... adding to classpath")
  mallet_set_classpath()
}


#' @export mallet_get_version
#' @rdname mallet_utils
#' @details The \code{mallet_get_version} function will return a
#'   \code{numeric_version} object of mallet is installed, or \code{NULL} if
#'   mallet is not available.
mallet_get_version <- function(){
  if (isFALSE(mallet_is_installed())) return(NULL)
  java_dir <- system.file(package = "biglda", "java")
  mallet_install_dir <- grep(
    "mallet-\\d+\\.\\d+\\.\\d+",
    list.dirs(java_dir, recursive = FALSE, full.names = FALSE)[1],
    value = TRUE
  )
  version_vec <- list(c(
    gsub("mallet-(\\d+)\\.\\d+\\.\\d+", "\\1", mallet_install_dir),
    gsub("mallet-\\d+\\.(\\d+)\\.\\d+", "\\1", mallet_install_dir),
    gsub("mallet-\\d+\\.\\d+\\.(\\d+)", "\\1", mallet_install_dir)
  ))
  class(version_vec) <- "numeric_version"
  version_vec
}

#' @rdname mallet_utils
#' @details The \code{mallet_is_installed} utility will return a logical value
#'   whether mallet is installed or not.
#' @export mallet_is_installed
mallet_is_installed <- function(){
  mallet_dir <- system.file(package = "biglda", "java")
  j_dirs <- list.dirs(system.file(package = "biglda", "java"), recursive = FALSE)
  if (length(j_dirs) == 0L) return(FALSE)
  grepl("mallet-\\d+\\.\\d+\\.\\d+", j_dirs)
}


#' @rdname biglda-package
#' @importFrom rJava .jaddClassPath
mallet_set_classpath <- function(){
  if (isFALSE(mallet_is_installed())){
    warning("mallet is not available, call mallet_install()")
    return(NULL)
  }
  mallet_dir <- system.file(package = "biglda", "java", sprintf("mallet-%s", as.character(mallet_get_version())))
  mallet_jar_file <- file.path(mallet_dir, "lib", "mallet-deps.jar")
  mallet_class_dir <- file.path(mallet_dir, "class")
  .jaddClassPath(mallet_jar_file)
  .jaddClassPath(mallet_class_dir)
}


