#' Functions to manage mallet installation
#' 
#' @param url url of mallet
#' @param verbose A `logical` value, whether to output progress messages.
#' @export mallet_install
#' @rdname mallet_utils
#' @importFrom rJava .jinit
#' @importFrom utils download.file untar
#' @examples
#' if (isFALSE(mallet_is_installed())) mallet_install()
#' mallet_get_version()
#' 
#' @details `mallet_install()` will download the mallet Java package
#'   and put it into the java directory of the biglda R package.
mallet_install <- function(url = "https://github.com/mimno/Mallet/releases/download/v202108/Mallet-202108-bin.tar.gz", verbose = TRUE){
  # "http://mallet.cs.umass.edu/dist/mallet-2.0.8.tar.gz"
  if (verbose) cli::cli_progress_step("downloading mallet")
  mallet_dir <- system.file(package = "biglda", "java")
  mallet_tarball <- file.path(mallet_dir, basename(url))
  download.file(url = url, destfile = mallet_tarball)
  
  if (verbose) cli::cli_progress_step("extract tarball")
  untar(tarfile = mallet_tarball, verbose = FALSE, exdir = mallet_dir)
  unlink(x = mallet_tarball)
  
  if (verbose) cli_progress_step("initializing JVM")
  .jinit()
  
  if (verbose) cli_progress_step("adding mallet jars to classpath")
  mallet_set_classpath()
}


#' @export mallet_get_version
#' @rdname mallet_utils
#' @details `mallet_get_version()` will return a `numeric_version` object of
#'   mallet is installed, or `NULL` if mallet is not available.
mallet_get_version <- function(){
  if (isFALSE(mallet_is_installed())) return(NULL)
  mallet_dir <- basename(get_mallet_dir())
  if (grepl("mallet-\\d+\\.\\d+\\.\\d+", mallet_dir)){
    version_vec <- list(c(
      gsub("mallet-(\\d+)\\.\\d+\\.\\d+", "\\1", mallet_dir),
      gsub("mallet-\\d+\\.(\\d+)\\.\\d+", "\\1", mallet_dir),
      gsub("mallet-\\d+\\.\\d+\\.(\\d+)", "\\1", mallet_dir)
    ))
  } else if (grepl("[mM]allet-\\d+$", mallet_dir)){
    version_vec <- list(c(gsub("^[Mm]allet-(\\d+)$", "\\1", mallet_dir)))
  } else {
    warning("cannot extract version from mallet dir")
    return(NULL)
  }
  class(version_vec) <- "numeric_version"
  version_vec
}

#' @rdname mallet_utils
#' @details `mallet_is_installed()` utility will return a `logical` value
#'   whether mallet is installed or not.
#' @export mallet_is_installed
mallet_is_installed <- function()
  if (length(get_mallet_dir()) == 1L) TRUE else FALSE



#' @rdname biglda-package
#' @importFrom rJava .jaddClassPath
mallet_set_classpath <- function(){
  mallet_dir <- get_mallet_dir()
  if (length(mallet_dir) != 1L){
    warning("mallet is not available, call mallet_install()")
    return(NULL)
  }
  
  mallet_jar_file <- file.path(mallet_dir, "lib", "mallet-deps.jar")
  mallet_class_dir <- file.path(mallet_dir, "class")
  .jaddClassPath(mallet_jar_file)
  .jaddClassPath(mallet_class_dir)
}


#' Get directory with Mallet jar files.
#' 
#' This utility function checks two options where Mallet jars might be: (a) The
#' directory indicated by the environment variable 'MALLET_DIR' and the
#' directory 'extdata/java' within the installed biglda package.
#' @export get_mallet_dir
get_mallet_dir <- function(){
  if (nchar(Sys.getenv("MALLET_DIR")) > 0L){
    malletdir <- Sys.getenv("MALLET_DIR")
    if (!file.exists(malletdir))
      warning("Directory indicated by envvar 'MALLET_DIR' does not exist")
    return(malletdir)
  } else {
    jdirfiles <- list.files(
      system.file(package = "biglda", "java"),
      full.names = TRUE
    )
    dirs <- jdirfiles[sapply(jdirfiles, function(f) file.info(f)$isdir)]
    malletdir <- dirs[grepl("^[Mm]allet-", basename(dirs))]
    
    if (length(malletdir) >= 2L){
      warning("more than one Mallet version installed")
    } else if (length(malletdir) == 0L){
      return(NULL)
    } else {
      return(malletdir)
    }
  }
  malletdir
}

#' Get size of file and return formatted value
#' @param x Path to a file
get_formatted_filesize <- function(x){
  filesize <- file.info(x)$size
  class(filesize) <- "object_size"
  format(filesize, "GB")
}


