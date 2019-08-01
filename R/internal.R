check_directory <- function(path) {
  chk_string(path)
  if(!dir.exists(path)) err("directory '", path, "' does not exist")
  path
}

.batch_files <- function(path, pattern, recursive, full.names = FALSE) {
  list.files(path = path, pattern = pattern, recursive = recursive,
             full.names = full.names)
}

arg_files <- function(path, recursive, full.names = FALSE) {
  list.files(path = path, pattern = .argsbatchr_pattern, recursive = recursive,
             full.names = full.names)
}

sys_time <- function() {
  time <- Sys.time()
  attr(time, "tzone") <- "UTC" 
  time
}

save_args <- function(path, pattern, recursive, FUN, dots) {
  args <- list(time = sys_time(), pattern = pattern, 
               recursive = recursive, FUN = FUN, 
               dots = dots)
  saveRDS(args, file = file.path(path, .argsbatchr))
}

read_args <- function(path) {
  path <- file.path(path, .argsbatchr)
  if(!file.exists(path)) err("there is no '", .argsbatchr, "' file")
  readRDS(file = path)
}
