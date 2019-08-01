sys_time <- function() {
  time <- Sys.time()
  attr(time, "tzone") <- "UTC" 
  time
}

check_directory <- function(path) {
  chk_string(path)
  if(!dir.exists(path)) err("directory '", path, "' does not exist")
  path
}

.batch_files <- function(path, pattern, recursive, full.names = FALSE) {
  list.files(path = path, pattern = pattern, recursive = recursive,
             full.names = full.names)
}

file.exists_batchr_setup.rds <- function(path) 
  file.exists(file.path(path, ".batchr_setup.rds"))

save_batchr_setup.rds <- function(path, pattern, recursive, FUN, dots) {
  args <- list(time = sys_time(), pattern = pattern, 
               recursive = recursive, FUN = FUN, 
               dots = dots)
  saveRDS(args, file = file.path(path, ".batchr_setup.rds"))
}

read_batchr_setup.rds <- function(path) {
  if(!file.exists_batchr_setup.rds(path))
    err("directory '", path, "' does not contain a '.batchr_setup.rds' file")
  readRDS(file = file.path(path, ".batchr_setup.rds"))
}
