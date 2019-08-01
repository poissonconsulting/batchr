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

arg_files <- function(path, recursive, full.names = FALSE) {
  list.files(path = path, pattern = "^[.]batchr_setup[.]rds$", 
             recursive = recursive, full.names = full.names, all.files = TRUE)
}

save_batchr_setup.rds <- function(path, pattern, recursive, FUN, dots) {
  args <- list(time = sys_time(), pattern = pattern, 
               recursive = recursive, FUN = FUN, 
               dots = dots)
  saveRDS(args, file = file.path(path, ".batchr_setup.rds"))
}

read_batchr_setup.rds <- function(path) {
  path <- file.path(path, ".batchr_setup.rds")
  if(!file.exists(path)) err("there is no '.batchr_setup.rds' file")
  readRDS(file = path)
}
