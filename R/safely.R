

#' @export

safe_read_rds <- purrr::safely(read_rds)
