#' R Library Summary
#'
#' Provides a brief summary of the package library on your machine
#'
#' @param sizes A logical value indicating whether to calculate sizes.
#'    Default `FALSE`
#'
#' @return A `data.frame` containing the count of packages in each of the user's libraries.
#'    If `sizes=TRUE`, a column of library sizes will be added.
#' @export
#'
#' @examples
#' lib_summary()
#' lib_summary(sizes=TRUE)
lib_summary <- function(sizes = FALSE) {
  if (!is.logical(sizes)) {
    stop("sizes must be a logical value (TRUE/FALSE)")
  }

  pkgs <- utils::installed.packages()
  pkg_tbl <- table(pkgs[, "LibPath"])
  pkg_df <- as.data.frame(pkg_tbl, stringsAsFactors = FALSE)
  names(pkg_df) <- c("library","n_packages")

  if (sizes) {
    pkg_df <- calculate_sizes(pkg_df)
  }

  pkg_df
}

#' calculate sizes
#'
#' @param df a dataframe of libraries
#'
#' @noRd make it so user doesn't get documentation for a function that they won't see
#'
#'@return a `data.frame` with a lib_size column
calculate_sizes <- function(df) {
  df$lib_size <- map_dbl(
    df$library,
    ~ sum(fs::file_size(fs::dir_ls(.x, recurse=TRUE)))
  )
  df
}



