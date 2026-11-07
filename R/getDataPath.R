#' GEt the path to an included data file
#'
#' @param filename The filename of the file
#'
#' @returns The path as a character value
#' @export
#'
#' @examples
#' behaviorchange::getDataPath(
#'   "COMPLECS-spec-example.Rds"
#'  );
getDataPath <-
  function(filename) {
    system.file(
      "extdata",
      filename,
      package = "behaviorchange"
    );
  }
