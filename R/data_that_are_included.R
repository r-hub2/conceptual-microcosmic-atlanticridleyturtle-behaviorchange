### Easily update all
updateAll <- FALSE;

#' Subsets of Party Panel datasets
#'
#' These are subsets of Party Panel datasets. Party Panel is an annual
#' semi-panel determinant study among Dutch nightlife patrons, where every
#' year, the determinants of another nightlife-related risk behavior are
#' mapped.
#'
#' The behaviors of the Party Panel waves were:
#'
#' - 2015: Behaviors related to using highly dosed ecstasy pills
#' - 2016: Behaviors related to visiting nightlife first-aid facilities
#' - 2017: Behaviors related to hearing protection
#' - 2018: Behaviors related to flirting and boundary crossing
#' - 2019: Behaviors related to sleeping hygiene surrounding nightlife participation
#'
#' The full datasets are publicly available through the Open Science
#' Framework (https://osf.io/s4fmu/). Also see the GitLab
#' repositories (https://gitlab.com/partypanel) and the website
#' at https://partypanel.eu.
#'
#' @docType data
#' @aliases BBC_data BBC_pp15.1 BBC_pp16.1 BBC_pp17.1 BBC_pp18.1
#' @keywords data
#' @name partypanelData
#' @usage data(BBC_pp15.1)
#' @usage data(BBC_pp16.1)
#' @usage data(BBC_pp17.1)
#' @usage data(BBC_pp18.1)
#' @format For BBC_pp15.1, a `data.frame` with 123 columns and 829 rows.
#' For BBC_pp16.1, a `data.frame` with 63 columns and 1077 rows.
#' For BBC_pp17.1, a `data.frame` with 94 columns and 943 rows.
#' For BBC_pp18.1, a `data.frame` with 84 columns and 880 rows.
#' Note that many rows contain missing values; the columns and rows
#' were taken directly from the original Party Panel datasets, and
#' represent all participants that made it past a given behavior.
#' @examples data('BBC_pp17.1', package='behaviorchange');
#' behaviorchange::CIBERlite(data=BBC_pp17.1,
#'                           determinants=c("epw_attitude",
#'                                          "epw_perceivedNorm",
#'                                          "epw_pbc",
#'                                          "epw_habit"),
#'                           targets=c("epw_intention"));
c("BBC_pp15.1", "BBC_pp16.1", "BBC_pp17.1", "BBC_pp18.1");

### Systematic review prereg form
if (exists("updateAll") && updateAll) {

  BBC_pp15.1 <- read.csv2(getDataPath("BBC_pp15.1.csv"));
  BBC_pp15.1$highDose_intention <- as.numeric(BBC_pp15.1$highDose_intention);
  BBC_pp15.1$highDose_attitude <- as.numeric(BBC_pp15.1$highDose_attitude);
  BBC_pp15.1$highDose_perceivedNorm <- as.numeric(BBC_pp15.1$highDose_perceivedNorm);
  BBC_pp15.1$highDose_pbc <- as.numeric(BBC_pp15.1$highDose_pbc);
  usethis::use_data(BBC_pp15.1, overwrite=TRUE);

  BBC_pp16.1 <- read.csv2(getDataPath("BBC_pp16.1.csv"));
  BBC_pp16.1$faSelfAlc_intention <- as.numeric(BBC_pp16.1$faSelfAlc_intention);
  BBC_pp16.1$faSelfAlc_attitude <- as.numeric(BBC_pp16.1$faSelfAlc_attitude);
  BBC_pp16.1$faSelfAlc_perceivedNorm <- as.numeric(BBC_pp16.1$faSelfAlc_perceivedNorm);
  BBC_pp16.1$faSelfAlc_pbc <- as.numeric(BBC_pp16.1$faSelfAlc_pbc);
  usethis::use_data(BBC_pp16.1, overwrite=TRUE);

  BBC_pp17.1 <- read.csv2(getDataPath("BBC_pp17.1.csv"));
  BBC_pp17.1$epw_behavior <- as.numeric(BBC_pp17.1$epw_behavior);
  BBC_pp17.1$epw_intention <- as.numeric(BBC_pp17.1$epw_intention);
  BBC_pp17.1$epw_attitude <- as.numeric(BBC_pp17.1$epw_attitude);
  BBC_pp17.1$epw_perceivedNorm <- as.numeric(BBC_pp17.1$epw_perceivedNorm);
  BBC_pp17.1$epw_pbc <- as.numeric(BBC_pp17.1$epw_pbc);
  BBC_pp17.1$epw_habit <- as.numeric(BBC_pp17.1$epw_habit);
  usethis::use_data(BBC_pp17.1, overwrite=TRUE);

  BBC_pp18.1 <- read.csv2(getDataPath("BBC_pp18.1.csv"));
  BBC_pp18.1$siba_intention <- as.numeric(BBC_pp18.1$siba_intention);
  BBC_pp18.1$siba_attitude <- as.numeric(BBC_pp18.1$siba_attitude);
  BBC_pp18.1$siba_perceivedNorm <- as.numeric(BBC_pp18.1$siba_perceivedNorm);
  BBC_pp18.1$siba_pbc <- as.numeric(BBC_pp18.1$siba_pbc);
  usethis::use_data(BBC_pp18.1, overwrite=TRUE);

}
