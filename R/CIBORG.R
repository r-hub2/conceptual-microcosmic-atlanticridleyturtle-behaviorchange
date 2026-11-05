# Update 8 juli 2026
  # added invisible(res) to allow x <- CIBORG() and pull intermediate results
  # added ufs:: to line 363 (tryCatch ... "Determinants: ", vecTxt(determinants, useQuote="`"))
  # remove orderBy / decrease (on what group?)
  # changed outputfile to output_plot (seems more clear)
  # changed outputpath to output_tables (seems more clear)
  # added most @param
#'
#' CIBORG plots allow you to compare determinant stuctures for multiple groups.
#'
#' This function is based on the [behaviorchange::CIBER()] function and on
#' [this post](https://web.archive.org/web/20241004175837/https://fediscience.org/@mjskay/111128765582788364)
#' and on [this post](https://web.archive.org/web/20241004175726/https://github.com/mjskay/uncertainty-examples/blob/master/diamond_plot.md)
#' by Matthew Kay.
#'
#' @param data A data frame containing the `determinants`, `target`, and `group` variables.
#' \cr Example --> data = mydataframe
#'
#' @param determinants The determinant variables (columns in `data`). --> e.g.,
#' \cr Example --> c('det1', 'det2', 'det3')
#'
#' @param target The target variable (column in `data`); only one target per plot is allowed.
#' \cr Example --> target = 'behavior',
#'
#' @param group The grouping variable (column in `data`); max 9 groups can be included in a plot.
#' \cr Example --> group = 'group_variable'
#'
#' @param conf.level An list with confidence levels used for the mean and associations (diamond widths)
#' \cr Example --> conf.level = list(means = .9999, associations = .95),
#'
#' @param subQuestions Optional character vector to give labels to determinants
#' \cr Example --> c('name1', 'name2', 'name3')
#'
#' @param leftAnchors Optional character vector with the lowest-value per determinant, in the same order as `determinants`; defaults is `'Lo'`.
#' \cr Example --> c('lowest', 'not at all', 'never')
#'
#' @param rightAnchors Optional character vector with the highest-value per determinant, in the same order as `determinants`; defaults to `'Hi'`.
#' \cr Example --> c('highest', 'very much', 'always')
#'
#' @param output_plot Optional name and extention with path to save the plot.
#' \cr Example --> output_plot = 'plots/Awesome_plot.pdf',
#'
#' @param outputWidth Optional width of the saved plot; if `NULL`, computed automatically.
#' \cr Example --> outputWidth = 30
#'
#' @param outputHeight Optional height of the saved plot; if `NULL`, computed automatically based on the number of determinants.
#' \cr Example --> outputHeight = 30
#'
#' @param outputUnits Units for `outputWidth` & `outputHeight` (e.g. `'in'`, `'cm'`); defaults to `'i'n`.
#' \cr Example --> outputUnits = 'cm'
#'
#' @param outputParams A list with optional ggplot arguments; works only if output_plot is specified
#' \cr Example --> outputParams = list(dpi = 600)
#'
#' @param output_tables Optional directory path to store CSV tables `means.csv` and `associations.csv`.
#' \cr Example --> output_tables = '/Users/JohnDoe/Desktop/'
#'
#' @param groupColorPalette Optional character vector of colors, one per group; if `NULL`, a colorblind-friendly default is used.
#' \cr **Example**: `groupColorPalette = c('gold', 'purple', 'green')`
#'
#' @param strokeColors Optional character vector for the diamond outline colors of means associations
#' \cr Example --> strokeColors = c('black', 'orange')
#'
#' @param titlePrefix Optional HTML-editable string to add a prefix of the plot title, before the target variable name.
#' \cr Example --> titlePrefix = "**Figure 1.** <br> The means and associations",
#'
#' @param titleVarLabels Optional HTML-editable string to rename the target variable's in the title, instead of the raw `target` name.
#' \cr Example --> titleVarLabels = 'eductional level'
#'
#' @param titleSuffix Optional HTML-editable string to the end of the title; empty by default.
#' \cr Example --> titleSuffix = 'this is just an example'
#'
#' @param titleGroupNames Optional character vector to display names for the groups in the title that serve as legend.
#' \cr Example --> titleGroupNames = c('one', 'two', 'three')
#'
#' @param FootnoteText Optional HTML-style character string to add a footnote below the plot; if `NULL`, no footnote is shown.
#' \cr Example --> FootnoteText = 'In this footnote you can provide **HTML-styled** info to readers'
#'
#' @param jitterWidth Numeric value to set jitter along the width of points in the left (means) scatter plot.
#' \cr Example --> jitterWidth = 1
#'
#' @param jitterHeight Numeric value controlling jitter along the height of points in the left (means) scatter plot.
#' \cr Example --> jitterHeight = 0.3
#'
#' @param titleSize Numeric value for the font size of the plot title.
#' \cr Example --> titleSize = 18
#'
#' @param dotSize Numeric value for the size of individual data points in the left (means) scatter plot.
#' \cr Example --> dotSize = 3
#'
#' @param determinantSize Numeric value for the font size of the determinant labels/subquestions on the y-axis.
#' \cr Example --> determinantSize = 14
#'
#' @param axisSize Numeric value for the font size of the axis text below the plots.
#' \cr Example --> axisSize = 12
#'
#' @param rsq if `TRUE` (default) the \eqn{R^2} (CIs) are calculated per group and shown in the title; if `FALSE`, this is not performed.
#' \cr Example --> rsq = FALSE
#'
#' @param leftScaleRange Optional to set the min/max range for ALL determinants scales (leftplot); auto-detected if `NULL`; ignored if `scaleRanges` is set.
#' \cr Example --> leftScaleRange = c(0,7),
#'
#' @param scaleRanges Optional list to set the min/max per determinant, to normalize (0 to 1) determinants measured on different scales.
#' \cr Example --> scaleRanges = list(det1 = c(1,3), det2 = c(1,7), det3 = c(1,7))
#'
#' @param drawPlot if `TRUE` (default) the plot is drawn; if `FALSE`, it is built and returned but not drawn.
#' \cr Example --> drawPlot = FALSE
#'
#' @param useCompleteCases if `TRUE`, only complete cases (no `NA`) on `determinants` or `target` are used; defaults is `FALSE`.
#' \cr Example --> useCompleteCases = TRUE
#'
#' @param na.random if `TRUE`, NAs are considered random and the warning is suppresed if means and associations plots are based on different sample sizes.
#' \cr Example --> na.random = TRUE
#'
#' @param silent Whether to be silent (`TRUE`, the default) or chatty (`FALSE`).
#'
#' @param ... Any additional arguments are ignored.
#'
#' @return An object with the results, including the CIBORG plot.
#' \cr Example --> x <- CIBORG()
#'
#' @title Confidence Interval-Based Overview of Relevant Group patterns in behavioral determinants (CIBORG)
#'
#' @seealso [behaviorchange::CIBER()] for "regular" CIBER plots.
#'
#' @export
#' @examples
#' \donttest{### This example uses the determinant study Party Panel 17.1;
#' ### see ?behaviorchange::BBC_data for more information.
#' data(BBC_pp17.1);
#'
#' ### Create a three-level group variable
#' BBC_pp17.1$genderPrivilege <-
#'   ifelse(
#'     is.na(BBC_pp17.1$gender),
#'     3,
#'     as.numeric(BBC_pp17.1$gender != "Male") + 1
#'   );
#'
#' ### Produce the CIBORG plot
#'
#' ### Note that all question start with
#' ### (translated from Dutch) "If I'm in a venue
#' ### with loud music, and I wear earplugs, then..."
#'
#' behaviorchange::CIBORG(
#'   data = BBC_pp17.1,
#'   determinants = c(
#'     'epw_AttExpect_hearingDamage',
#'     'epw_AttExpect_highTone',
#'     'epw_AttExpect_musicVolume',
#'     'epw_AttExpect_musicFidelity',
#'     'epw_AttExpect_loudConversation',
#'     'epw_AttExpect_musicFocus',
#'     'epw_AttExpect_musicEnjoy'
#'   ),
#'   subQuestions = c(
#'     "...the probability of hearing damage is...",
#'     "...the probability of hearing a beep the next day is...",
#'     "...I hear the music...",
#'     "...I hear the music...",
#'     "...loud conversation bothers me...",
#'     "...focusing on the music is...",
#'     "...I enjoy the music..."
#'   ),
#'   leftAnchors = c(
#'     "very small",
#'     "very small",
#'     "very softly",
#'     "exactly the same",
#'     "not at all",
#'     "much harder",
#'     "much less"
#'   ),
#'   rightAnchors = c(
#'     "very large",
#'     "very large",
#'     "very loudly",
#'     "very distorted",
#'     "a lot",
#'     "much easier",
#'     "much more"
#'   ),
#'   target = 'epw_attitude',
#'   group = 'genderPrivilege'
#' );
#'
#' }
CIBORG <- function(data, # to indicate the data
                   determinants, # to inducate the determinants
                   target, # to indicate the target behavior
                   group, # to indicate the grouping variable
                   conf.level = list(means = .9999, # to adjust the CI of means (width of diamond)
                                     associations = .95), # to adjust the CI of associatons (width of diamond)
                   subQuestions = NULL, # to add determinant names (or items used)
                   leftAnchors = NULL, # to add lowest bound of items scores
                   rightAnchors = NULL, # to add higest bound of item scores
                   output_plot = NULL,
                   outputWidth = NULL,
                   outputHeight = NULL,
                   outputUnits = "in",
                   outputParams = list(),
                   output_tables = NULL, # plots with underlying data can be stored as CSV files
                   # orderBy = NULL,
                   # decreasing = NULL,
                   groupColorPalette = NULL, # to change colors (standard is colorblind friendly)
                   strokeColors = c("white", "black"),
                   titlePrefix = "Means and associations (r) with", # to change the title prefix
                   titleVarLabels = NULL, # to change the target name in the title
                   titleSuffix = "", # to add a suffix in the title <br> to position it on a new line
                   titleGroupNames = NULL, # to change group names displayed in the title
                   FootnoteText = NULL, # to add a HTML-style footnote to the plot
                   jitterWidth = .4, # adjust jitter (spacing) width of scatter plot
                   jitterHeight = .4, # adjust jitter (spacing) height scatter plot
                   titleSize = 15, # adjust size of title
                   dotSize = 2.5, # adjust size of dots leftplot
                   determinantSize = 14, # adjust size of subquestions, determinants
                   axisSize = 12, # adjust size of text below plots
                   # xbreaks=NULL,
                   rsq = TRUE, # disable the R2 and regression used to calculate it
                   leftScaleRange = NULL, # auto detects but you can set it manually for all determinants c(1, 7)
                   scaleRanges = NULL, # to equalize across determinant scales. e.g., list(det1 = c(1,5), det2 = c(1,7))
                   drawPlot = TRUE, # if FALSE no plot is produced at all (only)
                   useCompleteCases = FALSE, # if TRUE all NAs are omitted from the analysis
                   na.random = FALSE, # if TRUE warning messages on NAs can be suppressed if NAs are random
                   # fullColorRange = NULL,
                   # associationsAlpha = .5,
                   # vLines = c(-0.5, 0, 0.5),
                   # vLineColors = "grey",
                   silent = behaviorchange::opts$get("silent"),
                   ...) {

  ciborg_subquestion_alignment <-
    behaviorchange::opts$get("ciborg_subquestion_alignment");

  ciborg_subquestion_alignment <-
    ifelse(ciborg_subquestion_alignment == "r",
           1,
           ifelse(
             ciborg_subquestion_alignment == "c",
             0.5,
             0
           ));


  if (!requireNamespace("ggdist", quietly = TRUE)) {
    stop("You need the {ggdist} package. ",
         "To install the newest version from CRAN, you can use:\n\n",
         "  install.packages('ggdist');\n");

  }

  if (!requireNamespace("ggtext", quietly = TRUE)) {
    stop("You need at the {ggtext} package. ",
         "To install the newest version from CRAN, you can use:\n\n",
         "  install.packages('ggtext');\n");
  }

  if (!requireNamespace("patchwork", quietly = TRUE)) {
    stop("You need at the {patchwork} package, in at least version ",
         "1.3.0. To install the newest version from CRAN, you can use:\n\n",
         "  install.packages('patchwork');\n");

  } else if (utils::packageVersion("patchwork") < "1.3.0") {
    stop("You need at the {patchwork} package, in at least version ",
         "1.3.0. To install the newest version from CRAN, you can use:\n\n",
         "  install.packages('patchwork');");
  }

  if (!all(c(determinants, target, group) %in% names(data))) {
    stop("Not all variables names you passed in arguments ",
         "'determinants', 'target' or 'group' are in the dataset!\n",
         "Specifically, ",
         ufs::vecTxtQ(c(determinants, target, group)[!(c(determinants, target, group) %in% names(data))]),
         " is or are not in the provided dataset.");
  }

  if (!is.null(output_tables) && !dir.exists(output_tables)) {
    message("The directory '", output_tables, "' does not exist, so not saving the CSV tables!")
    output_tables <- NULL
  }

  theme <- ggplot2::theme_bw(base_size = determinantSize) # removed this out of the arguments (signature)

  res <- list(input = as.list(environment()),
              intermediate = list(),
              output = list());


  ###---------------------------------------------------------------------------
  ### Ensure correct data format and ommit NAs

  # Make sure its a df
  data <- as.data.frame(data) # make sure its a data.frame as users can upload a tibble or matrix

  # Remove rows with NA on the grouping variable (and give warning message)
  na_removed <- sum(is.na(data[[group]]))
  if (na_removed > 0) {
    message(na_removed, " row(s) with NA in the grouping variable were removed before plotting.")
  }
  data <- data[!is.na(data[[group]]), ]

  ###---------------------------------------------------------------------------
  ### useCompleteCases = TRUE to only use complete cases on determinant && target

  if (useCompleteCases) {
    data <- data[stats::complete.cases(data[, c(determinants, target)]), ]
  }

  ###---------------------------------------------------------------------------
  ### Create a factor with the group levels

    groups <- sort(unique(data[, group]));
    groups <- groups[!is.na(groups)];
    names(groups) <- groups;

  data[, group] <-
    factor(
      data[, group],
      levels = groups,
      labels = names(groups),
      ordered = TRUE
    );

  ###---------------------------------------------------------------------------
  ### Check number of groups and warn or stop

  n_groups <- length(groups);

  if (n_groups > 9) {
    stop("More than 9 groups are identified. The plot is not produced as
         this compromises interpretability. The max number of groups is 9.")
  } else if (n_groups >=5) {
    message(">=5 groups are identified. The plot is produced but it may be difficult
    to interpret group differences. The max number of groups is 9.");
  }

  ###---------------------------------------------------------------------------
  ### Check and set titleGroupNames

  # Make group_names with NA with the correct length
  group_names <- rep(NA_character_, n_groups)

  # If user specified titleGroupNames
  if (!is.null(titleGroupNames)) {
    if (length(titleGroupNames) == n_groups) {
      group_names <- titleGroupNames
    } else if (length(titleGroupNames) > n_groups) {
      group_names <- titleGroupNames[1:n_groups]
    } else {  # length(titleGroupNames) < n_groups
      group_names[1:length(titleGroupNames)] <- titleGroupNames
    }
  } else { # if no titleGroupNames are specified it uses names specified earlier
    group_names <- names(groups)
  }

  ###---------------------------------------------------------------------------
  ### Set the group palette

  if (is.null(groupColorPalette)) {

    groupColorPalette <- c("#CC79A7", # reorganized palette_okabe_ito
                           "#009E73",
                           "#0072B2",
                           "#E69F00",
                           "#56B4E9",
                           "#999999",
                           "#F5C710",
                           "#D55E00",
                           "#000000"
                           );

    groupColorPalette <-
      groupColorPalette[
        seq_along(groups)
      ];

    names(groupColorPalette) <- names(groups);
  }

  ###---------------------------------------------------------------------------
  ### Vectors with indices and labels

  determinantIndices <- stats::setNames(
    seq_along(determinants),
    nm = determinants
  )

  ###---------------------------------------------------------------------------
  ### Set subQuestions vector

  # Check number of determinants
  n_det <- length(determinants)

  # Make subQuestions_names with NA with the correct length
  subQuestions_names <- rep(NA_character_, n_det)

  if (!is.null(subQuestions)) {
    if (length(subQuestions) == n_det) {
      subQuestions_names <- subQuestions
    } else if (length(subQuestions) > n_det) {
      subQuestions_names <- subQuestions[1:n_det]
    } else {
      subQuestions_names[1:length(subQuestions)] <- subQuestions
    }
  } else {
    subQuestions_names <- determinants
  }

  subQuestions <- subQuestions_names

  ###---------------------------------------------------------------------------
  ### Check determinant and measurement levels of items

  l_det <- sapply(determinants, function(x) length(unique(stats::na.omit(data[[x]]))));
  names(l_det) <- determinants

if (any(l_det == 2)) {
  message("The following determinants have 2 values: ",
          paste0(names(l_det[l_det ==2]), collapse = ", "),
          "\nIn case any are dichotomous, interpret as point-biserial correlations.\n")
};

if (any(l_det <=1)) {
  message("The following determinants have 0 or 1 value and no variance: ",
          paste0(names(l_det[l_det <=1]), collapse = ", "),
          "\nCorrelations can't be calculated.\n")
}
  ;


  ###---------------------------------------------------------------------------
  ### Check n of targets (one is allowed) and measurement level

  n_targets <- length(target);
  if (n_targets > 1) {
    stop("Only one target variable is allowed, but ", n_targets, " were provided.")
  };

  l_target <- length(unique(stats::na.omit(data[[target]])));

  if (l_target == 2) {
    message("The target ", target, " has only two unique values.\nIn case it is dochotomous interpret as point-biserial correlations.\n")
  } else if (l_target < 2) {
    stop("The target ", target, " has ", l_target, " value(s) and no variance.
         The correlations can't be calculated/n.")
  };


  ###---------------------------------------------------------------------------
  ### If determinants target are both dichotomous then message on point
  if (any(l_det == 2) && (l_target == 2)) {
    message("The target ", target, " has only two unique values. \n And, the following determinants have 2 unique values: ",
    paste0(names(l_det[l_det ==2]), collapse = ", "),
    "\nIn case target and determinants are both dichotomous, interpret as phi coefficients.\n")
    }

  ###---------------------------------------------------------------------------
  ### Set Anchors vector

  # Make leftAnchors_names with Lo with the correct length
  leftAnchors_names <- rep("Lo", n_det)

  if (!is.null(leftAnchors)) {
    if (length(leftAnchors) == n_det) {
      leftAnchors_names <- leftAnchors
    } else if (length(leftAnchors) > n_det) {
      leftAnchors_names <- leftAnchors[1:n_det]
    } else {
      leftAnchors_names[1:length(leftAnchors)] <- leftAnchors
    }
  }

  leftAnchors <- leftAnchors_names

  # Make rightAnchors_names with NA with the correct length
  rightAnchors_names <- rep("Hi", n_det)

  if (!is.null(rightAnchors)) {
    if (length(rightAnchors) == n_det) {
      rightAnchors_names <- rightAnchors
    } else if (length(rightAnchors) > n_det) {
      rightAnchors_names <- rightAnchors[1:n_det]
    } else {
      rightAnchors_names[1:length(rightAnchors)] <- rightAnchors
    }
  }

  rightAnchors <- rightAnchors_names

  ## --- labels ---
  labels <- paste0("**", subQuestions, "**<br>[", leftAnchors, "-", rightAnchors, "]")

  ###---------------------------------------------------------------------------
  ### Set scale range of the LeftPlot based on range or normalize input

  if (!is.null(scaleRanges)) {

    if (!is.null(leftScaleRange)) {
      message("Both 'scaleRanges' and 'leftScaleRange' are specified. ",
              "'scaleRanges' is prioritized over 'leftScaleRange'")
    }

    missing_ranges <- determinants[!determinants %in% names(scaleRanges)]
    if (length(missing_ranges) > 0) {
      stop("The following determinants are missing from 'scaleRanges':\n",
           ufs::vecTxtQ(missing_ranges),
           "\nPlease provide a theoretical min and max for each determinant!");
    }

    for (det in determinants) {
      det_min <- scaleRanges[[det]][1]
      det_max <- scaleRanges[[det]][2]
      data[[det]] <- (data[[det]] - det_min) / (det_max - det_min)
    }

    leftScaleRange <- c(0, 1)

  } else if (is.null(leftScaleRange)) {
    leftScaleRange <- c(
      min(data[, determinants], na.rm = TRUE),
      max(data[, determinants], na.rm = TRUE)
    )
  }

  ###---------------------------------------------------------------------------
  ### Make long ('tidy') data frame

  res$intermediate$determinantDat_long <-
    data.frame(
      determinant = rep(determinants, each = nrow(data)),
      subQuestion = rep(subQuestions, each = nrow(data)),
      leftAnchor = rep(leftAnchors, each = nrow(data)),
      rightAnchor = rep(rightAnchors, each = nrow(data)),
      label = rep(labels, each = nrow(data)),
      determinantIndex = rep(determinantIndices, each = nrow(data)),
      group = rep(data[, group], times = length(determinants)),
      value = unlist(data[, determinants])
    );

  ### Reverse plotting order of groups (first group on top),
  res$intermediate$determinantDat_long$group <-
    factor(
      res$intermediate$determinantDat_long$group,
      levels = rev(names(groups)),
      ordered = TRUE
    );

  ### Store determinants as a factor to preserve order
  res$intermediate$determinantDat_long$determinant <-
    factor(
      res$intermediate$determinantDat_long$determinant,
      levels = rev(determinants),
      labels = rev(determinants),
      ordered = TRUE
    );

  res$intermediate$determinantDat_long <-
    res$intermediate$determinantDat_long[
      !is.na(res$intermediate$determinantDat_long$value),
    ];

  res$intermediate$datForCors <- data[, c(determinants, target, group)];

  res$intermediate$meanPartDats <- list();
  res$intermediate$assocPartDats <- list();

  groupLevels <- names(groups);

  ###---------------------------------------------------------------------------
  ### Compute means and correlations per group
  ###---------------------------------------------------------------------------

  for (currentGroup in groupLevels) {

    tmpDat <- res$intermediate$datForCors[
      res$intermediate$datForCors[[group]] == currentGroup,
    ];

    res$intermediate$assocPartDats[[currentGroup]] <-
      sapply(target, function(currentTarget) {

        tryCatch({
          tmpCors <-
            lapply(tmpDat[, determinants, drop=FALSE],
                   stats::cor.test,
                   tmpDat[, currentTarget]);
        }, error = function(errorMsg) {
          stop("Error when computing correlation confidence intervals. ",
               "Determinants: ", ufs::vecTxt(determinants, useQuote="`"), # added ufs::
               ". Target: `", currentTarget, "`. ",
               "The error message was: ", errorMsg);
        });

        res <-
          data.frame(
            determinant = determinants,
            determinantIndex = determinantIndices,
            target = currentTarget,
            group = currentGroup,
            lo=unlist(lapply(tmpCors, function(x) return(x$conf.int[1]))),
            es=unlist(lapply(tmpCors, function(x) return(x$estimate))),
            hi=unlist(lapply(tmpCors, function(x) return(x$conf.int[2]))),
            n=unlist(lapply(tmpCors, function(x) return(x$parameter + 2))) # to add n to the output (df+2)
          );

        res$width <- res$hi - res$lo;

        return(res);

      }, simplify=FALSE);

    res$intermediate$assocPartDats[[currentGroup]] <-
      do.call(
        rbind,
        res$intermediate$assocPartDats[[currentGroup]]
      );

    ###-------------------------------------------------------------------------
    ### CIs for means
    ###-------------------------------------------------------------------------

    res$intermediate$meanPartDats[[currentGroup]] <-
      ufs::rbind_df_list(
        lapply(tmpDat[, determinants, drop=FALSE],
               function(vector) {
                 confIntObject <-
                   ufs::meanConfInt(
                     vector,
                     conf.level = conf.level$means # I think here an -s was missing (see line 109)
                   );
                 res <- as.data.frame(confIntObject$output$ci);
                 res$mean <- mean(vector, na.rm=TRUE);
                 res$width <- res$ci.hi - res$ci.lo;
                 res$n <- sum(!is.na(vector)); # to add n to the output
                 return(res);
               })
      );

    res$intermediate$meanPartDats[[currentGroup]]$determinant <-
      determinants;

    res$intermediate$meanPartDats[[currentGroup]]$determinantIndex <-
      determinantIndices[res$intermediate$meanPartDats[[currentGroup]]$determinant];

    res$intermediate$meanPartDats[[currentGroup]]$group <-
      rep(currentGroup, length(determinants));

  }

  ###---------------------------------------------------------------------------
  ### Combining into one dataframe for correlations

  res$intermediate$assocPartDats <-
    do.call(
      rbind,
      res$intermediate$assocPartDats
    );

  ### Restore factor
  res$intermediate$assocPartDats$group <-
    factor(
      res$intermediate$assocPartDats$group,
      levels = names(groups),
      labels = names(groups),
      ordered = TRUE
    );

  res$intermediate$assocPartDats_long <-
    data.frame(
      determinant = rep(res$intermediate$assocPartDats$determinant, 3),
      determinantIndex = rep(determinantIndices[res$intermediate$assocPartDats$determinant], 3),
      target = rep(res$intermediate$assocPartDats$target, 3),
      group = rep(res$intermediate$assocPartDats$group, 3),
      is_point = rep(c(0, 1, 0), each = nrow(res$intermediate$assocPartDats)),
      value = c(res$intermediate$assocPartDats$lo,
                res$intermediate$assocPartDats$es,
                res$intermediate$assocPartDats$hi),
      width = rep(res$intermediate$assocPartDats$width, 3)
    );

  ### Reverse plotting order of groups for correlations (first group on top)
  res$intermediate$assocPartDats_long$group <-
    factor(
      res$intermediate$assocPartDats_long$group,
      levels = rev(names(groups)),
      ordered = TRUE
    );

  res$intermediate$assocPartDats_long$slabHeight <-
    res$intermediate$assocPartDats_long$is_point /
    res$intermediate$assocPartDats_long$width;

  ### Make sure the associations are plotted in order of the determinants
  res$intermediate$assocPartDats_long$determinant <-
    factor(
      res$intermediate$assocPartDats_long$determinant,
      levels = rev(determinants),
      labels = determinants,
      ordered = TRUE
    );

  ###---------------------------------------------------------------------------
  ### Combining into one dataframe for means

  res$intermediate$meanPartDats <-
    do.call(
      rbind,
      res$intermediate$meanPartDats
    );

  ### Restore factor
  res$intermediate$meanPartDats$group <-
    factor(
      res$intermediate$meanPartDats$group,
      levels = names(groups),
      labels = names(groups),
      ordered = TRUE
    );

  res$intermediate$meanPartDats_long <-
    data.frame(
      determinant = rep(res$intermediate$meanPartDats$determinant, 3),
      determinantIndex = rep(determinantIndices[res$intermediate$meanPartDats$determinant], 3),
      group = rep(res$intermediate$meanPartDats$group, 3),
      is_point = rep(c(0, 1, 0), each = nrow(res$intermediate$meanPartDats)),
      value = c(res$intermediate$meanPartDats$ci.lo,
                res$intermediate$meanPartDats$mean,
                res$intermediate$meanPartDats$ci.hi),
      width = rep(res$intermediate$meanPartDats$width, 3)
    );

  ### Reverse plotting order of groups for means
  res$intermediate$meanPartDats_long$group <-
    factor(
      res$intermediate$meanPartDats_long$group,
      levels = rev(names(groups)),
      ordered = TRUE
    );

  res$intermediate$meanPartDats_long$slabHeight <-
    res$intermediate$meanPartDats_long$is_point /
    res$intermediate$meanPartDats_long$width;

  ###---------------------------------------------------------------------------
  ### Creating left plot
  ###---------------------------------------------------------------------------

  res$intermediate$leftPlot <-
    ggplot2::ggplot(
      data = res$intermediate$determinantDat_long,
      mapping = ggplot2::aes(
        x = .data$value,
        y = .data$determinant, #determinantIndex,
        fill = .data$group,
        group = .data$group
      )
    ) +
    ggplot2::geom_point(
      shape = 21,
      size = dotSize,
      alpha = .2,
      mapping = ggplot2::aes(fill = .data$group, color = "black"),
      position = ggplot2::position_jitterdodge(
          jitter.width = jitterHeight, # this actually adjusts the height
          jitter.height = jitterWidth, # this actually adjusts the width
          dodge.width = .6,
        )
    ) +

    ###-------------------------------------------------------------------------
    ### Diamond layer
    ggdist::geom_slab(
      data = res$intermediate$meanPartDats_long,
      mapping = ggplot2::aes(x = .data$value,
                             y = .data$determinant,
                             thickness = .data$slabHeight),
      position = ggdist::position_dodgejust(width = .6),
      normalize = "groups",
      side = "both",
      scale = 0.35,
      alpha = 0.80,
      color = strokeColors[1],
      linewidth = 0.5
    ) +

    ###-------------------------------------------------------------------------
    ### General settings

    ggplot2::scale_fill_manual( # changed to manual to plot groups in correct order
      values = groupColorPalette,
      breaks = names(groups),
      limits = names(groups)
    ) +

    ggplot2::scale_x_continuous(
      breaks = pretty(leftScaleRange),
      limits = c(leftScaleRange[1] -.5, leftScaleRange[2] + 0.5)
    ) +

    #ggplot2::scale_y_continuous(
    ggplot2::scale_y_discrete(
      breaks = determinants,
      labels = labels #leftAnchors
      # ,
      # sec.axis =
      #   ggplot2::dup_axis(
      #     labels = rightAnchors
      #   )
    ) +

    theme +

    ggplot2::guides(
      color = ggplot2::guide_none()
    ) +

    ggplot2::labs(
      x = paste0("Means and ", round(100 * conf.level$means, 2), "% CIs"),
      y = NULL,
      group = group,
      fill = group
    ) +

    ggplot2::theme(
      legend.position = "none", # set to none as title is legend
      axis.title.x = ggplot2:: element_text(size = axisSize, color = "dimgrey"),
      axis.text.x = ggplot2::element_text(size = axisSize * .5),
      axis.text.y = ggtext::element_markdown(hjust = ciborg_subquestion_alignment)
    );

  ###---------------------------------------------------------------------------
  ### Right plot
  ###---------------------------------------------------------------------------

  res$intermediate$rightPlot <-
    ggplot2::ggplot(
      data = res$intermediate$assocPartDats_long,
      mapping = ggplot2::aes(x = .data$value,
                             y = .data$determinant, #determinantIndex,
                             fill = .data$group,
                             thickness = .data$slabHeight)
    ) +
    ggdist::geom_slab(
      data = res$intermediate$assocPartDats_long,
      position = ggdist::position_dodgejust(width = .6),
      normalize = "groups",
      side = "both",
      scale = 0.35,
      alpha = 0.80,
      color = strokeColors[2],
      linewidth = 0.5
    ) +

    ###-------------------------------------------------------------------------
    ### General settings

    ggplot2::coord_cartesian(xlim = c(-1, 1)) +

    ggplot2::scale_fill_manual(
      values = groupColorPalette,
      breaks = names(groups),
      limits = names(groups)
    ) +

    ggplot2::scale_x_continuous(
      breaks = c(-1, -.5, 0, .5, 1),
      labels = c("-1", "-.5", "0", ".5", "1")
    ) +

    #ggplot2::scale_y_continuous(
    ggplot2::scale_y_discrete(
      breaks = determinants, #determinantIndices,
      labels = labels, #leftAnchors
    ) +

    # ggplot2::scale_fill_discrete(groupColorPalette) +
    # ggplot2::scale_color_discrete(groupColorPalette) +

    theme +

    ggplot2::guides(
      color = ggplot2::guide_none(),
      fill = ggplot2::guide_none(),
      group = ggplot2::guide_none()
    ) +

    ggplot2::labs(
      x = paste0("Associations and ", round(100 * conf.level$association, 2), "% CIs"),
      y = NULL
    ) +

    ggplot2::theme(
      legend.position = "none", # set to none as title is legend
      axis.title.x = ggplot2:: element_text(size = axisSize, color = "dimgrey"),
      axis.text.x = ggplot2::element_text(size = axisSize * 0.5),
      axis.text.y = ggplot2::element_blank()
      );


  ###---------------------------------------------------------------------------
  ### Calculate Explained Variance
  ###---------------------------------------------------------------------------
  if (rsq) {

    data_split_by_group  <- split(data, data[[group]]) # split data by group

    results_R2 <- lapply(data_split_by_group, function(group_R2) { # loop over the list with groups
      formula <- stats::as.formula(paste(target, "~", paste(determinants, collapse = " + "))) # Makes the formula --> y (target) ~ x (determinant_1) + x (determinant_2) + ...
      behaviorchange::lm_rSq_ci( # uses lm function from the behavior change package
        formula    = formula, # inserts the formula (y ~ x + x + ...)
        data       = group_R2, # uses the lapply currentGroup per itteration
        conf.level = 0.95
      )
    })

    ### make output results_R2 with two decimals
    R2_CIs <- lapply(results_R2, function(group_CIs) {
      round(group_CIs, 2) # Extract all output (it are only lo - hi CI) and round to 2 decimals
    })

    ### makes it text and combines lo-hi CIs with -
    R2_text_parts <- lapply(R2_CIs, function(text) {
      paste(text, collapse = "-")
    })
  } else { # make an empty list with groupsnames (needed for the colored title legend)
    R2_text_parts <- as.list(rep("", length(groups)))
    names(R2_text_parts) <- names(groups)
  }

  ###---------------------------------------------------------------------------
  ### Make colored title with R2 to serve as legend
  ###---------------------------------------------------------------------------
  ### Example: https://www.infoworld.com/article/2256014/add-color-to-your-ggplot2-text-in-r.html

  combined_title_parts <- c()

  ### loop to combine name, R2, color in combined_title_parts
  for(x in seq_along(group_names)) {
    name <- group_names[x]
    R2   <- if (rsq) R2_text_parts[[x]] else ""
    color <- groupColorPalette[x]

    new_title_part <- paste0(
      "<span style='color:", color, "'>**", name, "**</span>",
      if (rsq) paste0("<sup>[R\uB2=", R2, "]</sup>") else "" # [R^2=...] is only added when rsq=TRUE
    )                                                        # the unicode for R^2 is \uB2

    combined_title_parts <- c(combined_title_parts, new_title_part)
  }

  ### write out combined_title_parts seperated with comma's
  title_text <- paste(combined_title_parts, collapse = ", ")

  ### allow users to set a costum target name in title
  target_label <- if(!is.null(titleVarLabels)) titleVarLabels else target

  ### make title (allow users to specify prefix and suffix)
  Title <- paste0(
    titlePrefix, " ", target_label, " for ", title_text, " ", titleSuffix
  )

  ###---------------------------------------------------------------------------
  ### Combine plots
  res$output$plot <-
    patchwork::wrap_plots(
      res$intermediate$leftPlot,
      res$intermediate$rightPlot,
      # guides = "collect", # coded this out as there are no legends to collect
      axes = "collect"
    ) +
    patchwork::plot_annotation(
      title = Title,
      caption = if (!is.null(FootnoteText)) FootnoteText else NULL,
      theme = ggplot2::theme(
        # legend.position = 'none', # coded this out as there are no legends
        plot.title = ggtext:: element_textbox(
          size = titleSize,
          halign = 0, # align the text to the left
          lineheight = 1.1,
          width = grid::unit(1, "npc") # width is 100% of the plot width (relative = Normalized Parent Coordinates)
        ),
        plot.caption = if (!is.null(FootnoteText)) ggtext::element_textbox(
          size = titleSize * .85,
          color = "dimgrey",
          halign = 0,
          width = grid::unit(1, "npc")
        )
        else NULL,
        plot.margin = ggplot2::margin(0.01, 0, 0.01, 0, unit = "npc")  # long title + footnote and texts fall of at botum
      )                                                # margins npc (relative %) goes clockwise (top, richt, bottum, left)
    );

  ###---------------------------------------------------------------------------
  ### Store output matrices + n + add determinant names

  subQuestion_CSV <- if (!is.null(subQuestions)) {
    stats::setNames(subQuestions, determinants) }
                      else NULL

  # Means: Extract relevant metrics from meanPartDats
  res$output$means <- data.frame(
    res$intermediate$meanPartDats[, c("determinant", "group", "n")],
    round(res$intermediate$meanPartDats[, c("mean", "ci.lo", "ci.hi")], 2)
  )
    if (!is.null(subQuestion_CSV)) {
      res$output$means$subQuestion <- unname(subQuestion_CSV[as.character(res$intermediate$meanPartDats$determinant)])
      res$output$means <- res$output$means[, c("determinant", "subQuestion", "group", "n", "mean", "ci.lo", "ci.hi")]
  }

  # Associations: Extract relevant metrics from assocPartDat
  res$output$associations <- data.frame(
    res$intermediate$assocPartDats[, c("determinant", "group", "n")],
    round(res$intermediate$assocPartDats[, c("es", "lo", "hi")], 2)
  )
    if (!is.null(subQuestion_CSV)) {
      res$output$associations$subQuestion <- unname(subQuestion_CSV[as.character(res$intermediate$assocPartDats$determinant)])
      res$output$associations <- res$output$associations[, c("determinant", "subQuestion", "group", "n", "es", "lo", "hi")]
  }

  # Export CSV if a output path is specified
  if (!is.null(output_tables)) {
    utils::write.csv(res$output$means, file.path(output_tables, "means.csv"), row.names = FALSE)
    utils::write.csv(res$output$associations, file.path(output_tables, "associations.csv"), row.names = FALSE)
  }


  ###---------------------------------------------------------------------------
  ### Check for n differences between determinants & target
  ###---------------------------------------------------------------------------

  n_comparison <- merge(
    res$output$means[, c("determinant", "group", "n")],
    res$output$associations[, c("determinant", "group", "n")],
    by = c("group", "determinant"),
    suffixes = c("_means", "_associations")
  )

  n_comparison$diff_pct <- round((n_comparison$n_means - n_comparison$n_associations) / n_comparison$n_means * 100, 1)

  if (any(n_comparison$diff_pct >= 10) && !na.random) {
    message("The left (means) and right (associations) plots are based on different sample sizes.",
            "A difference of >=10% was detected for at least one determinant.",
            "Please inspect the table below.",
            "If NAs are not random consider using useCompleteCases = TRUE to analyze complete cases.",
            "If NAs are considered random this warning message can be suppressed using na.random = TRUE")
    print(n_comparison)
  }

  ###---------------------------------------------------------------------------
  ### Post processing, drawing, saving, etc
  ###---------------------------------------------------------------------------

  ### Default sizes ; first compute in centimeters, then convert to inches
  attr(res$output$plot, 'height') <- determinantSize + 1.25 * determinantSize * max(length(determinants), 1.5);
  attr(res$output$plot, 'width') <- 21 - 3;
  attr(res$output$plot, 'height') <- attr(res$output$plot, 'height') / 2.54;
  attr(res$output$plot, 'width') <- attr(res$output$plot, 'width') / 2.54;

  if (drawPlot) {
    grid::grid.newpage();
    grid::grid.draw(res$output$plot);
  }

  if (!is.null(output_plot)) {
    if ((nchar(dirname(output_plot)) == 0) | (!dir.exists(dirname(output_plot)))) {
      message("The directory specified to save the the output_plot to ('",
              dirname(output_plot),
              "') does not exist, so not saving the plot!");
    } else {
      if (is.null(outputWidth)) {
        outputWidth <- attr(res$output$plot, 'width');
        outputUnits <- "in";
      }
      if (is.null(outputHeight)) {
        outputHeight <- attr(res$output$plot, 'height');
        outputUnits <- "in";
      }
      if (is.null(outputUnits)) {
        outputUnits <- "in";
      }
      do.call(ggplot2::ggsave,
              c(list(file=output_plot,
                     plot=res$output$plot,
                     width = outputWidth,
                     height = outputHeight,
                     units = outputUnits),
                outputParams));
    }
  }
  invisible(res)
}



### Avoiding the R CHECK error
if(getRversion() >= "2.15.1") { invisible(utils::globalVariables(".data")); };
