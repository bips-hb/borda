#' Create a Box Plot
#'
#' Creates and returns a box plot with the results
#'
#' @export
plotBoxplot <-
  function(data,
           n_patients = 10 ^ 4,
           #probability_exposed_year = 0.1,
           ylim = NULL,
           title = sprintf("No. of patients: %g", n_patients),
           ylabel = NULL,
           measure = "rocauc",
           sort = "mean",
           font_size = 12,
           show_baseline = TRUE,
           auc_baseline = NULL,
           process_alpha = FALSE,
           fancy_labels = TRUE) {
    if (show_baseline & is.null(auc_baseline)) {
      auc_baseline <- .5
      if (measure == "prcauc_rare") {
        auc_baseline <- 30 / 80
      }
    }
    
    if (is.null(ylabel)) {
      if (grepl("prc", measure)) {
        ylabel <- "Area under the Precision-Recall Curve"
      } else {
        ylabel <- "Area under the Receiver Operating Curve"
      }
    }
    
    n_patients_ <- n_patients
    data <- dplyr::filter(data,
                          n_patients_exposed == n_patients_)
   
    if (is.null(ylim)) {
      if (!is.null(auc_baseline)) {
        ylim <- c(max(0.0, min(
          auc_baseline, min(data[[measure]], na.rm = TRUE)
        ) - 0.01),
        min(1.0, max(data[[measure]], na.rm = TRUE) + 0.01))
      } else {
        ylim <- c(max(0.0, min(data[[measure]], na.rm = TRUE) - 0.01),
                  min(1.0, max(data[[measure]], na.rm = TRUE) + 0.01))
      }
    }
    
    # combine the name with alpha
    if (process_alpha) {
      for (i in 1:nrow(data)) {
        if (!is.na(data[i,]$alpha)) {
          if (data[i,]$alpha == .025) {
            data[i,]$algorithm <- sprintf("%s0.025", data[i,]$algorithm)
          } else {
            data[i,]$algorithm <- sprintf("%s0.05", data[i,]$algorithm)
          }
        }
      }
    }
    
    # sort the algorithm
    data <- data %>% dplyr::arrange(algorithm)
    
    # create nicer looking labels
    data$algorithm_label <- data$algorithm
    
    if (fancy_labels) {
      data$algorithm_label <-
        replace(
          data$algorithm_label,
          data$algorithm_label == "number_of_reports",
          "$#$ co-occurences"
        )
      data$algorithm_label <-
        replace(data$algorithm_label,
                data$algorithm_label == "chi2",
                "$\\chi^2$")
      data$algorithm_label <-
        replace(
          data$algorithm_label,
          data$algorithm_label == "chi2YatesYates",
          "$\\chi^2_{Yates}$"
        )
      data$algorithm_label <-
        replace(data$algorithm_label,
                data$algorithm_label == "GPS0",
                "GPS")
      data$algorithm_label <-
        replace(data$algorithm_label,
                data$algorithm_label == "GPS0.025",
                "$GPS_{025}$")
      data$algorithm_label <-
        replace(data$algorithm_label,
                data$algorithm_label == "GPS0.05",
                "$GPS_{05}$")
      data$algorithm_label <-
        replace(data$algorithm_label,
                data$algorithm_label == "ICAlt0",
                "$IC^{alternative}$")
      data$algorithm_label <-
        replace(
          data$algorithm_label,
          data$algorithm_label == "ICAlt0.025",
          "$IC^{alternative}_{025}$"
        )
      data$algorithm_label <-
        replace(
          data$algorithm_label,
          data$algorithm_label == "ICAlt0.05",
          "$IC^{alternative}_{05}$"
        )
      data$algorithm_label <-
        replace(data$algorithm_label,
                data$algorithm_label == "ICOrig0",
                "$IC^{original}$")
      data$algorithm_label <-
        replace(
          data$algorithm_label,
          data$algorithm_label == "ICOrig0.025",
          "$IC^{original}_{025}$"
        )
      data$algorithm_label <-
        replace(
          data$algorithm_label,
          data$algorithm_label == "ICOrig0.05",
          "$IC^{original}_{05}$"
        )
      data$algorithm_label <-
        replace(
          data$algorithm_label,
          data$algorithm_label == "lbinomial",
          "$\\Lambda_{binomial}$"
        )
      data$algorithm_label <-
        replace(data$algorithm_label,
                data$algorithm_label == "LGPS0.025",
                "$LGPS_{025}$")
      data$algorithm_label <-
        replace(data$algorithm_label,
                data$algorithm_label == "LGPS0.05",
                "$LGPS_{05}$")
      data$algorithm_label <-
        replace(data$algorithm_label,
                data$algorithm_label == "LGPS0",
                "LGPS")
      data$algorithm_label <-
        replace(data$algorithm_label,
                data$algorithm_label == "highest_lambda",
                "LASSO")
      data$algorithm_label <-
        replace(data$algorithm_label,
                data$algorithm_label == "midRFETmid",
                "midRFET")
      data$algorithm_label <-
        replace(data$algorithm_label,
                data$algorithm_label == "ppoisson",
                "$p_{Poisson}$")
      data$algorithm_label <-
        replace(data$algorithm_label,
                data$algorithm_label == "PRR0",
                "PRR")
      data$algorithm_label <-
        replace(data$algorithm_label,
                data$algorithm_label == "PRR0.025",
                "$PRR_{025}$")
      data$algorithm_label <-
        replace(data$algorithm_label,
                data$algorithm_label == "PRR0.05",
                "$PRR_{05}$")
      data$algorithm_label <-
        replace(data$algorithm_label, data$algorithm_label == "Q", "$Q$")
      data$algorithm_label <-
        replace(data$algorithm_label,
                data$algorithm_label == "Q025",
                "$Q_{025}$")
      data$algorithm_label <-
        replace(data$algorithm_label,
                data$algorithm_label == "Q05",
                "$Q_{05}$")
      data$algorithm_label <-
        replace(data$algorithm_label,
                data$algorithm_label == "RFET",
                "RFET")
      data$algorithm_label <-
        replace(data$algorithm_label,
                data$algorithm_label == "ROR0",
                "ROR")
      data$algorithm_label <-
        replace(data$algorithm_label,
                data$algorithm_label == "ROR0.025",
                "$ROR_{025}$")
      data$algorithm_label <-
        replace(data$algorithm_label,
                data$algorithm_label == "ROR0.05",
                "$ROR_{05}$")
      data$algorithm_label <-
        replace(data$algorithm_label, data$algorithm_label == "RRR", "RRR")
      data$algorithm_label <-
        replace(data$algorithm_label,
                data$algorithm_label == "RF_all_drugs",
                "RF (all drugs)")
      data$algorithm_label <-
        replace(data$algorithm_label,
                data$algorithm_label == "RF_corrected",
                "$RF_{corrected}$")
    }
    
    data$value <- data[[measure]]
    
    # sort the results by 'mean', 'median' or alphabetically
    if (sort == "mean") {
      data <- data %>% group_by(algorithm) %>%
        mutate(score = mean(value, na.rm = TRUE)) %>%
        dplyr::ungroup() %>%
        dplyr::arrange(score)
    } else if (input$sorted == "median") {
      data <- data %>% group_by(algorithm) %>%
        mutate(score = median(value, na.rm = TRUE)) %>%
        dplyr::ungroup() %>%
        dplyr::arrange(score)
    } else {
      data <- data %>% dplyr::mutate(score = row_number())
    }
    
    data$algorithm <-
      factor(data$algorithm, levels = unique(data$algorithm[order(data$score)]))
    data$algorithm_label <-
      factor(data$algorithm_label, levels = unique(data$algorithm_label[order(data$score)]))
    ml <-
      as.vector(unlist(lapply(unique(
        data$algorithm_label
      ), TeX)))
    
    p <- ggplot(data) +
      geom_boxplot(aes(x = algorithm, y = value), fill = 'grey90') +
      ylab(ylabel)
    
    if (!is.null(auc_baseline)) {
      p <-
        p + geom_hline(yintercept = auc_baseline,
                       linetype = "dashed",
                       color = "black")
    }
    
    p + scale_x_discrete(breaks = unique(data$algorithm), labels = ml) +
      scale_y_continuous(limits = ylim, expand = c(0, 0)) +
      coord_flip() +
      xlab("") +
      ggtitle(title) +
      theme_bw() +
      theme(
        legend.position = "none",
        axis.text = element_text(size = font_size),
        axis.title = element_text(size = font_size)
      )
  }