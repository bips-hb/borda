#' Create a Box Plot
#'
#' Creates and returns a box plot with the results
#'
#' @export
plotLinePlot <-
  function(data,
           ylim = NULL,
           xlim = c(0, 100100), 
           title = "",
           xlabel = "Number of Patients Exposed",
           ylabel = "Average AUC",
           font_size = 12,
           show_baseline = TRUE,
           auc_baseline = .5,
           process_alpha = FALSE,
           fancy_labels = TRUE,
           smooth = FALSE, 
           logtransform = FALSE,
           show_legend = TRUE, 
           span = 1.1, # regulates the smoothness
           algorithms = c("number_of_reports", 
                          "ppoisson",
                          "chi2",
                          "chi2YatesYates",
                          "GPS0", 
                          "GPS0.025" ,
                          "GPS0.05" ,
                          "ICAlt0",
                          "ICAlt0.025",
                          "ICAlt0.05",
                          "ICOrig0", 
                          "ICOrig0.025",
                          "ICOrig0.05",
                          "midRFETmid",
                          "RF", 
                          "RF_corrected",
                          "RF_permutation",
                          "RFET", 
                          "ROR0",
                          "ROR0.025",
                          "ROR0.05",
                          "PRR0",
                          "PRR0.025",
                          "PRR0.05",
                          "LGPS0",
                          "LGPS0.025",
                          "LGPS0.05"
                          )) {
    
    
    if (!is.null(algorithms)) { 
      data <- data %>% dplyr::filter(algorithm %in% algorithms)
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
    
    # summarize data
    data <- data %>% 
      group_by(algorithm, n_patients_exposed) %>% 
      summarise(
        mean = mean(rocauc), 
        n = n(), 
        CI = 1.96 * sd(rocauc) / sqrt(n)
      )

    if (is.null(ylim)) {
      ylim <- c(max(0.0, min(data$mean - data$CI, na.rm = TRUE) - 0.01),
                min(1.0, max(data$mean + data$CI, na.rm = TRUE) + 0.01))
    }
    
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
    }
    
    
    data$ml <- as.vector(unlist(lapply(
      data$algorithm_label
    , TeX))) 
  
    
    # 
    # ml <- unname(TeX(unique(as.factor(data$algorithm_label)))) 
    # 
    # Standard error of the mean
    
    if (smooth) { 
      p <- ggplot(data, aes(x = n_patients_exposed, y = mean, colour = algorithm_label)) +
        geom_smooth(se = FALSE, span = span) 
    } else {
      p <- ggplot(data, aes(x = n_patients_exposed, y = mean, colour = algorithm_label)) +
        geom_errorbar(aes(ymin= mean - CI, ymax = mean + CI), width=.1) +
        geom_line() +
        geom_point()
    }
    
    p <- p +
      scale_color_discrete(name = "Method", breaks = unique(data$algorithm_label), labels = data$ml) +
      xlab(xlabel) +
      ylab(ylabel) +
      scale_x_continuous(limits = xlim, expand = c(0, 0))
      ggtitle(title) +
      theme_bw() +
      theme(
        axis.text = element_text(size = font_size),
        axis.title = element_text(size = font_size, face = "bold")
      )
      
    if (logtransform) { 
      p <- p + scale_y_continuous(limits = ylim, expand = c(0, 0), trans='log2') 
    } else {
      p <- p + scale_y_continuous(limits = ylim, expand = c(0, 0)) 
    }
    
    if (show_baseline) { 
      p <- p + geom_hline(yintercept = auc_baseline,
                              linetype = "dashed",
                              color = "black")
    }
      
    if (!show_legend) {
      p + guides(colour =FALSE)
    }
      
    return(p + theme_bw(base_size = font_size))
  }