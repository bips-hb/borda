# Auxilary function for creating the standard dataset given CSV file: 
create_rds_file_default_dataset <- function(path = "data/raw_rivaroxaban_dataset.csv") {
  raw_data <- read.csv(path, encoding = "latin1") 
  dataset <- createDatasetObject(raw_data) 
  write_rds(dataset, "data/rivaroxaban_dataset.rds")
}

#' Compute Borda Ranking
#'
#' Computes the Borda ranking given a 'raw_data' table, i.e., 
#' a dataframe/table where each column contains the ranking of 
#' an individual method. 
#'
#' @param raw_data Raw data frame 
#'
#' @return the 'raw_data' with a 'Borda' column with the Borda ranking
#' @export
computeBordaRanking <- function(raw_data) { 
  
  raw_data[,'borda_count'] <- rowSums(raw_data[,3:length(raw_data)]) 
  raw_data <- raw_data[order(raw_data$borda_count), ]
  raw_data[,'Borda'] <- rank(raw_data$borda_count)
  
  # compute the relative rank 
  raw_data$relative_rank <- (raw_data$Borda - 1) / (nrow(raw_data) - 1)

  return(subset(raw_data, select = -borda_count))
}


#' Get Lower Triangular Matrix
#'
#' Returns the lower triangular of a given matrix
#'
#' @param matrix A square matrix 
#'
#' @return A square matrix where the upper triangular 
#'              part is filled with 'NA'
#' @export
get_lower_tri <- function(matrix) {
  matrix[upper.tri(matrix)] <- NA
  return(matrix)
}

#' Compute Kendall's Tau
#'
#' Returns a Kendall's Tau correlation matrix given a raw data table. 
#'
#' @param matrix raw_data Raw data table 
#'
#' @return A square matrix with the Kendall's Tau of the rankings 
#'              of the methods
#' @export
computeKendallsTau <- function(raw_data) { 
  raw_data <- raw_data[, -c(1,2)]
  
  return(
    get_lower_tri(
      cor(raw_data, method = 'kendall')
    )
  )
}

#' Return Kendall's Tau Correlation Matrix plot
#'
#' Returns a heatmap of a given correlation matrix.  
#'
#' @param matrix raw_data Raw data table 
#'
#' @return Plot of the Kendall's Tau correlation matrix
#' @export
createKendallCorrelationPlot <- function(cormat) { 
  melted_cormat <- reshape2::melt(cormat, na.rm = TRUE)

  p <- ggplot(data = melted_cormat, aes(Var2, Var1, fill = value)) +
    geom_tile(color = "white") +
    scale_fill_gradient2(
      low = "white",
      high = "#1763AA",
      midpoint = 0,
      limit = c(0, 1),
      space = "Lab",
      name = "Kendall's tau"
  ) +
    geom_text(aes(label = round(value, 2))) +
    theme_minimal() +
    theme(axis.text.x = element_text(
    angle = 45,
    vjust = 1,
    hjust = 1
  )) +
    xlab("") +
    ylab("") +
    coord_fixed()

  return(p)
}


createICDPlot <- function(borda_dataset, show_icd_codes = FALSE) { 

  p <-  ggplot(borda_dataset, aes(x = relative_rank, y = 0, label = ICD)) +
      # create axis
      annotate("segment", x = 0, xend = 1, y = 0, yend = 0, size = .5) +
      geom_point(size = 3, color = "blue", alpha = 0.5) + #, position=position_jitter(width=0.1,height=00.1)) +
      scale_x_continuous(limits = c(1, 0), expand = c(0.01, 0.01), trans = "reverse") +
      scale_y_continuous(limits = c(-.1, .1), expand = c(0, 0)) +
      geom_text(x = 0, y = -.02, label = "0", color = "black") +
      geom_text(x = -0.25, y= -.02, label="0.25", color = "black") +
      geom_text(x = -.5, y= -.02, label="0.5", color = "black") +
      geom_text(x = -.75, y= -.02, label="0.75", color = "black") +
      geom_text(x = -1, y= -.02, label="1", color = "black") +
      geom_text(x = -.5, y= -.04, label="Relative Rank", color = "black") + 
      theme(
        panel.background = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank()
      )
  
  if (show_icd_codes) { 
    p <- p + ggrepel::geom_text_repel() #geom_text position=position_jitter(width=1,height=1)
  }

  return(p)
}