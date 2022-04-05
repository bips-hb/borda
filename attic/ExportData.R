
rm(list = ls())
options(java.parameters = "- Xmx1024m")
library(tidyverse)
library(xlsx)
library(ggplot2)
library(grid)
library(gridExtra)
library(votesys)
library(rJava)

options(java.parameters = "-Xmx8000m")

# XLSX filepath of all the rankings 
XLSX_FILEPATH <- "data/ranks_4digit.xlsx"


# drugs: Apixaban, Dabigatran, Edoxaban, Rivaroxaban
read_raw_xlsx_file <- function(path = XLSX_FILEPATH, 
                               drug, 
                               methods_of_interest = c("ICA", "LGPS", "RF_CORR", "LASSO"), 
                               keep_description_ICD = TRUE) { 
  # read in file 
  xlsx_data <- as_tibble(xlsx::read.xlsx2(path, sheetName = drug))
  
  #remove first column
  xlsx_data <- xlsx_data %>% select(-Rank)
  
  # remove first row
  xlsx_data <- xlsx_data %>% slice(-1)
  
  # create column names
  methods <- c("ICA", "ICO", "LGPS", "POIS", "NUM_REPORTS", "PRR", "RF_IMPU", "RF_CORR", "LASSO") 
  columns <- as.vector(sapply(methods, function(method) c(method, paste0(method, '.ICD'), paste0(method, '.Text'), paste0(method, '.empty'))))
  colnames(xlsx_data) <- columns
  
  # change classes of the columns
  xlsx_data <- xlsx_data %>% mutate_if(is.factor, as.character) %>% 
    mutate_at(methods, as.double)
  
  # keep columns for selection of methods
  if (keep_description_ICD) { 
    columns_of_interest <- as.vector(sapply(methods_of_interest, function(method) c(method, paste0(method, '.ICD'), paste0(method, '.Text'))))
  } else { 
    columns_of_interest <- as.vector(sapply(methods_of_interest, function(method) c(method, paste0(method, '.ICD'))))
  }
  
  xlsx_data %>% select(columns_of_interest)
}

### Process the data for one method given the raw XLSX data
get_data_for_one_method <- function(data, method, keep_description_ICD = TRUE) { 
  
  if(keep_description_ICD) { 
    data <- setNames(data[, c(method, paste0(method, ".ICD"), paste0(method, ".Text"))], c("score", "ICD", "Description"))
  } else { 
    data <- setNames(data[, c(method, paste0(method, ".ICD"))], c("score", "ICD"))
  }
  
  data$method <- method
  
  ### create ranking
  data$rank <- nrow(data) + 1 - rank(data$score)
  data$relative_rank <- (data$rank - 1) / (nrow(data) - 1)
  
  data
}
#Make sure to reload each time u use this -.-

dat <- read_raw_xlsx_file(keep_description_ICD = TRUE, drug = "Edoxaban")
ICA <- get_data_for_one_method(dat, "ICA", T)
LGPS <- get_data_for_one_method(dat, "LGPS", T)
RF <- get_data_for_one_method(dat, "RF_CORR", T) 
RF$method <- "RF"
LASSO <- get_data_for_one_method(dat, "LASSO", T)
vars <- c("ICD","Description","rank")

ICA_dat <- ICA[vars]
names(ICA_dat)[3] <- "ICA"
LGPS_dat <- LGPS[vars]
names(LGPS_dat)[3] <- "LGPS"
RF_dat <- RF[vars]
names(RF_dat)[3] <- "RF"
Lasso_dat <- LASSO[vars]
names(Lasso_dat)[3] <- "Lasso"

data <- right_join(ICA_dat, LGPS_dat, by = c("ICD", "Description")) %>%
     right_join(., RF_dat, by = c("ICD", "Description")) %>% 
     right_join(., Lasso_dat, by = c("ICD", "Description")) 



#.rs.restartR() 
# data <- full_join(ICA, LGPS, by = c("ICD", "method", "score", "rank", "relative_rank")) %>%
#   full_join(., RF) %>% 
#   full_join(., LASSO) 

# write.table(data, "data/Riva.csv", row.names = FALSE, sep = ",")
# dat <- read_raw_xlsx_file(keep_description_ICD = TRUE, drug = "Dabigatran")
#write.table(data, "data/Dabi.csv", row.names = FALSE, sep = ",")
# dat <- read_raw_xlsx_file(keep_description_ICD = TRUE, drug = "Edoxaban")
 write.table(data, "data/Edox.csv", row.names = FALSE, sep = ",")
# dat <- read_raw_xlsx_file(keep_description_ICD = TRUE, drug = "Apixaban")
# write.table(data, "data/Apix.csv", row.names = FALSE, sep = ",")
