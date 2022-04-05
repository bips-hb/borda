library(data.table)

rank3_1<-read.xlsx("resultate_neu/ranks_3digit.xlsx", sheet=1)
rank3_2<-read.xlsx("resultate_neu/ranks_3digit.xlsx", sheet=2)
rank3_3<-read.xlsx("resultate_neu/ranks_3digit.xlsx", sheet=3)
rank3_4<-read.xlsx("resultate_neu/ranks_3digit.xlsx", sheet=4)


rank4_1<-read.xlsx("resultate_neu/ranks_4digit.xlsx", sheet=1)
rank4_2<-read.xlsx("resultate_neu/ranks_4digit.xlsx", sheet=2)
rank4_3<-read.xlsx("resultate_neu/ranks_4digit.xlsx", sheet=3)
rank4_4<-read.xlsx("resultate_neu/ranks_4digit.xlsx", sheet=4)

rank5_1<-read.xlsx("resultate_neu/ranks_5digit.xlsx", sheet=1)
rank5_2<-read.xlsx("resultate_neu/ranks_5digit.xlsx", sheet=2)
rank5_3<-read.xlsx("resultate_neu/ranks_5digit.xlsx", sheet=3)
rank5_4<-read.xlsx("resultate_neu/ranks_5digit.xlsx", sheet=4)

rank6_1<-read.xlsx("resultate_neu/ranks_hmg.xlsx", sheet=1)
rank6_2<-read.xlsx("resultate_neu/ranks_hmg.xlsx", sheet=2)
rank6_3<-read.xlsx("resultate_neu/ranks_hmg.xlsx", sheet=3)
rank6_4<-read.xlsx("resultate_neu/ranks_hmg.xlsx", sheet=4)


corr3_1 <- as.matrix(fread("data/apix_3digit_correlation.csv"))
corr3_2 <- as.matrix(fread("data/dabi_3digit_correlation.csv"))
corr3_3 <- as.matrix(fread("data/edox_3digit_correlation.csv"))
corr3_4 <- as.matrix(fread("data/riva_3digit_correlation.csv"))

corr4_1 <- as.matrix(fread("data/apix_4digit_correlation.csv"))
corr4_2 <- as.matrix(fread("data/dabi_4digit_correlation.csv"))
corr4_3 <- as.matrix(fread("data/edox_4digit_correlation.csv"))
corr4_4 <- as.matrix(fread("data/riva_4digit_correlation.csv"))

corr5_1 <- as.matrix(fread("data/apix_5digit_correlation.csv"))
corr5_2 <- as.matrix(fread("data/dabi_5digit_correlation.csv"))
corr5_3 <- as.matrix(fread("data/edox_5digit_correlation.csv"))
corr5_4 <- as.matrix(fread("data/riva_5digit_correlation.csv"))

corr6_1 <- as.matrix(fread("data/apix_hmg_correlation.csv"))
corr6_2 <- as.matrix(fread("data/dabi_hmg_correlation.csv"))
corr6_3 <- as.matrix(fread("data/edox_hmg_correlation.csv"))
corr6_4 <- as.matrix(fread("data/riva_hmg_correlation.csv"))



results <- readr::read_rds("data/sccs_results.rds") 