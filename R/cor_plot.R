
library(data.table)
library(ggplot2)
library(latex2exp)
library(xtable)


plotCorr<-function(cc){
  
 # colnames(cc)<- c(TeX("\\(IC^{alternative}\\)",output = "text"),TeX("\\(IC^{original}\\)",output = "text"),
  #                 TeX("\\(LGPS\\)",output = "text"),TeX("\\(p_{Poisson}\\)",output = "text"),
   #                TeX("\\(NREP\\)",output = "text"),TeX("\\(PRR\\)",output = "text"),
    #               TeX("\\(RF_{impu}\\)",output = "text"),TeX("\\(RF_{corr}\\",output = "text"),
     #              TeX("\\(LASSO\\)",output = "text"))

rownames(cc) <- colnames(cc)

# Plot
cc[lower.tri(cc)] <- NA
cc_dt <- melt(round(cc, 2))
cc_dt$Var2 <- factor(cc_dt$Var2, levels = rev(levels(cc_dt$Var2)))
ggplot(cc_dt, aes(Var1, Var2, fill = value)) + 
  geom_tile() + 
  geom_text(aes(Var1, Var2, label = value), color = "#073642", size = 4) +
  scale_fill_gradient(name=expression("Kendall" * ~ tau), low = "#fdf6e3", high = "steelblue",
                      breaks=seq(0, 1, by = 0.2), limits = c(0, 1), na.value = "white") +
  scale_x_discrete(labels=c('ICA'=parse(text=TeX('$IC^{alternative}$')),
                            'ICO'=parse(text=TeX('$IC^{original}$')),
                            'LGPS'=parse(text=TeX('$LGPS$')),
                            'POIS'=parse(text=TeX('$p_{poisson}$')),
                            'NREP'=parse(text=TeX('$NREP$')),
                            'PRR'=parse(text=TeX('$PRR$')),
                            'RF_IMPU'=parse(text=TeX('$RF_{impu}$')),
                            'RF_CORR'=parse(text=TeX('$RF_{corr}$')),
                            'LASSO'=parse(text=TeX('$LASSO$'))
                              )) +
  scale_y_discrete(labels=c('ICA'=parse(text=TeX('$IC^{alternative}$')),
                            'ICO'=parse(text=TeX('$IC^{original}$')),
                            'LGPS'=parse(text=TeX('$LGPS$')),
                            'POIS'=parse(text=TeX('$p_{poisson}$')),
                            'NREP'=parse(text=TeX('$NREP$')),
                            'PRR'=parse(text=TeX('$PRR$')),
                            'RF_IMPU'=parse(text=TeX('$RF_{impu}$')),
                            'RF_CORR'=parse(text=TeX('$RF_{corr}$')),
                            'LASSO'=parse(text=TeX('$LASSO$'))
  )) +
  labs(x = "", y = "") + 
  guides(fill = guide_colorbar(barwidth = 7, barheight = 1, title.position = "top",
                               title.hjust = 0.5)) +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size=20),
        axis.text.y=element_text(size=20),
         panel.grid.major = element_blank(),
         panel.border = element_blank(),
         panel.background = element_blank(),
         axis.ticks = element_blank(),
         legend.justification = c(1, 0),
         legend.position = c(0.9, 0.7),
         legend.text = element_text(size = 10), 
         legend.direction = "horizontal") +
  
  guides(fill = guide_colorbar(barwidth = 7, barheight = 1, title.position = "top", 
                               title.hjust = 0.5))
}