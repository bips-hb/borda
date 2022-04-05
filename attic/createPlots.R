###########################################################
# Creates all the plots
###########################################################

library(gepardsim)
library(latex2exp)

# Create boxplots 
results <- readr::read_rds("sccs_results.rds")

### filter the results for RF_corrected 
results <- results %>% 
  dplyr::filter(algorithm != "RF_corrected")

n_patients <- c(1000, 10000, 25000, 100000)

plotBoxplot(results,
            n_patients = 1000,
            title = "1,000 Exposed Patients",
            ylabel = "")
ggsave(
  sprintf("figures/boxplot_1000.eps", n),
  width = 20,
  height = 18,
  units = "cm"
)

plotBoxplot(results,
            n_patients = 10000,
            title = "10,000 Exposed Patients",
            ylabel = "")
ggsave(
  sprintf("figures/boxplot_10000.eps", n),
  width = 20,
  height = 18,
  units = "cm"
)

plotBoxplot(results,
            n_patients = 25000,
            title = "25,000 Exposed Patients")
ggsave(
  sprintf("figures/boxplot_25000.eps", n),
  width = 20,
  height = 18,
  units = "cm"
)

plotBoxplot(results,
            n_patients = 100000,
            title = "100,000 Exposed Patients")
ggsave(
  sprintf("figures/boxplot_100000.eps", n),
  width = 20,
  height = 18,
  units = "cm"
)


# Create line plots

methods <- c("LGPS0.025", "PRR0", "RF", "number_of_reports", "ICAlt0.05", "ICOrig0.05", "ppoisson")

p <- plotLinePlot(results, algorithms = methods)
p + theme_bw(legend.position="none")
p
ggsave(
  "figures/line.eps",
  width = 20,
  height = 15,
  units = "cm"
)

gepardsim::plotLinePlot(results, algorithms = methods, xlim = c(0, 26000), ylim = c(.47, .79), ylabel = "")
ggsave(
  "figures/line_detail.eps",
  width = 20,
  height = 15,
  units = "cm"
)


res <- results %>% group_by(n_patients_exposed, algorithm) %>% 
  summarise(score = mean(rocauc), 
            CI = 1.96 * sd(rocauc) / sqrt(n())) %>% 
  filter(score == max(score)) %>%
  arrange(n_patients_exposed)

yhigh <- .42
hjust <- .15

ggplot(res, aes(x = n_patients_exposed, y = score)) + 
  geom_line() + 
  geom_errorbar(aes(ymin= score - CI, ymax = score + CI), width=.1) +
  geom_point() + 
  ylab("Highest Average AUC") + 
  xlab("Number of Patients Exposed") + 
  scale_y_continuous(limits=c(.40, 0.9), expand = c(0, 0)) + 
  scale_x_continuous(limits=c(0, 101000), expand = c(0,0), breaks = c(1000, seq(2500, 100000, by = 2500))) + 
  theme_bw() + 
  ggtitle("Best Performing Method") + 
  theme(axis.text.x = element_text(angle = 70, hjust = 1),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(),
        panel.background = element_blank()) + 
  annotate("text", label = "midRFET", x = 1000, y = yhigh, angle = 90, size = 3, hjust = hjust) + 
  annotate('text', label=TeX("$IC^{original}_{05}$", output='character'), x=2500, angle = 90, size = 3, y= yhigh, parse=TRUE, hjust = hjust) +
  annotate('text', label=TeX("$IC^{original}_{025}$", output='character'), x=5000, angle = 90, size = 3, y= yhigh, parse=TRUE, hjust = hjust) +
  annotate('text', label=TeX("$\\chi^2$", output='character'), x=8750, angle = 0, size = 3, y= yhigh, parse=TRUE, hjust = 0.5) +
  geom_vline(xintercept = 6250,
             linetype = "dashed",
             color = "grey") + 
  geom_vline(xintercept = 11250,
             linetype = "dashed",
             color = "grey") + 
  annotate("text", label = "RF", x = 13750, y = yhigh, angle = 0, size = 3, hjust = 0.5) + 
  geom_vline(xintercept = 16250,
             linetype = "dashed",
             color = "grey") + 
  annotate("text", label = "ROR", x = 21250, y = yhigh, angle = 0, size = 3, hjust = 0.5) + 
  geom_vline(xintercept = 26250,
             linetype = "dashed",
             color = "grey") + 
  annotate('text', label=TeX("$p_{Poisson}$", output='character'), x=33750, angle = 0, size = 3, y= yhigh, parse=TRUE, hjust = 0.5) +
  geom_vline(xintercept = 41250,
             linetype = "dashed",
             color = "grey") + 
  annotate('text', label=TeX("$LGPS_{025}$", output='character'), x=51250, angle = 0, size = 3, y= yhigh, parse=TRUE, hjust = 0.5) +
  geom_vline(xintercept = 61250,
             linetype = "dashed",
             color = "grey") + 
  annotate('text', label=TeX("$IC^{alternative}$", output='character'), x=65000, angle = 90, size = 3, y= yhigh, parse=TRUE, hjust = hjust) +
  geom_vline(xintercept = 68250,
             linetype = "dashed",
             color = "grey") +
  annotate("text", label = TeX("$IC^{original}$", output='character'), x = 70000, angle = 90, size = 3, y= yhigh, parse=TRUE, hjust = hjust) + 
  annotate("text", label = TeX("$IC^{alternative}$", output='character'), x = 72500, angle = 90, size = 3, y= yhigh, parse=TRUE, hjust = hjust) + 
  geom_vline(xintercept = 73750,
             linetype = "dashed",
             color = "grey") +
  annotate('text', label=TeX("$IC^{alternative}_{05}$", output='character'), x=87500, angle = 0, size = 3, y= yhigh, parse=TRUE, hjust = 0.5) 

ggsave(
  "figures/bestmethods.pdf",
  width = 30,
  height = 10,
  units = "cm"
)
