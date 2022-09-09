library(tidyverse)
library(ggpubr)

df1 <- tibble(est = c(0.64, 0.73, 0.94, 0.95, 0.82, 0.60),
             lower = c(0.33, 0.35, 0.50, 0.49, 0.17, 0.06),
             higher = c(1.26, 1.52, 1.78, 1.84, 3.98, 5.76),
             type = factor(c("0-13 days after first dose", "14-27 days after first dose",
                      "0-13 days after second dose", "14-27 days after second dose",
                      "0-13 days after third dose", "14-27 days after third dose"),
                      levels = rev(c("0-13 days after first dose", "14-27 days after first dose",
                                  "0-13 days after second dose", "14-27 days after second dose",
                                  "0-13 days after third dose", "14-27 days after third dose"))))

plot1 <- ggplot(df1) +
  geom_point(aes(type, est), shape = 15) +
  geom_point(aes(type, lower), shape = 3) +
  geom_point(aes(type, higher), shape = 3) +
  geom_segment(aes(type, lower, xend = type, yend = higher)) +
  geom_hline(yintercept = 1, linetype="dashed") +
  scale_y_log10(breaks = c(0.06, 1, 7), limits = c(0.06, 7)) +
  coord_flip() +
  theme_classic() +
  ylab("IRR (95% CI)") +
  theme(axis.title.y = element_blank(), axis.text.y = element_blank(), axis.ticks.y = element_blank(), 
        axis.line.y = element_blank())
  
df2 <- tibble(est = c(0.60, 0.91, 0.71, 0.79, 1.64, 1.71),
             lower = c(0.41, 0.63, 0.45, 0.46, 0.40, 0.44),
             higher = c(0.88, 1.32, 1.12, 1.35, 6.77, 6.62),
             type = factor(c("0-13 days after first dose", "14-27 days after first dose",
                             "0-13 days after second dose", "14-27 days after second dose",
                             "0-13 days after third dose", "14-27 days after third dose"),
                           levels = rev(c("0-13 days after first dose", "14-27 days after first dose",
                                          "0-13 days after second dose", "14-27 days after second dose",
                                          "0-13 days after third dose", "14-27 days after third dose"))))

plot2 <- ggplot(df2) +
  geom_point(aes(type, est), shape = 15) +
  geom_point(aes(type, lower), shape = 3) +
  geom_point(aes(type, higher), shape = 3) +
  geom_segment(aes(type, lower, xend = type, yend = higher)) +
  geom_hline(yintercept = 1, linetype="dashed") +
  scale_y_log10(breaks = c(0.4, 1, 7), limits = c(0.4, 7)) +
  coord_flip() +
  theme_classic() +
  ylab("IRR (95% CI)") +
  theme(axis.title.y = element_blank(), axis.text.y = element_blank(), axis.ticks.y = element_blank(), 
        axis.line.y = element_blank())

ggarrange(plot1, plot2, ncol = 2)
tiff("plot1.tif", width = 600, height = 1200, res = 300)
plot1
dev.off()
tiff("plot2.tif", width = 600, height = 1200, res = 300)
plot2
dev.off()
