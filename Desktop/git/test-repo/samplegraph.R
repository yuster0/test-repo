groups <- c("Group A","Group B","Group C")
performance <- c(.55,.58,.67)
data <- data.frame(groups,performance)
# Organize colors by defining them as objects.
dark <- "#595959"
light <- "#d9d9d9"
accent <- "#eb7d2e"
# Load ggplot2 from the package library.
library(ggplot2)
# Create and clean the visual.
ggplot(data = data, aes(x = groups, y = performance), color = dark) +
  geom_bar(width = 0.4, stat = "identity") +
  scale_y_continuous(limits=c(0,0.8)) +
  annotate("text", x = data$groups, y = data$performance - .06, label = paste(data$performance*100, "%", sep = ""), size = 5, color = "white", fontface = "bold") + 
  ggtitle("Two of the three grantee groups did not meet \nperformance benchmarks this year.") +
  geom_hline(aes(yintercept = 0.65), color = accent, size = 1) +
  annotate("text", x = data$groups[1], y = .62, label = "Benchmark", size = 5, color = dark, fontface = "bold", hjust = 1) +
  labs(x = "", y = "") +
  theme_bw() +
  theme(
    plot.title = element_text(size = 18, face = "bold", hjust = 0),
    panel.border = element_blank(),
    panel.grid = element_blank(),
    axis.ticks = element_blank(),
    axis.text.x = element_text(size = 14, face = "bold", color = dark),
    axis.text.y = element_blank(),
    axis.line = element_blank()
  ) +
  geom_hline(yintercept = 0, size = 0.5, color = light)
