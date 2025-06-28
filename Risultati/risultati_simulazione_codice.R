load("simulazione_1.1.RData")
ris <- simulazione_1.1
str(ris)
head(ris)
library(tidyverse)
library(ggplot2)

ris_ari <- ris %>%
  select(contains("ARI"))

ris_nmi <- ris %>%
  select(contains("NMI"))

ris_time <- ris %>%
  select(contains("Time")) %>%
  mutate(across(everything(), as.numeric))

#Osservo i risultati e le loro variabilità


long_ari <- pivot_longer(ris_ari, cols = starts_with("ARI_"),
                        names_to = "Metodo", values_to = "ARI")
ggplot(long_ari, aes(x = Metodo, y = ARI, fill = Metodo))+
  geom_boxplot()+
  theme_minimal()


long_nmi <- pivot_longer(ris_nmi, cols = starts_with("NMI_"),
                         names_to = "Metodo", values_to = "NMI")

ggplot(long_nmi, aes(x = Metodo, y = NMI, fill = Metodo))+
  geom_boxplot()+
  theme_minimal()

long_time <- pivot_longer(ris_time, cols = starts_with("Time"),
                          names_to = "Metodo", values_to = "Time")

ggplot(long_time, aes(x = Metodo, y = Time, fill = Metodo))+
  geom_boxplot()+
  theme_minimal()

mean_ari <- colMeans(ris_ari)  
mean_nmi <- colMeans(ris_nmi)
mean_time <- colMeans(ris_time)
sd_ari <- apply(ris_ari, 2, sd)
sd_nmi <- apply(ris_ari, 2, sd)
sd_time <- apply(ris_time, 2, sd)


summary <- cbind(mean_ari, sd_ari, mean_nmi, sd_nmi, mean_time, sd_time)
colnames(summary) <- c("ARI Media", "ARI sd", "NMI media", "NMI sd", "Time media", "Time sd")
rownames(summary) <- c("K-mode", "E-M", "E-M parsimonia", "Bayes Standard", "Bayes parsimonia", "GBPP")

library(xtable)

xtable(summary, digits = 6, caption = "Risultati scenario simulativo con n = 100, m = c(4,4,4,3,3,3), delta = 0.25, tau = c(1/3,1/3,1/3)")

# Rimuovo "Time_" e ordino per tempo mediano
long_time <- long_time %>%
  mutate(Metodo = gsub("Time_", "", Metodo)) %>%
  mutate(Metodo = reorder(Metodo, Time, FUN = median))

# Grafico finale
ggplot(long_time, aes(x = Metodo, y = Time, fill = Metodo)) +
  geom_boxplot(outlier.size = 0.5, width = 0.6, alpha = 0.8) +
  stat_summary(fun = mean, geom = "point", shape = 21, size = 2.5, fill = "white", color = "black") +
  scale_y_log10() +
  scale_fill_brewer(palette = "Set2") +
  labs(title = "Tempi computazionali dei metodi",
       x = "Metodo",
       y = "Tempo (secondi, scala logaritmica)") +
  theme_minimal(base_size = 13) +
  theme(
    axis.text.x = element_text(angle = 30, hjust = 1),
    legend.position = "none",
    plot.title = element_text(face = "bold", hjust = 0.5)
  )
