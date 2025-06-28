library(tidyverse)
library(xtable)

load("simulazione_1.1.RData") #n = 100
load("simulazione_1.2.RData") #n = 200
load("simulazione_1.3.RData") #n = 400
load("simulazione_1.4.RData") #n = 800

ris_1 <- cbind("n" = 100, simulazione_1.1)
colnames(ris_1)
ris_2 <- cbind("n" = 200, simulazione_1.2)
colnames(ris_2)
ris_3 <- cbind("n" = 400, simulazione_1.3)
colnames(ris_3)
# ris_4 <- cbind("n" = 800, simulazione_1.4)
# colnames(ris_4)


ris <- rbind(ris_1, ris_2, ris_3) #,ris_4)
head(ris)

if ("Time_kmodes" %in% names(ris)) {
  ris <- ris %>%
    rename(Time_Kmodes = Time_kmodes) # Rinomina Time_kmodes in Time_Kmodes
}

colonne_tempo <- names(ris)[str_detect(names(ris), "^Time_")]

ris <- ris %>%
  mutate(across(all_of(colonne_tempo), ~as.numeric(str_replace(.x, "_secs", ""))))
ris_long <- ris %>%
  pivot_longer(
    cols = -n,
    names_to = c(".value", "Metodo"), 
    names_pattern = "(ARI|NMI|Time)_(.+)" 
  )

summary_sim1 <- ris_long %>%
  group_by(n, Metodo) %>%
  summarise(
    ARI_Media = mean(ARI, na.rm = TRUE),
    ARI_Mediana = median(ARI, na.rm = TRUE),
    ARI_DevStandard = sd(ARI, na.rm = TRUE),
    NMI_Media = mean(NMI, na.rm = TRUE),
    NMI_Mediana = median(NMI, na.rm = TRUE),
    NMI_DevStandard = sd(NMI, na.rm = TRUE),
    Tempo_Medio_sec = mean(Time, na.rm = TRUE), # Le colonne Time sono già numeriche e in secondi
    .groups = 'drop' # Rimuove il raggruppamento dopo summarise
  )

print(summary_sim1)

digits_colonne <- c(0,0,  # Per la colonna 'n' (nessun decimale)
                    0,  # Per 'Metodo' (carattere, i decimali sono ignorati ma serve un placeholder)
                    6,  # Per ARI_Media
                    6,  # Per ARI_Mediana
                    6,  # Per ARI_DevStandard
                    6,  # Per NMI_Media
                    6,  # Per NMI_Mediana
                    6,  # Per NMI_DevStandard
                    6)  # Per Tempo_Medio_sec



library(xtable)
tabella_sim1 <- xtable(summary_sim1, digits = digits_colonne)
print(tabella_sim1, include.rownames = FALSE, type = "latex")

ris_long_n100 <- ris_long %>%
  filter(n == 100)




p1_n100_ARI <-ggplot(ris_long_n100, aes(x = Metodo, y = ARI, fill = Metodo))+
    geom_boxplot()+
    theme_minimal()

p1_n100_NMI <- ggplot(ris_long_n100, aes(x = Metodo, y = NMI, fill = Metodo))+
  geom_boxplot()+
  theme_minimal()

p1_n100_time <- ggplot(ris_long_n100, aes(x = Metodo, y = Time, fill = Metodo))+
  geom_boxplot()+
  theme_minimal()

plot_tempi_barre <- ggplot(summary_sim1, aes(x = reorder(Metodo, Tempo_Medio_sec), y = Tempo_Medio_sec, fill = Metodo)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  scale_y_log10()+
  facet_wrap(~n, scales = "free_x", labeller = labeller(n = function(val) paste("n =", val))) + 
  labs(
    title = "",
    x = "Metodo",
    y = "Tempo Medio (scala log10)"
  ) +
  theme_minimal(base_size = 12) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1), 
        plot.title = element_text(hjust = 0.5), 
        strip.background = element_rect(fill="lightblue", color="black"), 
        strip.text = element_text(face="bold")) +
  scale_fill_brewer(palette = "Paired")


ggsave(plot_tempi_barre, filename = "plot_tempi_simulazione1.pdf")


