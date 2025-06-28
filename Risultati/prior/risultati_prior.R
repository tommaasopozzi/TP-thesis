#Studio prior
rm(list=ls())
setwd("G:/My Drive/Università/Magistrale/Tesi/Studio di simulazione/Risultati/prior")
load("prior_1.1.RData") #n = 25, delta = 0.55
load("prior_1.2.RData") #n = 25, delta = 0.7
load("prior_1.RData") # n= 25, delta = 0.4
load("prior_2.1.RData") #n = 50, delta = 0.55
load("prior_2.2.RData") #n = 50, delta = 0.7
load("prior_2.RData") # n = 50, delta = 0.4
load("prior_3.1.RData") # n = 100, delta = 0.55
load("prior_3.2.RData") # n = 100, delta = 0.7
load("prior_3.RData") #n = 100, delta = 0.4
load("prior_4.1.RData") # n = 200, delta = 0.55
load("prior_4.2.RData") # n = 200, delta = 0.7
load("prior_4.RData") #n = 200, delta = 0.4


prior1 <- rbind(cbind(prior_1, "delta" = 0.4), cbind(prior_1.1, "delta" = 0.55), cbind(prior_1.2, "delta" = 0.7))
prior2 <- rbind(cbind(prior_2, "delta" = 0.4), cbind(prior_2.1, "delta" = 0.55), cbind(prior_2.2, "delta" = 0.7))
prior3 <- rbind(cbind(prior_3, "delta" = 0.4), cbind(prior_3.1, "delta" = 0.55), cbind(prior_3.2, "delta" = 0.7))
prior4 <- rbind(cbind(prior_4, "delta" = 0.4), cbind(prior_4.1, "delta" = 0.55), cbind(prior_4.2, "delta" = 0.7))

ris <- rbind(cbind(prior1, "n" = 25), cbind(prior2, "n" = 50),
             cbind(prior3, "n" = 100), cbind(prior4, "n" = 200))

str(ris)

names(ris) <- gsub("\\.\\.\\.", "=", names(ris))
names(ris)[2] <- "NMI.b=0.1"



ris_long <- ris %>%
  pivot_longer(
    cols = -c(n,delta),
    names_to = c(".value", "Metodo"), 
    names_pattern = "(ARI|NMI).(.+)" 
  ) %>%
  mutate(Metodo = str_to_upper(Metodo))


summary <- ris_long %>%
  group_by(n, Metodo, delta) %>%
  filter(startsWith(Metodo, "C"))%>%
  summarise(
    ARI_Media = mean(ARI, na.rm = TRUE),
    ARI_Mediana = median(ARI, na.rm = TRUE),
    ARI_DevStandard = sd(ARI, na.rm = TRUE),
    NMI_Media = mean(NMI, na.rm = TRUE),
    NMI_Mediana = median(NMI, na.rm = TRUE),
    NMI_DevStandard = sd(NMI, na.rm = TRUE),
    .groups = 'drop' # Rimuove il raggruppamento dopo summarise
  )
summary

digits_colonne <- c(0, 0, 0, 2, 6, 6, 6, 6, 6, 6)
tabella_sim <- xtable(summary, digits = digits_colonne)
print(tabella_sim, include.rownames = FALSE, type = "latex")

library(stringr)

ris_long <- ris_long %>%
  mutate(Metodo = str_replace(Metodo, "^B", "b")) %>%
  mutate(Metodo = str_replace(Metodo, "^C", "c"))


summary_b <- ris_long %>%
  group_by(n, Metodo, delta) %>%
  filter(startsWith(Metodo, "b"))%>%
  summarise(
    ARI_Media = mean(ARI, na.rm = TRUE),
    ARI_Mediana = median(ARI, na.rm = TRUE),
    ARI_DevStandard = sd(ARI, na.rm = TRUE),
    NMI_Media = mean(NMI, na.rm = TRUE),
    NMI_Mediana = median(NMI, na.rm = TRUE),
    NMI_DevStandard = sd(NMI, na.rm = TRUE),
    .groups = 'drop' # Rimuove il raggruppamento dopo summarise
  )
summary

digits_colonne <- c(0, 0, 0, 2, 6, 6, 6, 6, 6, 6)
tabella_sim <- xtable(summary_b, digits = digits_colonne)
print(tabella_sim, include.rownames = FALSE, type = "latex")




summary_eb <- ris_long %>%
  group_by(n, Metodo, delta) %>%
  filter(startsWith(Metodo, "EB")| startsWith(Metodo, "TRUE"))%>%
  summarise(
    ARI_Media = mean(ARI, na.rm = TRUE),
    ARI_Mediana = median(ARI, na.rm = TRUE),
    ARI_DevStandard = sd(ARI, na.rm = TRUE),
    NMI_Media = mean(NMI, na.rm = TRUE),
    NMI_Mediana = median(NMI, na.rm = TRUE),
    NMI_DevStandard = sd(NMI, na.rm = TRUE),
    .groups = 'drop' # Rimuove il raggruppamento dopo summarise
  )%>%
  mutate(Metodo = str_replace(Metodo, "EB.STRENGHT", "strenght"))


digits_colonne <- c(0, 0, 0, 2, 6, 6, 6, 6, 6, 6)
tabella_sim <- xtable(summary_eb, digits = digits_colonne)
print(tabella_sim, include.rownames = FALSE, type = "latex")


ari_n25 <- ris_long %>%
  select(delta, ARI, n, Metodo) %>%
  filter(n == 25, startsWith(Metodo, "c") | startsWith(Metodo, "TRUE")) 


boxplot_ari_n25 <- ggplot(ari_n25, aes(x = Metodo, y = ARI, fill = Metodo))+
  geom_boxplot()+
  facet_wrap(~delta, scales = "free_x", 
             labeller = labeller(delta = function(val) paste("\u03B4 =", val)))+
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1), 
        plot.title = element_text(hjust = 0.5), 
        strip.background = element_rect(fill="lightblue", color="black"), 
        strip.text = element_text(face="bold"), legend.position = "none")

print(boxplot_ari_n25)

ari_c <- ris_long %>%
  select(delta, ARI, n, Metodo) %>%
  filter( startsWith(Metodo, "c") | startsWith(Metodo, "TRUE")) 



df_punti_medi_ari <- ari_c %>%
  filter(!is.na(ARI)) %>% # Importante: escludi NA prima di calcolare la media
  group_by(delta, Metodo,n) %>%
  summarise(
    ARI_medio = mean(ARI, na.rm = TRUE),
    .groups = 'drop' # Rimuove il raggruppamento dopo summarise
  ) %>%
  mutate(
    delta_factor = factor(delta),
    n_factor = factor(n)
  ) %>%
  filter(delta == 0.4)

grafico_ari_punti_medi <- ggplot(df_punti_medi_ari,
                                 aes(x = Metodo, y = ARI_medio, group = n_factor, color = n_factor)) + # group = 1 per una singola linea per facet
  geom_line(alpha = 0.8, linewidth = 1) + # Linea che collega i punti medi
  geom_point(size = 3, alpha = 0.9) +  # Punti medi
  labs(
    title = "",
    x = "Metodo",
    y = "ARI Medio",
    color = "N"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, size = 10), # Ruota etichette asse x
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
    strip.text = element_text(face = "bold", size = 11),
    axis.title = element_text(face = "bold", size = 13),
    legend.title = element_text(face = "bold")
  )
print(grafico_ari_punti_medi)
