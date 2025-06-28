rm(list =ls())
setwd("G:/My Drive/Università/Magistrale/Tesi/Studio di simulazione/Risultati/N_clust")

load("n_clust_1.1.RData")# n = 100,tau = rep(1/3, 3),delta = 0.25,m = c(4,4,4,3,3,3)
load("n_clust_1.2.RData")# n = 100,tau = rep(1/4, 4),delta = 0.25,m = c(4,4,4,3,3,3)
load("n_clust_1.3.RData") # n = 100,tau = rep(1/5, 5),delta = 0.25,m = c(4,4,4,3,3,3)
load("n_clust_2.1.RData") # n = 100,tau = rep(1/3, 3),delta = 0.4,m = c(4,4,4,3,3,3)
load("n_clust_2.2.RData") # n = 100,tau = rep(1/4, 4),delta = 0.4,m = c(4,4,4,3,3,3)
load("n_clust_2.3.RData") # n = 100,tau = rep(1/5, 5),delta = 0.4,m = c(4,4,4,3,3,3)
load("n_clust_3.1.RData")
load("n_clust_3.2.RData")
load("n_clust_3.3.RData")


n_clust_1 <- rbind(cbind(n_clust_1.1, "n_clust" = 3),
                   cbind(n_clust_1.2, "n_clust" = 4),
                   cbind(n_clust_1.3, "n_clust" = 5))

n_clust_2 <- rbind(cbind(n_clust_2.1, "n_clust" = 3),
                   cbind(n_clust_2.2, "n_clust" = 4),
                   cbind(n_clust_2.3, "n_clust" = 5))

n_clust_3 <- rbind(cbind(n_clust_3.1, "n_clust" = 3),
                   cbind(n_clust_3.2, "n_clust" = 4),
                   cbind(n_clust_3.3, "n_clust" = 5))

n_clust <- rbind(cbind(n_clust_1, "delta" = 0.25),
                 cbind(n_clust_2, "delta" = 0.4),
                 cbind(n_clust_3, "delta" = 0.55))


sim <- n_clust


names(sim)[3] <-"Time_Kmodes"

colonne_tempo <- names(sim)[str_detect(names(sim), "^Time_")]
sim <- sim %>%
  mutate(across(all_of(colonne_tempo), ~as.numeric(str_replace(.x, "_secs", ""))))

ris_long <- sim %>%
  pivot_longer(
    cols = -c(delta, n_clust),
    names_to = c(".value", "Metodo"), 
    names_pattern = "(ARI|NMI|Time)_(.+)" 
  ) %>%
  mutate(Metodo = str_to_upper(Metodo))


summary_mix <- ris_long %>%
  group_by( Metodo, delta, n_clust) %>%
  summarise(
    ARI_Media = mean(ARI, na.rm = TRUE),
    # ARI_Mediana = median(ARI, na.rm = TRUE),
    ARI_DevStandard = sd(ARI, na.rm = TRUE),
    NMI_Media = mean(NMI, na.rm = TRUE),
    # NMI_Mediana = median(NMI, na.rm = TRUE),
    NMI_DevStandard = sd(NMI, na.rm = TRUE),
    Tempo_Medio_sec = mean(Time, na.rm = TRUE), # Le colonne Time sono già numeriche e in secondi
    .groups = 'drop' # Rimuove il raggruppamento dopo summarise
  )

digits_colonne <- c(0, 0, 2, 0, 6, 6, 6, 6, 6)
tabella_sim <- xtable(summary_mix, digits = digits_colonne)
print(tabella_sim, include.rownames = FALSE, type = "latex")


# ARI ---------------------------------------------------------------------

setwd("G:/My Drive/Università/Magistrale/Tesi/Studio di simulazione/Grafici/n_clust")

ris_long$n_clust <- factor(ris_long$n_clust)

levels(ris_long$n_clust) <- c("G = 3", "G = 4", "G = 5")

ris_delta_.25 <- ris_long %>%
  select(delta, n_clust, ARI, Metodo)%>%
  filter(delta == 0.25) 

boxplot_delta.25 <- ggplot(ris_delta_.25, aes(x = Metodo, y = ARI, fill = Metodo))+
  geom_boxplot()+
  facet_wrap(~ n_clust, scales = "free_x")+
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1), 
        plot.title = element_text(hjust = 0.5), 
        strip.background = element_rect(fill="lightblue", color="black"), 
        strip.text = element_text(face="bold"), legend.position = "none")
print(boxplot_delta.25)
ggsave(boxplot_delta.25, file = "boxplot_delta.25.pdf",
       width = 10, height = 6, units = "in")

ris_delta_.4 <- ris_long %>%
  select(delta, n_clust, ARI, Metodo)%>%
  filter(delta == 0.4) 

boxplot_delta.4 <- ggplot(ris_delta_.4, aes(x = Metodo, y = ARI, fill = Metodo))+
  geom_boxplot()+
  facet_wrap(~ n_clust, scales = "free_x")+
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1), 
        plot.title = element_text(hjust = 0.5), 
        strip.background = element_rect(fill="lightblue", color="black"), 
        strip.text = element_text(face="bold"), legend.position = "none")
print(boxplot_delta.4)
ggsave(boxplot_delta.4, file = "boxplot_delta.4.pdf",
       width = 10, height = 6, units = "in")


ris_delta_.55 <- ris_long %>%
  select(delta, n_clust, ARI, Metodo)%>%
  filter(delta == 0.55) 

boxplot_delta.55 <- ggplot(ris_delta_.55, aes(x = Metodo, y = ARI, fill = Metodo))+
  geom_boxplot()+
  facet_wrap(~ n_clust, scales = "free_x")+
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1), 
        plot.title = element_text(hjust = 0.5), 
        strip.background = element_rect(fill="lightblue", color="black"), 
        strip.text = element_text(face="bold"), legend.position = "none")
print(boxplot_delta.55)
ggsave(boxplot_delta.55, file = "boxplot_delta.55.pdf",
       width = 10, height = 6, units = "in")


#NMI --------------------

ris_delta_.25 <- ris_long %>%
  select(delta, n_clust, NMI, Metodo)%>%
  filter(delta == 0.25) 

boxplot_delta.25 <- ggplot(ris_delta_.25, aes(x = Metodo, y = NMI, fill = Metodo))+
  geom_boxplot()+
  facet_wrap(~ n_clust, scales = "free_x")+
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1), 
        plot.title = element_text(hjust = 0.5), 
        strip.background = element_rect(fill="lightblue", color="black"), 
        strip.text = element_text(face="bold"), legend.position = "none")
print(boxplot_delta.25)
ggsave(boxplot_delta.25, file = "nmi_delta.25.pdf",
       width = 10, height = 6, units = "in")

ris_delta_.4 <- ris_long %>%
  select(delta, n_clust, NMI, Metodo)%>%
  filter(delta == 0.4) 

boxplot_delta.4 <- ggplot(ris_delta_.4, aes(x = Metodo, y = NMI, fill = Metodo))+
  geom_boxplot()+
  facet_wrap(~ n_clust, scales = "free_x")+
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1), 
        plot.title = element_text(hjust = 0.5), 
        strip.background = element_rect(fill="lightblue", color="black"), 
        strip.text = element_text(face="bold"), legend.position = "none")
print(boxplot_delta.4)
ggsave(boxplot_delta.4, file = "nmi_delta.4.pdf",
       width = 10, height = 6, units = "in")


ris_delta_.55 <- ris_long %>%
  select(delta, n_clust, NMI, Metodo)%>%
  filter(delta == 0.55) 

boxplot_delta.55 <- ggplot(ris_delta_.55, aes(x = Metodo, y = NMI, fill = Metodo))+
  geom_boxplot()+
  facet_wrap(~ n_clust, scales = "free_x")+
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1), 
        plot.title = element_text(hjust = 0.5), 
        strip.background = element_rect(fill="lightblue", color="black"), 
        strip.text = element_text(face="bold"), legend.position = "none")
print(boxplot_delta.55)
ggsave(boxplot_delta.55, file = "nmi_delta.55.pdf",
       width = 10, height = 6, units = "in")
