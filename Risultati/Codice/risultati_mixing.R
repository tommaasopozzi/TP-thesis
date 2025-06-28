setwd("G:/My Drive/Università/Magistrale/Tesi/Studio di simulazione/Risultati/Mixing")

load("Mixing_1.1.1.RData") #n = 100, tau = c(0.1,0.7,0.2), delta = 0.55
mixing_1.1.1 <- df_results
load("Mixing_1.1.1_equal.RData")
mixing_1.1.1_eq <- df_results
load("Mixing_1.1.RData")
mixing_1.1 <- df_results #n = 100, tau = c(0.1,0.7,0.2), delta = 0.4
load("Mixing_1.1_equal.RData")
mixing_1.1_eq <- df_results #n = 100, tau = rep(1/3, 3), delta = 0.4

mix_1_1 <- rbind(cbind(mixing_1.1, "delta" = 0.4, "n_clust" = 3, "tau" = "not equal"),
               cbind(mixing_1.1_eq, "delta" = 0.4, "n_clust" = 3, "tau" = "equal"),
               cbind(mixing_1.1.1, "delta" = 0.55, "n_clust" = 3, "tau" = "not equal"),
               cbind(mixing_1.1.1_eq, "delta" = 0.55, "n_clust" = 3, "tau" = "equal"))


load("Mixing_1.2.1.RData") #n = 200, tau = c(0.1,0.7,0.2), delta = 0.55
mixing_1.2.1 <- df_results
load("Mixing_1.2.1_equal.RData")
mixing_1.2.1_eq <- df_results
load("Mixing_1.2.RData")
mixing_1.2 <- df_results #n = 200, tau = c(0.1,0.7,0.2), delta = 0.4
load("Mixing_1.2_equal.RData")
mixing_1.2_eq <- df_results #n = 200, tau = rep(1/3, 3), delta = 0.4


mix_1_2 <- rbind(cbind(mixing_1.2, "delta" = 0.4, "n_clust" = 3, "tau" = "not equal"),
                 cbind(mixing_1.2_eq, "delta" = 0.4, "n_clust" = 3, "tau" = "equal"),
                 cbind(mixing_1.2.1, "delta" = 0.55, "n_clust" = 3, "tau" = "not equal"),
                 cbind(mixing_1.2.1_eq, "delta" = 0.55, "n_clust" = 3, "tau" = "equal"))

load("Mixing_1.3.1.RData") #n = 400, tau = c(0.1,0.7,0.2), delta = 0.55
mixing_1.3.1 <- df_results
load("Mixing_1.3.1_equal.RData")
mixing_1.3.1_eq <- df_results
load("Mixing_1.3.RData")
mixing_1.3 <- df_results #n = 400, tau = c(0.1,0.7,0.2), delta = 0.4
load("Mixing_1.3_equal.RData")
mixing_1.3_eq <- df_results #n = 400, tau = rep(1/3, 3), delta = 0.4


mix_1_3 <- rbind(cbind(mixing_1.3, "delta" = 0.4, "n_clust" = 3, "tau" = "not equal"),
                 cbind(mixing_1.3_eq, "delta" = 0.4, "n_clust" = 3, "tau" = "equal"),
                 cbind(mixing_1.3.1, "delta" = 0.55, "n_clust" = 3, "tau" = "not equal"),
                 cbind(mixing_1.3.1_eq, "delta" = 0.55, "n_clust" = 3, "tau" = "equal"))


load("Mixing_1.4.1.RData") #n = 600, tau = c(0.1,0.7,0.2), delta = 0.55
mixing_1.4.1 <- df_results
load("Mixing_1.4.1_equal.RData")
mixing_1.4.1_eq <- df_results
load("Mixing_1.4.RData")
mixing_1.4 <- df_results #n = 600, tau = c(0.1,0.7,0.2), delta = 0.4
load("Mixing_1.4_equal.RData")
mixing_1.4_eq <- df_results #n = 600, tau = rep(1/3, 3), delta = 0.4


mix_1_4 <- rbind(cbind(mixing_1.4, "delta" = 0.4, "n_clust" = 3, "tau" = "not equal"),
                 cbind(mixing_1.4_eq, "delta" = 0.4, "n_clust" = 3, "tau" = "equal"),
                 cbind(mixing_1.4.1, "delta" = 0.55, "n_clust" = 3, "tau" = "not equal"),
                 cbind(mixing_1.4.1_eq, "delta" = 0.55, "n_clust" = 3, "tau" = "equal"))

mix_1 <- rbind(cbind(mix_1_1, "n" = 100),
               cbind(mix_1_2, "n" = 200),
               cbind(mix_1_3, "n" = 400),
               cbind(mix_1_4, "n" = 600))





save(mix_1, file = "mix_1.RData")


# Primo scenario ----------------------------------------------------------

names(mix_1)[3] <-"Time_Kmodes"

colonne_tempo <- names(mix_1)[str_detect(names(mix_1), "^Time_")]
mix_1 <- mix_1 %>%
  mutate(across(all_of(colonne_tempo), ~as.numeric(str_replace(.x, "_secs", ""))))

ris_long <- mix_1 %>%
  pivot_longer(
    cols = -c(n,delta, tau, n_clust),
    names_to = c(".value", "Metodo"), 
    names_pattern = "(ARI|NMI|Time)_(.+)" 
  ) %>%
  mutate(Metodo = str_to_upper(Metodo))


summary_mix <- ris_long %>%
  group_by(n, Metodo, delta, tau, n_clust) %>%
  summarise(
    ARI_Media = mean(ARI, na.rm = TRUE),
    # ARI_Mediana = median(ARI, na.rm = TRUE),
    ARI_DevStandard = sd(ARI, na.rm = TRUE),
    NMI_Media = mean(NMI, na.rm = TRUE),
    # NMI_Mediana = median(NMI, na.rm = TRUE),
    NMI_DevStandard = sd(NMI, na.rm = TRUE),
    Tempo_Medio_sec = mean(Time, na.rm = TRUE), # Le colonne Time sono già numeriche e in secondi
    .groups = 'drop' # Rimuove il raggruppamento dopo summarise
  ) %>%
  select(-n_clust)


digits_colonne <- c(0, 0, 0, 2, 0, 6, 6, 6, 6, 6)
tabella_sim <- xtable(summary_mix, digits = digits_colonne)
print(tabella_sim, include.rownames = FALSE, type = "latex")


ris_long$tau <- as.factor(ris_long$tau)
levels(ris_long$tau) <- c("tau = (1/3,1/3,1/3)", "tau = (0.1,0.7,0.2)")




setwd("G:/My Drive/Università/Magistrale/Tesi/Studio di simulazione/Grafici/mixing_simulation")
ris_delta_.55 <- ris_long %>%
  select(tau,delta, n, ARI, Metodo)%>%
  filter(delta == 0.55, n == 100)


boxplot_delta.55 <- ggplot(ris_delta_.55, aes(x = Metodo, y = ARI, fill = Metodo))+
  geom_boxplot()+
  facet_wrap(~ tau, scales = "free_x")+
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1), 
        plot.title = element_text(hjust = 0.5), 
        strip.background = element_rect(fill="lightblue", color="black"), 
        strip.text = element_text(face="bold"), legend.position = "none")
print(boxplot_delta.55)
ggsave(boxplot_delta.55, file = "boxplot_delta.55_n100.pdf",
       width = 8, height = 6, units = "in")


#n = 200
ris_delta_.55_n200<- ris_long %>%
  select(tau,delta, n, ARI, Metodo)%>%
  filter(delta == 0.55, n == 200)


boxplot_delta.55_n200 <- ggplot(ris_delta_.55_n200, aes(x = Metodo, y = ARI, fill = Metodo))+
  geom_boxplot()+
  facet_wrap(~ tau, scales = "free_x")+
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1), 
        plot.title = element_text(hjust = 0.5), 
        strip.background = element_rect(fill="lightblue", color="black"), 
        strip.text = element_text(face="bold"), legend.position = "none")
print(boxplot_delta.55_n200)
ggsave(boxplot_delta.55_n200, file = "boxplot_delta.55_n200.pdf",
       width = 8, height = 6, units = "in")

#n = 400

ris_delta_.55_n400<- ris_long %>%
  select(tau,delta, n, ARI, Metodo)%>%
  filter(delta == 0.55, n == 400)


boxplot_delta.55_n400 <- ggplot(ris_delta_.55_n400, aes(x = Metodo, y = ARI, fill = Metodo))+
  geom_boxplot()+
  facet_wrap(~ tau, scales = "free_x")+
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1), 
        plot.title = element_text(hjust = 0.5), 
        strip.background = element_rect(fill="lightblue", color="black"), 
        strip.text = element_text(face="bold"), legend.position = "none")
print(boxplot_delta.55_n400)
ggsave(boxplot_delta.55_n400, file = "boxplot_delta.55_n400.pdf",
       width = 8, height = 6, units = "in")

#n = 600

ris_delta_.55_n600<- ris_long %>%
  select(tau,delta, n, ARI, Metodo)%>%
  filter(delta == 0.55, n == 600)


boxplot_delta.55_n600 <- ggplot(ris_delta_.55_n600, aes(x = Metodo, y = ARI, fill = Metodo))+
  geom_boxplot()+
  facet_wrap(~ tau, scales = "free_x")+
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1), 
        plot.title = element_text(hjust = 0.5), 
        strip.background = element_rect(fill="lightblue", color="black"), 
        strip.text = element_text(face="bold"), legend.position = "none")
print(boxplot_delta.55_n600)
ggsave(boxplot_delta.55_n600, file = "boxplot_delta.55_n600.pdf",
       width = 8, height = 6, units = "in")



#delta = 0.4

ris_delta_.4 <- ris_long %>%
  select(tau,delta, n, ARI, Metodo)%>%
  filter(delta == 0.4, n == 100)


boxplot_delta.4_n100 <- ggplot(ris_delta_.4, aes(x = Metodo, y = ARI, fill = Metodo))+
  geom_boxplot()+
  facet_wrap(~ tau, scales = "free_x")+
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1), 
        plot.title = element_text(hjust = 0.5), 
        strip.background = element_rect(fill="lightblue", color="black"), 
        strip.text = element_text(face="bold"), legend.position = "none")
print(boxplot_delta.4_n100)
ggsave(boxplot_delta.4_n100, file = "boxplot_delta.4_n100.pdf",
       width = 8, height = 6, units = "in")


#n = 200
ris_delta_.4_n200<- ris_long %>%
  select(tau,delta, n, ARI, Metodo)%>%
  filter(delta == 0.4, n == 200)


boxplot_delta.4_n200 <- ggplot(ris_delta_.4_n200, aes(x = Metodo, y = ARI, fill = Metodo))+
  geom_boxplot()+
  facet_wrap(~ tau, scales = "free_x")+
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1), 
        plot.title = element_text(hjust = 0.5), 
        strip.background = element_rect(fill="lightblue", color="black"), 
        strip.text = element_text(face="bold"), legend.position = "none")
print(boxplot_delta.4_n200)
ggsave(boxplot_delta.4_n200, file = "boxplot_delta.4_n200.pdf",
       width = 8, height = 6, units = "in")

#n = 400

ris_delta_.4_n400<- ris_long %>%
  select(tau,delta, n, ARI, Metodo)%>%
  filter(delta == 0.4, n == 400)


boxplot_delta.4_n400 <- ggplot(ris_delta_.4_n400, aes(x = Metodo, y = ARI, fill = Metodo))+
  geom_boxplot()+
  facet_wrap(~ tau, scales = "free_x")+
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1), 
        plot.title = element_text(hjust = 0.5), 
        strip.background = element_rect(fill="lightblue", color="black"), 
        strip.text = element_text(face="bold"), legend.position = "none")
print(boxplot_delta.4_n400)
ggsave(boxplot_delta.4_n400, file = "boxplot_delta.4_n400.pdf",
       width = 8, height = 6, units = "in")

#n = 600

ris_delta_.4_n600<- ris_long %>%
  select(tau,delta, n, ARI, Metodo)%>%
  filter(delta == 0.4, n == 600)


boxplot_delta.4_n600 <- ggplot(ris_delta_.4_n600, aes(x = Metodo, y = ARI, fill = Metodo))+
  geom_boxplot()+
  facet_wrap(~ tau, scales = "free_x")+
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1), 
        plot.title = element_text(hjust = 0.5), 
        strip.background = element_rect(fill="lightblue", color="black"), 
        strip.text = element_text(face="bold"), legend.position = "none")
print(boxplot_delta.4_n600)
ggsave(boxplot_delta.4_n600, file = "boxplot_delta.4_n600.pdf",
       width = 8, height = 6, units = "in")



#NMI 

ris_delta_.55 <- ris_long %>%
  select(tau,delta, n, NMI, Metodo)%>%
  filter(delta == 0.55, n == 100)


boxplot_delta.55 <- ggplot(ris_delta_.55, aes(x = Metodo, y = NMI, fill = Metodo))+
  geom_boxplot()+
  facet_wrap(~ tau, scales = "free_x")+
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1), 
        plot.title = element_text(hjust = 0.5), 
        strip.background = element_rect(fill="lightblue", color="black"), 
        strip.text = element_text(face="bold"), legend.position = "none")
print(boxplot_delta.55)
ggsave(boxplot_delta.55, file = "nmi_delta.55_n100.pdf",
       width = 8, height = 6, units = "in")


#n = 200
ris_delta_.55_n200<- ris_long %>%
  select(tau,delta, n, NMI, Metodo)%>%
  filter(delta == 0.55, n == 200)


boxplot_delta.55_n200 <- ggplot(ris_delta_.55_n200, aes(x = Metodo, y = NMI, fill = Metodo))+
  geom_boxplot()+
  facet_wrap(~ tau, scales = "free_x")+
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1), 
        plot.title = element_text(hjust = 0.5), 
        strip.background = element_rect(fill="lightblue", color="black"), 
        strip.text = element_text(face="bold"), legend.position = "none")
print(boxplot_delta.55_n200)
ggsave(boxplot_delta.55_n200, file = "nmi_delta.55_n200.pdf",
       width = 8, height = 6, units = "in")

#n = 400

ris_delta_.55_n400<- ris_long %>%
  select(tau,delta, n, NMI, Metodo)%>%
  filter(delta == 0.55, n == 400)


boxplot_delta.55_n400 <- ggplot(ris_delta_.55_n400, aes(x = Metodo, y = NMI, fill = Metodo))+
  geom_boxplot()+
  facet_wrap(~ tau, scales = "free_x")+
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1), 
        plot.title = element_text(hjust = 0.5), 
        strip.background = element_rect(fill="lightblue", color="black"), 
        strip.text = element_text(face="bold"), legend.position = "none")
print(boxplot_delta.55_n400)
ggsave(boxplot_delta.55_n400, file = "nmi_delta.55_n400.pdf",
       width = 8, height = 6, units = "in")

#n = 600

ris_delta_.55_n600<- ris_long %>%
  select(tau,delta, n, NMI, Metodo)%>%
  filter(delta == 0.55, n == 600)


boxplot_delta.55_n600 <- ggplot(ris_delta_.55_n600, aes(x = Metodo, y = NMI, fill = Metodo))+
  geom_boxplot()+
  facet_wrap(~ tau, scales = "free_x")+
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1), 
        plot.title = element_text(hjust = 0.5), 
        strip.background = element_rect(fill="lightblue", color="black"), 
        strip.text = element_text(face="bold"), legend.position = "none")
print(boxplot_delta.55_n600)
ggsave(boxplot_delta.55_n600, file = "nmi_delta.55_n600.pdf",
       width = 8, height = 6, units = "in")



#delta = 0.4

ris_delta_.4 <- ris_long %>%
  select(tau,delta, n, NMI, Metodo)%>%
  filter(delta == 0.4, n == 100)


boxplot_delta.4_n100 <- ggplot(ris_delta_.4, aes(x = Metodo, y = NMI, fill = Metodo))+
  geom_boxplot()+
  facet_wrap(~ tau, scales = "free_x")+
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1), 
        plot.title = element_text(hjust = 0.5), 
        strip.background = element_rect(fill="lightblue", color="black"), 
        strip.text = element_text(face="bold"), legend.position = "none")
print(boxplot_delta.4_n100)
ggsave(boxplot_delta.4_n100, file = "nmi_delta.4_n100.pdf",
       width = 8, height = 6, units = "in")


#n = 200
ris_delta_.4_n200<- ris_long %>%
  select(tau,delta, n, NMI, Metodo)%>%
  filter(delta == 0.4, n == 200)


boxplot_delta.4_n200 <- ggplot(ris_delta_.4_n200, aes(x = Metodo, y = NMI, fill = Metodo))+
  geom_boxplot()+
  facet_wrap(~ tau, scales = "free_x")+
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1), 
        plot.title = element_text(hjust = 0.5), 
        strip.background = element_rect(fill="lightblue", color="black"), 
        strip.text = element_text(face="bold"), legend.position = "none")
print(boxplot_delta.4_n200)
ggsave(boxplot_delta.4_n200, file = "nmi_delta.4_n200.pdf",
       width = 8, height = 6, units = "in")

#n = 400

ris_delta_.4_n400<- ris_long %>%
  select(tau,delta, n, NMI, Metodo)%>%
  filter(delta == 0.4, n == 400)


boxplot_delta.4_n400 <- ggplot(ris_delta_.4_n400, aes(x = Metodo, y = NMI, fill = Metodo))+
  geom_boxplot()+
  facet_wrap(~ tau, scales = "free_x")+
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1), 
        plot.title = element_text(hjust = 0.5), 
        strip.background = element_rect(fill="lightblue", color="black"), 
        strip.text = element_text(face="bold"), legend.position = "none")
print(boxplot_delta.4_n400)
ggsave(boxplot_delta.4_n400, file = "nmi_delta.4_n400.pdf",
       width = 8, height = 6, units = "in")

#n = 600

ris_delta_.4_n600<- ris_long %>%
  select(tau,delta, n, NMI, Metodo)%>%
  filter(delta == 0.4, n == 600)


boxplot_delta.4_n600 <- ggplot(ris_delta_.4_n600, aes(x = Metodo, y = NMI, fill = Metodo))+
  geom_boxplot()+
  facet_wrap(~ tau, scales = "free_x")+
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1), 
        plot.title = element_text(hjust = 0.5), 
        strip.background = element_rect(fill="lightblue", color="black"), 
        strip.text = element_text(face="bold"), legend.position = "none")
print(boxplot_delta.4_n600)
ggsave(boxplot_delta.4_n600, file = "nmi_delta.4_n600.pdf",
       width = 8, height = 6, units = "in")



rm(list=ls())


# Secondo scenario --------------------------------------------------------


setwd("G:/My Drive/Università/Magistrale/Tesi/Studio di simulazione/Risultati/Mixing")
load("Mixing_2.1.1.RData") #n = 100, tau = c(0.2,0.8), delta = 0.55
mixing_2.1.1 <- df_results
load("Mixing_2.1.1_equal.RData")
mixing_2.1.1_eq <- df_results
load("Mixing_2.1.RData")
mixing_2.1 <- df_results #n = 100, tau = c(0.2,0.8), delta = 0.4
load("Mixing_2.1_equal.RData")
mixing_2.1_eq <- df_results #n = 100, tau = rep(1/2, 2), delta = 0.4

mix_2_1 <- rbind(cbind(mixing_2.1, "delta" = 0.4, "tau" = "not equal"),
                 cbind(mixing_2.1_eq, "delta" = 0.4, "tau" = "equal"),
                 cbind(mixing_2.1.1, "delta" = 0.55, "tau" = "not equal"),
                 cbind(mixing_2.1.1_eq, "delta" = 0.55, "tau" = "equal"))

load("Mixing_2.2.1.RData") #n = 200, tau = c(0.2,0.8), delta = 0.55
mixing_2.2.1 <- df_results
load("Mixing_2.2.1_equal.RData")
mixing_2.2.1_eq <- df_results
load("Mixing_2.2.RData")
mixing_2.2 <- df_results #n = 200, tau = c(0.2,0.8), delta = 0.4
load("Mixing_2.2_equal.RData")
mixing_2.2_eq <- df_results #n = 200, tau = rep(1/2, 2), delta = 0.4

mix_2_2 <- rbind(cbind(mixing_2.2, "delta" = 0.4, "tau" = "not equal"),
                 cbind(mixing_2.2_eq, "delta" = 0.4, "tau" = "equal"),
                 cbind(mixing_2.2.1, "delta" = 0.55, "tau" = "not equal"),
                 cbind(mixing_2.2.1_eq, "delta" = 0.55, "tau" = "equal"))

load("Mixing_2.3.1.RData") #n = 400, tau = c(0.2,0.8), delta = 0.55
mixing_2.3.1 <- df_results
load("Mixing_2.3.1_equal.RData")
mixing_2.3.1_eq <- df_results
load("Mixing_2.3.RData")
mixing_2.3 <- df_results #n = 400, tau = c(0.2,0.8), delta = 0.4
load("Mixing_2.3_equal.RData")
mixing_2.3_eq <- df_results #n = 400, tau = rep(1/2, 2), delta = 0.4

mix_2_3 <- rbind(cbind(mixing_2.3, "delta" = 0.4, "tau" = "not equal"),
                 cbind(mixing_2.3_eq, "delta" = 0.4, "tau" = "equal"),
                 cbind(mixing_2.3.1, "delta" = 0.55, "tau" = "not equal"),
                 cbind(mixing_2.3.1_eq, "delta" = 0.55, "tau" = "equal"))

load("Mixing_2.4.1.RData") #n = 600, tau = c(0.2,0.8), delta = 0.55
mixing_2.4.1 <- df_results
load("Mixing_2.4.1_equal.RData")
mixing_2.4.1_eq <- df_results
load("Mixing_2.4.RData")
mixing_2.4 <- df_results #n = 600, tau = c(0.2,0.8), delta = 0.4
load("Mixing_2.4_equal.RData")
mixing_2.4_eq <- df_results #n = 600, tau = rep(1/2, 2), delta = 0.4

mix_2_4 <- rbind(cbind(mixing_2.4, "delta" = 0.4, "tau" = "not equal"),
                 cbind(mixing_2.4_eq, "delta" = 0.4, "tau" = "equal"),
                 cbind(mixing_2.4.1, "delta" = 0.55, "tau" = "not equal"),
                 cbind(mixing_2.4.1_eq, "delta" = 0.55, "tau" = "equal"))

mix_2 <- rbind(cbind(mix_2_1, "n" = 100),
               cbind(mix_2_2, "n" = 200),
               cbind(mix_2_3, "n" = 400),
               cbind(mix_2_4, "n" = 600))


names(mix_2)[3] <-"Time_Kmodes"

colonne_tempo <- names(mix_2)[str_detect(names(mix_2), "^Time_")]
mix_2 <- mix_2 %>%
  mutate(across(all_of(colonne_tempo), ~as.numeric(str_replace(.x, "_secs", ""))))

ris_long <- mix_2 %>%
  pivot_longer(
    cols = -c(n,delta, tau),
    names_to = c(".value", "Metodo"), 
    names_pattern = "(ARI|NMI|Time)_(.+)" 
  ) %>%
  mutate(Metodo = str_to_upper(Metodo))


summary_mix <- ris_long %>%
  group_by(n, Metodo, delta, tau) %>%
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


digits_colonne <- c(0, 0, 0, 2, 0, 6, 6, 6, 6, 6)
tabella_sim <- xtable(summary_mix, digits = digits_colonne)
print(tabella_sim, include.rownames = FALSE, type = "latex")

ris_long$tau <- as.factor(ris_long$tau)
levels(ris_long$tau) <- c("tau = (1/2,1/2)", "tau = (0.2,0.8)")


# Analisi grafica

setwd("G:/My Drive/Università/Magistrale/Tesi/Studio di simulazione/Grafici/mixing_simulation/mix_2")
ris_delta_.55 <- ris_long %>%
  select(tau,delta, n, ARI, Metodo)%>%
  filter(delta == 0.55, n == 100)


boxplot_delta.55 <- ggplot(ris_delta_.55, aes(x = Metodo, y = ARI, fill = Metodo))+
  geom_boxplot()+
  facet_wrap(~ tau, scales = "free_x")+
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1), 
        plot.title = element_text(hjust = 0.5), 
        strip.background = element_rect(fill="lightblue", color="black"), 
        strip.text = element_text(face="bold"), legend.position = "none")
print(boxplot_delta.55)
ggsave(boxplot_delta.55, file = "mix2_delta.55_n100.pdf",
       width = 10, height = 6, units = "in")


#n = 200
ris_delta_.55_n200<- ris_long %>%
  select(tau,delta, n, ARI, Metodo)%>%
  filter(delta == 0.55, n == 200)


boxplot_delta.55_n200 <- ggplot(ris_delta_.55_n200, aes(x = Metodo, y = ARI, fill = Metodo))+
  geom_boxplot()+
  facet_wrap(~ tau, scales = "free_x")+
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1), 
        plot.title = element_text(hjust = 0.5), 
        strip.background = element_rect(fill="lightblue", color="black"), 
        strip.text = element_text(face="bold"), legend.position = "none")
print(boxplot_delta.55_n200)
ggsave(boxplot_delta.55_n200, file = "mix2_delta.55_n200.pdf",
       width = 10, height = 6, units = "in")

#n = 400

ris_delta_.55_n400<- ris_long %>%
  select(tau,delta, n, ARI, Metodo)%>%
  filter(delta == 0.55, n == 400)


boxplot_delta.55_n400 <- ggplot(ris_delta_.55_n400, aes(x = Metodo, y = ARI, fill = Metodo))+
  geom_boxplot()+
  facet_wrap(~ tau, scales = "free_x")+
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1), 
        plot.title = element_text(hjust = 0.5), 
        strip.background = element_rect(fill="lightblue", color="black"), 
        strip.text = element_text(face="bold"), legend.position = "none")
print(boxplot_delta.55_n400)
ggsave(boxplot_delta.55_n400, file = "mix2_delta.55_n400.pdf",
       width = 10, height = 6, units = "in")

#n = 600

ris_delta_.55_n600<- ris_long %>%
  select(tau,delta, n, ARI, Metodo)%>%
  filter(delta == 0.55, n == 600)


boxplot_delta.55_n600 <- ggplot(ris_delta_.55_n600, aes(x = Metodo, y = ARI, fill = Metodo))+
  geom_boxplot()+
  facet_wrap(~ tau, scales = "free_x")+
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1), 
        plot.title = element_text(hjust = 0.5), 
        strip.background = element_rect(fill="lightblue", color="black"), 
        strip.text = element_text(face="bold"), legend.position = "none")
print(boxplot_delta.55_n600)
ggsave(boxplot_delta.55_n600, file = "mix2_delta.55_n600.pdf",
       width = 10, height = 6, units = "in")



#delta = 0.4

ris_delta_.4 <- ris_long %>%
  select(tau,delta, n, ARI, Metodo)%>%
  filter(delta == 0.4, n == 100)


boxplot_delta.4_n100 <- ggplot(ris_delta_.4, aes(x = Metodo, y = ARI, fill = Metodo))+
  geom_boxplot()+
  facet_wrap(~ tau, scales = "free_x")+
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1), 
        plot.title = element_text(hjust = 0.5), 
        strip.background = element_rect(fill="lightblue", color="black"), 
        strip.text = element_text(face="bold"), legend.position = "none")
print(boxplot_delta.4_n100)
ggsave(boxplot_delta.4_n100, file = "mix2_delta.4_n100.pdf",
       width = 10, height = 6, units = "in")


#n = 200
ris_delta_.4_n200<- ris_long %>%
  select(tau,delta, n, ARI, Metodo)%>%
  filter(delta == 0.4, n == 200)


boxplot_delta.4_n200 <- ggplot(ris_delta_.4_n200, aes(x = Metodo, y = ARI, fill = Metodo))+
  geom_boxplot()+
  facet_wrap(~ tau, scales = "free_x")+
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1), 
        plot.title = element_text(hjust = 0.5), 
        strip.background = element_rect(fill="lightblue", color="black"), 
        strip.text = element_text(face="bold"), legend.position = "none")
print(boxplot_delta.4_n200)
ggsave(boxplot_delta.4_n200, file = "mix2_delta.4_n200.pdf",
       width = 10, height = 6, units = "in")

#n = 400

ris_delta_.4_n400<- ris_long %>%
  select(tau,delta, n, ARI, Metodo)%>%
  filter(delta == 0.4, n == 400)


boxplot_delta.4_n400 <- ggplot(ris_delta_.4_n400, aes(x = Metodo, y = ARI, fill = Metodo))+
  geom_boxplot()+
  facet_wrap(~ tau, scales = "free_x")+
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1), 
        plot.title = element_text(hjust = 0.5), 
        strip.background = element_rect(fill="lightblue", color="black"), 
        strip.text = element_text(face="bold"), legend.position = "none")
print(boxplot_delta.4_n400)
ggsave(boxplot_delta.4_n400, file = "mix2_delta.4_n400.pdf",
       width = 10, height = 6, units = "in")

#n = 600

ris_delta_.4_n600<- ris_long %>%
  select(tau,delta, n, ARI, Metodo)%>%
  filter(delta == 0.4, n == 600)


boxplot_delta.4_n600 <- ggplot(ris_delta_.4_n600, aes(x = Metodo, y = ARI, fill = Metodo))+
  geom_boxplot()+
  facet_wrap(~ tau, scales = "free_x")+
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1), 
        plot.title = element_text(hjust = 0.5), 
        strip.background = element_rect(fill="lightblue", color="black"), 
        strip.text = element_text(face="bold"), legend.position = "none")
print(boxplot_delta.4_n600)
ggsave(boxplot_delta.4_n600, file = "mix2_delta.4_n600.pdf",
       width = 10, height = 6, units = "in")



#NMI 

ris_delta_.55 <- ris_long %>%
  select(tau,delta, n, NMI, Metodo)%>%
  filter(delta == 0.55, n == 100)


boxplot_delta.55 <- ggplot(ris_delta_.55, aes(x = Metodo, y = NMI, fill = Metodo))+
  geom_boxplot()+
  facet_wrap(~ tau, scales = "free_x")+
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1), 
        plot.title = element_text(hjust = 0.5), 
        strip.background = element_rect(fill="lightblue", color="black"), 
        strip.text = element_text(face="bold"), legend.position = "none")
print(boxplot_delta.55)
ggsave(boxplot_delta.55, file = "mix2nmi_delta.55_n100.pdf",
       width = 10, height = 6, units = "in")


#n = 200
ris_delta_.55_n200<- ris_long %>%
  select(tau,delta, n, NMI, Metodo)%>%
  filter(delta == 0.55, n == 200)


boxplot_delta.55_n200 <- ggplot(ris_delta_.55_n200, aes(x = Metodo, y = NMI, fill = Metodo))+
  geom_boxplot()+
  facet_wrap(~ tau, scales = "free_x")+
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1), 
        plot.title = element_text(hjust = 0.5), 
        strip.background = element_rect(fill="lightblue", color="black"), 
        strip.text = element_text(face="bold"), legend.position = "none")
print(boxplot_delta.55_n200)
ggsave(boxplot_delta.55_n200, file = "mix2nmi_delta.55_n200.pdf",
       width = 10, height = 6, units = "in")

#n = 400

ris_delta_.55_n400<- ris_long %>%
  select(tau,delta, n, NMI, Metodo)%>%
  filter(delta == 0.55, n == 400)


boxplot_delta.55_n400 <- ggplot(ris_delta_.55_n400, aes(x = Metodo, y = NMI, fill = Metodo))+
  geom_boxplot()+
  facet_wrap(~ tau, scales = "free_x")+
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1), 
        plot.title = element_text(hjust = 0.5), 
        strip.background = element_rect(fill="lightblue", color="black"), 
        strip.text = element_text(face="bold"), legend.position = "none")
print(boxplot_delta.55_n400)
ggsave(boxplot_delta.55_n400, file = "mix2nmi_delta.55_n400.pdf",
       width = 10, height = 6, units = "in")

#n = 600

ris_delta_.55_n600<- ris_long %>%
  select(tau,delta, n, NMI, Metodo)%>%
  filter(delta == 0.55, n == 600)


boxplot_delta.55_n600 <- ggplot(ris_delta_.55_n600, aes(x = Metodo, y = NMI, fill = Metodo))+
  geom_boxplot()+
  facet_wrap(~ tau, scales = "free_x")+
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1), 
        plot.title = element_text(hjust = 0.5), 
        strip.background = element_rect(fill="lightblue", color="black"), 
        strip.text = element_text(face="bold"), legend.position = "none")
print(boxplot_delta.55_n600)
ggsave(boxplot_delta.55_n600, file = "mix2nmi_delta.55_n600.pdf",
       width = 10, height = 6, units = "in")



#delta = 0.4

ris_delta_.4 <- ris_long %>%
  select(tau,delta, n, NMI, Metodo)%>%
  filter(delta == 0.4, n == 100)


boxplot_delta.4_n100 <- ggplot(ris_delta_.4, aes(x = Metodo, y = NMI, fill = Metodo))+
  geom_boxplot()+
  facet_wrap(~ tau, scales = "free_x")+
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1), 
        plot.title = element_text(hjust = 0.5), 
        strip.background = element_rect(fill="lightblue", color="black"), 
        strip.text = element_text(face="bold"), legend.position = "none")
print(boxplot_delta.4_n100)
ggsave(boxplot_delta.4_n100, file = "mix2nmi_delta.4_n100.pdf",
       width = 10, height = 6, units = "in")


#n = 200
ris_delta_.4_n200<- ris_long %>%
  select(tau,delta, n, NMI, Metodo)%>%
  filter(delta == 0.4, n == 200)


boxplot_delta.4_n200 <- ggplot(ris_delta_.4_n200, aes(x = Metodo, y = NMI, fill = Metodo))+
  geom_boxplot()+
  facet_wrap(~ tau, scales = "free_x")+
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1), 
        plot.title = element_text(hjust = 0.5), 
        strip.background = element_rect(fill="lightblue", color="black"), 
        strip.text = element_text(face="bold"), legend.position = "none")
print(boxplot_delta.4_n200)
ggsave(boxplot_delta.4_n200, file = "mix2nmi_delta.4_n200.pdf",
       width = 10, height = 6, units = "in")

#n = 400

ris_delta_.4_n400<- ris_long %>%
  select(tau,delta, n, NMI, Metodo)%>%
  filter(delta == 0.4, n == 400)


boxplot_delta.4_n400 <- ggplot(ris_delta_.4_n400, aes(x = Metodo, y = NMI, fill = Metodo))+
  geom_boxplot()+
  facet_wrap(~ tau, scales = "free_x")+
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1), 
        plot.title = element_text(hjust = 0.5), 
        strip.background = element_rect(fill="lightblue", color="black"), 
        strip.text = element_text(face="bold"), legend.position = "none")
print(boxplot_delta.4_n400)
ggsave(boxplot_delta.4_n400, file = "mix2nmi_delta.4_n400.pdf",
       width = 10, height = 6, units = "in")

#n = 600

ris_delta_.4_n600<- ris_long %>%
  select(tau,delta, n, NMI, Metodo)%>%
  filter(delta == 0.4, n == 600)


boxplot_delta.4_n600 <- ggplot(ris_delta_.4_n600, aes(x = Metodo, y = NMI, fill = Metodo))+
  geom_boxplot()+
  facet_wrap(~ tau, scales = "free_x")+
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1), 
        plot.title = element_text(hjust = 0.5), 
        strip.background = element_rect(fill="lightblue", color="black"), 
        strip.text = element_text(face="bold"), legend.position = "none")
print(boxplot_delta.4_n600)
ggsave(boxplot_delta.4_n600, file = "mix2nmi_delta.4_n600.pdf",
       width = 10, height = 6, units = "in")


# terzo scenario ----------------------------------------------------------

rm(list=ls())

setwd("G:/My Drive/Università/Magistrale/Tesi/Studio di simulazione/Risultati/Mixing")
load("Mixing_3.1.1.RData") #n = 100, tau = c(0.2, 0.6, 0.1, 0.1), delta = 0.55
mixing_3.1.1 <- df_results
load("Mixing_3.1.1_equal.RData")
mixing_3.1.1_eq <- df_results
load("Mixing_3.1.RData")
mixing_3.1 <- df_results #n = 100, tau = c(0.2, 0.6, 0.1, 0.1), delta = 0.4
load("Mixing_3.1_equal.RData")
mixing_3.1_eq <- df_results #n = 100, tau = rep(1/4, 4), delta = 0.4

mix_3_1 <- rbind(cbind(mixing_3.1, "delta" = 0.4, "tau" = "not equal"),
                 cbind(mixing_3.1_eq, "delta" = 0.4, "tau" = "equal"),
                 cbind(mixing_3.1.1, "delta" = 0.55, "tau" = "not equal"),
                 cbind(mixing_3.1.1_eq, "delta" = 0.55, "tau" = "equal"))

load("Mixing_3.2.1.RData") #n = 200, tau = c(0.2, 0.6, 0.1, 0.1), delta = 0.55
mixing_3.2.1 <- df_results
load("Mixing_3.2.1_equal.RData")
mixing_3.2.1_eq <- df_results
load("Mixing_3.2.RData")
mixing_3.2 <- df_results #n = 200, tau = c(0.2, 0.6, 0.1, 0.1), delta = 0.4
load("Mixing_3.2_equal.RData")
mixing_3.2_eq <- df_results #n = 200, tau = rep(1/4, 4), delta = 0.4

mix_3_2 <- rbind(cbind(mixing_3.2, "delta" = 0.4, "tau" = "not equal"),
                 cbind(mixing_3.2_eq, "delta" = 0.4, "tau" = "equal"),
                 cbind(mixing_3.2.1, "delta" = 0.55, "tau" = "not equal"),
                 cbind(mixing_3.2.1_eq, "delta" = 0.55, "tau" = "equal"))

load("Mixing_3.3.1.RData") #n = 400, tau = c(0.2, 0.6, 0.1, 0.1), delta = 0.55
mixing_3.3.1 <- df_results
load("Mixing_3.3.1_equal.RData")
mixing_3.3.1_eq <- df_results
load("Mixing_3.3.RData")
mixing_3.3 <- df_results #n = 400, tau = c(0.2, 0.6, 0.1, 0.1), delta = 0.4
load("Mixing_3.3_equal.RData")
mixing_3.3_eq <- df_results #n = 400, tau = rep(1/4, 4), delta = 0.4

mix_3_3 <- rbind(cbind(mixing_3.3, "delta" = 0.4, "tau" = "not equal"),
                 cbind(mixing_3.3_eq, "delta" = 0.4, "tau" = "equal"),
                 cbind(mixing_3.3.1, "delta" = 0.55, "tau" = "not equal"),
                 cbind(mixing_3.3.1_eq, "delta" = 0.55, "tau" = "equal"))

load("Mixing_3.4.1.RData") #n = 600, tau = c(0.2, 0.6, 0.1, 0.1), delta = 0.55
mixing_3.4.1 <- df_results
load("Mixing_3.4.1_equal.RData")
mixing_3.4.1_eq <- df_results
load("Mixing_3.4.RData")
mixing_3.4 <- df_results #n = 600, tau = c(0.2, 0.6, 0.1, 0.1), delta = 0.4
load("Mixing_3.4_equal.RData")
mixing_3.4_eq <- df_results #n = 600, tau = rep(1/2, 2), delta = 0.4

mix_3_4 <- rbind(cbind(mixing_3.4, "delta" = 0.4, "tau" = "not equal"),
                 cbind(mixing_3.4_eq, "delta" = 0.4, "tau" = "equal"),
                 cbind(mixing_3.4.1, "delta" = 0.55, "tau" = "not equal"),
                 cbind(mixing_3.4.1_eq, "delta" = 0.55, "tau" = "equal"))

mix_3 <- rbind(cbind(mix_3_1, "n" = 100),
               cbind(mix_3_2, "n" = 200),
               cbind(mix_3_3, "n" = 400),
               cbind(mix_3_4, "n" = 600))


names(mix_3)[3] <-"Time_Kmodes"

colonne_tempo <- names(mix_3)[str_detect(names(mix_3), "^Time_")]
mix_3 <- mix_3 %>%
  mutate(across(all_of(colonne_tempo), ~as.numeric(str_replace(.x, "_secs", ""))))

ris_long <- mix_3 %>%
  pivot_longer(
    cols = -c(n,delta, tau),
    names_to = c(".value", "Metodo"), 
    names_pattern = "(ARI|NMI|Time)_(.+)" 
  ) %>%
  mutate(Metodo = str_to_upper(Metodo))


ris_long$Metodo <- factor(ris_long$Metodo)
ris_long$tau <- factor(ris_long$tau)
ris_long$n <- factor(ris_long$n) # Converti n in fattore se vuoi testare le varianze per ogni livello di n
ris_long$delta <- factor(ris_long$delta)


summary_mix <- ris_long %>%
  group_by(n, Metodo, delta, tau) %>%
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

summary_mix %>%
  filter(n == 600 , Metodo == "BAYES_STD" | Metodo == "GBPP" , delta == 0.55)




digits_colonne <- c(0, 0, 0, 2, 0, 6, 6, 6, 6, 6)
tabella_sim <- xtable(summary_mix, digits = digits_colonne)
print(tabella_sim, include.rownames = FALSE, type = "latex")


summary_stdev <- ris_long %>%
  group_by(n, Metodo, delta, tau) %>%
  filter(delta == 0.55, tau == "tau = (0.2, 0.6, 0.1, 0.1) ") %>%
  summarise(
    ARI_DevStandard = sd(ARI, na.rm = TRUE),
    NMI_DevStandard = sd(NMI, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  select(-delta, -tau)






digits_colonne <- c(0, 0, 0, 6, 6)
tabella_sim <- xtable(summary_stdev, digits = digits_colonne)
print(tabella_sim, include.rownames = FALSE, type = "latex")




# Analisi grafica

ris_long$tau <- as.factor(ris_long$tau)
levels(ris_long$tau) <- c("tau = (1/4,1/4, 1/4, 1/4)", "tau = (0.2, 0.6, 0.1, 0.1) ")

setwd("G:/My Drive/Università/Magistrale/Tesi/Studio di simulazione/Grafici/mixing_simulation/mix_3")
ris_delta_.55 <- ris_long %>%
  select(tau,delta, n, ARI, Metodo)%>%
  filter(delta == 0.55, n == 100)


boxplot_delta.55 <- ggplot(ris_delta_.55, aes(x = Metodo, y = ARI, fill = Metodo))+
  geom_boxplot()+
  facet_wrap(~ tau, scales = "free_x")+
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1), 
        plot.title = element_text(hjust = 0.5), 
        strip.background = element_rect(fill="lightblue", color="black"), 
        strip.text = element_text(face="bold"), legend.position = "none")
print(boxplot_delta.55)
ggsave(boxplot_delta.55, file = "mix3_delta.55_n100.pdf",
       width = 10, height = 6, units = "in")


#n = 200
ris_delta_.55_n200<- ris_long %>%
  select(tau,delta, n, ARI, Metodo)%>%
  filter(delta == 0.55, n == 200)


boxplot_delta.55_n200 <- ggplot(ris_delta_.55_n200, aes(x = Metodo, y = ARI, fill = Metodo))+
  geom_boxplot()+
  facet_wrap(~ tau, scales = "free_x")+
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1), 
        plot.title = element_text(hjust = 0.5), 
        strip.background = element_rect(fill="lightblue", color="black"), 
        strip.text = element_text(face="bold"), legend.position = "none")
print(boxplot_delta.55_n200)
ggsave(boxplot_delta.55_n200, file = "mix3_delta.55_n200.pdf",
       width = 10, height = 6, units = "in")

#n = 400

ris_delta_.55_n400<- ris_long %>%
  select(tau,delta, n, ARI, Metodo)%>%
  filter(delta == 0.55, n == 400)


boxplot_delta.55_n400 <- ggplot(ris_delta_.55_n400, aes(x = Metodo, y = ARI, fill = Metodo))+
  geom_boxplot()+
  facet_wrap(~ tau, scales = "free_x")+
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1), 
        plot.title = element_text(hjust = 0.5), 
        strip.background = element_rect(fill="lightblue", color="black"), 
        strip.text = element_text(face="bold"), legend.position = "none")
print(boxplot_delta.55_n400)
ggsave(boxplot_delta.55_n400, file = "mix3_delta.55_n400.pdf",
       width = 10, height = 6, units = "in")

#n = 600

ris_delta_.55_n600<- ris_long %>%
  select(tau,delta, n, ARI, Metodo)%>%
  filter(delta == 0.55, n == 600)


boxplot_delta.55_n600 <- ggplot(ris_delta_.55_n600, aes(x = Metodo, y = ARI, fill = Metodo))+
  geom_boxplot()+
  facet_wrap(~ tau, scales = "free_x")+
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1), 
        plot.title = element_text(hjust = 0.5), 
        strip.background = element_rect(fill="lightblue", color="black"), 
        strip.text = element_text(face="bold"), legend.position = "none")
print(boxplot_delta.55_n600)
ggsave(boxplot_delta.55_n600, file = "mix3_delta.55_n600.pdf",
       width = 10, height = 6, units = "in")



#delta = 0.4

ris_delta_.4 <- ris_long %>%
  select(tau,delta, n, ARI, Metodo)%>%
  filter(delta == 0.4, n == 100)


boxplot_delta.4_n100 <- ggplot(ris_delta_.4, aes(x = Metodo, y = ARI, fill = Metodo))+
  geom_boxplot()+
  facet_wrap(~ tau, scales = "free_x")+
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1), 
        plot.title = element_text(hjust = 0.5), 
        strip.background = element_rect(fill="lightblue", color="black"), 
        strip.text = element_text(face="bold"), legend.position = "none")
print(boxplot_delta.4_n100)
ggsave(boxplot_delta.4_n100, file = "mix3_delta.4_n100.pdf",
       width = 10, height = 6, units = "in")


#n = 200
ris_delta_.4_n200<- ris_long %>%
  select(tau,delta, n, ARI, Metodo)%>%
  filter(delta == 0.4, n == 200)


boxplot_delta.4_n200 <- ggplot(ris_delta_.4_n200, aes(x = Metodo, y = ARI, fill = Metodo))+
  geom_boxplot()+
  facet_wrap(~ tau, scales = "free_x")+
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1), 
        plot.title = element_text(hjust = 0.5), 
        strip.background = element_rect(fill="lightblue", color="black"), 
        strip.text = element_text(face="bold"), legend.position = "none")
print(boxplot_delta.4_n200)
ggsave(boxplot_delta.4_n200, file = "mix3_delta.4_n200.pdf",
       width = 10, height = 6, units = "in")

#n = 400

ris_delta_.4_n400<- ris_long %>%
  select(tau,delta, n, ARI, Metodo)%>%
  filter(delta == 0.4, n == 400)


boxplot_delta.4_n400 <- ggplot(ris_delta_.4_n400, aes(x = Metodo, y = ARI, fill = Metodo))+
  geom_boxplot()+
  facet_wrap(~ tau, scales = "free_x")+
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1), 
        plot.title = element_text(hjust = 0.5), 
        strip.background = element_rect(fill="lightblue", color="black"), 
        strip.text = element_text(face="bold"), legend.position = "none")
print(boxplot_delta.4_n400)
ggsave(boxplot_delta.4_n400, file = "mix3_delta.4_n400.pdf",
       width = 10, height = 6, units = "in")

#n = 600

ris_delta_.4_n600<- ris_long %>%
  select(tau,delta, n, ARI, Metodo)%>%
  filter(delta == 0.4, n == 600)


boxplot_delta.4_n600 <- ggplot(ris_delta_.4_n600, aes(x = Metodo, y = ARI, fill = Metodo))+
  geom_boxplot()+
  facet_wrap(~ tau, scales = "free_x")+
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1), 
        plot.title = element_text(hjust = 0.5), 
        strip.background = element_rect(fill="lightblue", color="black"), 
        strip.text = element_text(face="bold"), legend.position = "none")
print(boxplot_delta.4_n600)
ggsave(boxplot_delta.4_n600, file = "mix3_delta.4_n600.pdf",
       width = 10, height = 6, units = "in")



#NMI 

ris_delta_.55 <- ris_long %>%
  select(tau,delta, n, NMI, Metodo)%>%
  filter(delta == 0.55, n == 100)


boxplot_delta.55 <- ggplot(ris_delta_.55, aes(x = Metodo, y = NMI, fill = Metodo))+
  geom_boxplot()+
  facet_wrap(~ tau, scales = "free_x")+
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1), 
        plot.title = element_text(hjust = 0.5), 
        strip.background = element_rect(fill="lightblue", color="black"), 
        strip.text = element_text(face="bold"), legend.position = "none")
print(boxplot_delta.55)
ggsave(boxplot_delta.55, file = "mix3nmi_delta.55_n100.pdf",
       width = 10, height = 6, units = "in")


#n = 200
ris_delta_.55_n200<- ris_long %>%
  select(tau,delta, n, NMI, Metodo)%>%
  filter(delta == 0.55, n == 200)


boxplot_delta.55_n200 <- ggplot(ris_delta_.55_n200, aes(x = Metodo, y = NMI, fill = Metodo))+
  geom_boxplot()+
  facet_wrap(~ tau, scales = "free_x")+
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1), 
        plot.title = element_text(hjust = 0.5), 
        strip.background = element_rect(fill="lightblue", color="black"), 
        strip.text = element_text(face="bold"), legend.position = "none")
print(boxplot_delta.55_n200)
ggsave(boxplot_delta.55_n200, file = "mix3nmi_delta.55_n200.pdf",
       width = 10, height = 6, units = "in")

#n = 400

ris_delta_.55_n400<- ris_long %>%
  select(tau,delta, n, NMI, Metodo)%>%
  filter(delta == 0.55, n == 400)


boxplot_delta.55_n400 <- ggplot(ris_delta_.55_n400, aes(x = Metodo, y = NMI, fill = Metodo))+
  geom_boxplot()+
  facet_wrap(~ tau, scales = "free_x")+
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1), 
        plot.title = element_text(hjust = 0.5), 
        strip.background = element_rect(fill="lightblue", color="black"), 
        strip.text = element_text(face="bold"), legend.position = "none")
print(boxplot_delta.55_n400)
ggsave(boxplot_delta.55_n400, file = "mix3nmi_delta.55_n400.pdf",
       width = 10, height = 6, units = "in")

#n = 600

ris_delta_.55_n600<- ris_long %>%
  select(tau,delta, n, NMI, Metodo)%>%
  filter(delta == 0.55, n == 600)


boxplot_delta.55_n600 <- ggplot(ris_delta_.55_n600, aes(x = Metodo, y = NMI, fill = Metodo))+
  geom_boxplot()+
  facet_wrap(~ tau, scales = "free_x")+
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1), 
        plot.title = element_text(hjust = 0.5), 
        strip.background = element_rect(fill="lightblue", color="black"), 
        strip.text = element_text(face="bold"), legend.position = "none")
print(boxplot_delta.55_n600)
ggsave(boxplot_delta.55_n600, file = "mix3nmi_delta.55_n600.pdf",
       width = 10, height = 6, units = "in")



#delta = 0.4

ris_delta_.4 <- ris_long %>%
  select(tau,delta, n, NMI, Metodo)%>%
  filter(delta == 0.4, n == 100)


boxplot_delta.4_n100 <- ggplot(ris_delta_.4, aes(x = Metodo, y = NMI, fill = Metodo))+
  geom_boxplot()+
  facet_wrap(~ tau, scales = "free_x")+
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1), 
        plot.title = element_text(hjust = 0.5), 
        strip.background = element_rect(fill="lightblue", color="black"), 
        strip.text = element_text(face="bold"), legend.position = "none")
print(boxplot_delta.4_n100)
ggsave(boxplot_delta.4_n100, file = "mix3nmi_delta.4_n100.pdf",
       width = 10, height = 6, units = "in")


#n = 200
ris_delta_.4_n200<- ris_long %>%
  select(tau,delta, n, NMI, Metodo)%>%
  filter(delta == 0.4, n == 200)


boxplot_delta.4_n200 <- ggplot(ris_delta_.4_n200, aes(x = Metodo, y = NMI, fill = Metodo))+
  geom_boxplot()+
  facet_wrap(~ tau, scales = "free_x")+
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1), 
        plot.title = element_text(hjust = 0.5), 
        strip.background = element_rect(fill="lightblue", color="black"), 
        strip.text = element_text(face="bold"), legend.position = "none")
print(boxplot_delta.4_n200)
ggsave(boxplot_delta.4_n200, file = "mix3nmi_delta.4_n200.pdf",
       width = 10, height = 6, units = "in")

#n = 400

ris_delta_.4_n400<- ris_long %>%
  select(tau,delta, n, NMI, Metodo)%>%
  filter(delta == 0.4, n == 400)


boxplot_delta.4_n400 <- ggplot(ris_delta_.4_n400, aes(x = Metodo, y = NMI, fill = Metodo))+
  geom_boxplot()+
  facet_wrap(~ tau, scales = "free_x")+
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1), 
        plot.title = element_text(hjust = 0.5), 
        strip.background = element_rect(fill="lightblue", color="black"), 
        strip.text = element_text(face="bold"), legend.position = "none")
print(boxplot_delta.4_n400)
ggsave(boxplot_delta.4_n400, file = "mix3nmi_delta.4_n400.pdf",
       width = 10, height = 6, units = "in")

#n = 600

ris_delta_.4_n600<- ris_long %>%
  select(tau,delta, n, NMI, Metodo)%>%
  filter(delta == 0.4, n == 600)


boxplot_delta.4_n600 <- ggplot(ris_delta_.4_n600, aes(x = Metodo, y = NMI, fill = Metodo))+
  geom_boxplot()+
  facet_wrap(~ tau, scales = "free_x")+
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1), 
        plot.title = element_text(hjust = 0.5), 
        strip.background = element_rect(fill="lightblue", color="black"), 
        strip.text = element_text(face="bold"), legend.position = "none")
print(boxplot_delta.4_n600)
ggsave(boxplot_delta.4_n600, file = "mix3nmi_delta.4_n600.pdf",
       width = 10, height = 6, units = "in")




