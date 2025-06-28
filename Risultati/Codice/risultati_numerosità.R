rm(list = ls());graphics.off(); gc()
setwd("G:/My Drive/Università/Magistrale/Tesi/Studio di simulazione/Risultati/Numerosità")
load("simulazione_1.1.RData") #delta = 0.25, n = 100
load("simulazione_1.2.RData") # delta = 0.25, n = 200
load("simulazione_1.3.RData") #delta = 0.25, n = 400
load("simulazione_2.1.RData") #delta = 0.4, n = 100
load("simulazione_2.2.RData") #delta = 0.4, n = 200
load("simulazione_2.3.RData") #delta = 0.4, n = 400
load("simulazione_3.1.RData") #delta = 0.55, n = 100
load("simulazione_3.2.RData") #delta = 0.55, n = 200
load("simulazione_3.3.RData") #delta = 0.55, n = 400
load("simulazione_4.1.RData") #delta = 0.7, n = 100
load("simulazione_4.2.RData") #delta = 0.7, n = 200
load("simulazione_4.3.RData") #delta = 0.7, n = 400


sim_1 <- rbind(cbind(simulazione_1.1, "n" = 100), cbind(simulazione_1.2, "n" = 200), cbind(simulazione_1.3, "n" = 400))
sim_1 <- cbind(sim_1, "delta" = 0.25)

sim_2 <- rbind(cbind(simulazione_2.1, "n" = 100), cbind(simulazione_2.2, "n" = 200), cbind(simulazione_2.3, "n" = 400))
sim_2 <- cbind(sim_2, "delta" = 0.4)

sim_3 <- rbind(cbind(simulazione_3.1, "n" = 100), cbind(simulazione_3.2, "n" = 200), cbind(simulazione_3.3, "n" = 400))
sim_3 <- cbind(sim_3, "delta" = 0.55)

sim_4 <- rbind(cbind(simulazione_4.1, "n" = 100), cbind(simulazione_4.2, "n" = 200), cbind(simulazione_4.3, "n" = 400))
sim_4 <- cbind(sim_4, "delta" = 0.7)


sim_n <- rbind(sim_1, sim_2, sim_3, sim_4)

save(sim_n, file = "simulazione_numerosita.RData")

names(sim_n)[3] <- "Time_Kmodes"
colonne_tempo <- names(sim_n)[str_detect(names(sim_n), "^Time_")]
sim_n <- sim_n %>%
  mutate(across(all_of(colonne_tempo), ~as.numeric(str_replace(.x, "_secs", ""))))

ris_long <- sim_n %>%
  pivot_longer(
    cols = -c(n,delta),
    names_to = c(".value", "Metodo"), 
    names_pattern = "(ARI|NMI|Time)_(.+)" 
  ) %>%
  mutate(Metodo = str_to_upper(Metodo))


summary_sim_n <- ris_long %>%
  group_by(n, Metodo, delta) %>%
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
summary_sim_n

digits_colonne <- c(0, 0, 0, 2, 6, 6, 6, 6, 6, 6, 6)
tabella_sim <- xtable(summary_sim_n, digits = digits_colonne)
print(tabella_sim, include.rownames = FALSE, type = "latex")

summary_stdev <- summary_sim_n %>%
  group_by(n, Metodo) %>%
  filter(delta == 0.7) %>%
  summarise(ARI_DevStandard_Media = mean(ARI_DevStandard, na.rm = T),
            NMI_DevStandard_Media = mean(NMI_DevStandard, na.rm = T))


digits_colonne <- c(0,0, 0, 4, 4)
tabella_stdev <- xtable(summary_stdev, digits = digits_colonne)
print(tabella_stdev, include.rownames = FALSE, type = "latex")



# Tempi computazionali  ---------------------------------------------------


delta_0.25 <- summary_sim_n%>%
  filter(delta == 0.25)

plot_0.25 <- ggplot(delta_0.25, aes(x = reorder(Metodo, Tempo_Medio_sec), y = Tempo_Medio_sec, fill = Metodo)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  facet_wrap(~n, scales = "free_x", labeller = labeller(n = function(val) paste("n =", val))) + 
  scale_y_log10()+
  labs(
    title = "",
    x = "Metodo",
    y = "Tempo Medio (Secondi in scala log)"
  ) +
  theme_minimal(base_size = 12) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1), 
        plot.title = element_text(hjust = 0.5), 
        strip.background = element_rect(fill="lightblue", color="black"), 
        strip.text = element_text(face="bold")) +
  scale_fill_brewer(palette = "Paired")
print(plot_0.25)


delta_0.4 <- summary_sim_n%>%
  filter(delta == 0.4)

plot_0.4 <- ggplot(delta_0.4, aes(x = reorder(Metodo, Tempo_Medio_sec), y = Tempo_Medio_sec, fill = Metodo)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  facet_wrap(~n, scales = "free_x", labeller = labeller(n = function(val) paste("n =", val))) + 
  scale_y_log10()+
  labs(
    title = "",
    x = "Metodo",
    y = "Tempo Medio (Secondi in scala log)"
  ) +
  theme_minimal(base_size = 12) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1), 
        plot.title = element_text(hjust = 0.5), 
        strip.background = element_rect(fill="lightblue", color="black"), 
        strip.text = element_text(face="bold")) +
  scale_fill_brewer(palette = "Paired")
print(plot_0.4)



delta_0.55 <- summary_sim_n%>%
  filter(delta == 0.55)

plot_0.55 <- ggplot(delta_0.55, aes(x = reorder(Metodo, Tempo_Medio_sec), y = Tempo_Medio_sec, fill = Metodo)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  facet_wrap(~n, scales = "free_x", labeller = labeller(n = function(val) paste("n =", val))) + 
  scale_y_log10()+
  labs(
    title = "",
    x = "Metodo",
    y = "Tempo Medio (Secondi in scala log)"
  ) +
  theme_minimal(base_size = 12) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1), 
        plot.title = element_text(hjust = 0.5), 
        strip.background = element_rect(fill="lightblue", color="black"), 
        strip.text = element_text(face="bold")) +
  scale_fill_brewer(palette = "Paired")
print(plot_0.55)



delta_0.7 <- summary_sim_n%>%
  filter(delta == 0.7)

plot_0.7 <- ggplot(delta_0.7, aes(x = reorder(Metodo, Tempo_Medio_sec), y = Tempo_Medio_sec, fill = Metodo)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  facet_wrap(~n, scales = "free_x", labeller = labeller(n = function(val) paste("n =", val))) + 
  scale_y_log10()+
  labs(
    title = "",
    x = "Metodo",
    y = "Tempo Medio (Secondi in scala log)"
  ) +
  theme_minimal(base_size = 12) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1), 
        plot.title = element_text(hjust = 0.5), 
        strip.background = element_rect(fill="lightblue", color="black"), 
        strip.text = element_text(face="bold")) +
  scale_fill_brewer(palette = "Paired")
print(plot_0.7)

ari_n100 <- ris_long %>%
  select(delta, ARI, n, Metodo) %>%
  filter(n == 100)


boxplot_ari_n100 <- ggplot(ari_n100, aes(x = Metodo, y = ARI, fill = Metodo))+
  geom_boxplot()+
  facet_wrap(~delta, scales = "free_x", 
             labeller = labeller(delta = function(val) paste("\u03B4 =", val)))+
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1), 
        plot.title = element_text(hjust = 0.5), 
        strip.background = element_rect(fill="lightblue", color="black"), 
        strip.text = element_text(face="bold"), legend.position = "none")
print(boxplot_ari_n100)


ari_n200 <- ris_long %>%
  select(delta, ARI, n, Metodo) %>%
  filter(n == 200)


boxplot_ari_n200 <- ggplot(ari_n200, aes(x = Metodo, y = ARI, fill = Metodo))+
  geom_boxplot()+
  facet_wrap(~delta, scales = "free_x", 
             labeller = labeller(delta = function(val) paste("\u03B4 =", val)))+
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1), 
        plot.title = element_text(hjust = 0.5), 
        strip.background = element_rect(fill="lightblue", color="black"), 
        strip.text = element_text(face="bold"), legend.position = "none")
print(boxplot_ari_n200)


ari_n400 <- ris_long %>%
  select(delta, ARI, n, Metodo) %>%
  filter(n == 400)


boxplot_ari_n400 <- ggplot(ari_n400, aes(x = Metodo, y = ARI, fill = Metodo))+
  geom_boxplot()+
  facet_wrap(~delta, scales = "free_x", 
             labeller = labeller(delta = function(val) paste("\u03B4 =", val)))+
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1), 
        plot.title = element_text(hjust = 0.5), 
        strip.background = element_rect(fill="lightblue", color="black"), 
        strip.text = element_text(face="bold"), legend.position = "none")
print(boxplot_ari_n400)



nmi_n100 <- ris_long %>%
  select(delta, NMI, n, Metodo) %>%
  filter(n == 100)


boxplot_nmin100 <- ggplot(nmi_n100, aes(x = Metodo, y = NMI, fill = Metodo))+
  geom_boxplot()+
  facet_wrap(~delta, scales = "free_x", 
             labeller = labeller(delta = function(val) paste("\u03B4 =", val)))+
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1), 
        plot.title = element_text(hjust = 0.5), 
        strip.background = element_rect(fill="lightblue", color="black"), 
        strip.text = element_text(face="bold"), legend.position = "none")
print(boxplot_nmin100)


nmi_n200 <- ris_long %>%
  select(delta, NMI, n, Metodo) %>%
  filter(n == 200)


boxplot_nmin200 <- ggplot(nmi_n200, aes(x = Metodo, y = NMI, fill = Metodo))+
  geom_boxplot()+
  facet_wrap(~delta, scales = "free_x", 
             labeller = labeller(delta = function(val) paste("\u03B4 =", val)))+
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1), 
        plot.title = element_text(hjust = 0.5), 
        strip.background = element_rect(fill="lightblue", color="black"), 
        strip.text = element_text(face="bold"), legend.position = "none")
print(boxplot_nmin200)



nmi_n400 <- ris_long %>%
  select(delta, NMI, n, Metodo) %>%
  filter(n == 400)


boxplot_nmin400 <- ggplot(nmi_n400, aes(x = Metodo, y = NMI, fill = Metodo))+
  geom_boxplot()+
  facet_wrap(~delta, scales = "free_x", 
             labeller = labeller(delta = function(val) paste("\u03B4 =", val)))+
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1), 
        plot.title = element_text(hjust = 0.5), 
        strip.background = element_rect(fill="lightblue", color="black"), 
        strip.text = element_text(face="bold"), legend.position = "none")
print(boxplot_nmin400)


# ARI al variare di delta -------------------------------------------------

ari_delta0.25<- ris_long %>%
  select(delta, ARI, n, Metodo) %>%
  filter(delta == 0.25)

boxplot_ari_0.25 <- ggplot(ari_delta0.25, aes(x = Metodo, y = ARI, fill = Metodo))+
  geom_boxplot()+
  facet_wrap(~n, scales = "free_x", 
             labeller = labeller(n = function(val) paste("n =", val)))+
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1), 
        plot.title = element_text(hjust = 0.5), 
        strip.background = element_rect(fill="lightblue", color="black"), 
        strip.text = element_text(face="bold"), legend.position = "none")


print(boxplot_ari_0.25)
ggsave(boxplot_ari_0.25, file = "boxplot_ari_0.25.pdf", width = 6, height = 8, units = "in")


ari_delta0.4<- ris_long %>%
  select(delta, ARI, n, Metodo) %>%
  filter(delta == 0.4)

boxplot_ari_0.4 <- ggplot(ari_delta0.4, aes(x = Metodo, y = ARI, fill = Metodo))+
  geom_boxplot()+
  facet_wrap(~n, scales = "free_x", 
             labeller = labeller(n = function(val) paste("n =", val)))+
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1), 
        plot.title = element_text(hjust = 0.5), 
        strip.background = element_rect(fill="lightblue", color="black"), 
        strip.text = element_text(face="bold"), legend.position = "none")
print(boxplot_ari_0.4)

ggsave(boxplot_ari_0.4, file = "boxplot_ari_0.4.pdf", 
       width = 6, height = 8, units = "in")


ari_delta0.55<- ris_long %>%
  select(delta, ARI, n, Metodo) %>%
  filter(delta == 0.55)

boxplot_ari_0.55 <- ggplot(ari_delta0.55, aes(x = Metodo, y = ARI, fill = Metodo))+
  geom_boxplot()+
  facet_wrap(~n, scales = "free_x", 
             labeller = labeller(n = function(val) paste("n =", val)))+
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1), 
        plot.title = element_text(hjust = 0.5), 
        strip.background = element_rect(fill="lightblue", color="black"), 
        strip.text = element_text(face="bold"), legend.position = "none")
print(boxplot_ari_0.55)
ggsave(boxplot_ari_0.55, file = "boxplot_ari_0.55.pdf", width = 6, height = 8, units = "in")


ari_delta0.7<- ris_long %>%
  select(delta, ARI, n, Metodo) %>%
  filter(delta == 0.7)

boxplot_ari_0.7 <- ggplot(ari_delta0.7, aes(x = Metodo, y = ARI, fill = Metodo))+
  geom_boxplot()+
  facet_wrap(~n, scales = "free_x", 
             labeller = labeller(n = function(val) paste("n =", val)))+
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1), 
        plot.title = element_text(hjust = 0.5), 
        strip.background = element_rect(fill="lightblue", color="black"), 
        strip.text = element_text(face="bold"), legend.position = "none")
print(boxplot_ari_0.7)
ggsave(boxplot_ari_0.7, file = "boxplot_ari_0.7.pdf", width = 6, height = 8, units = "in")


# NMI ---------------------------------------------------------------------


nmi_delta_0.25 <- ris_long %>%
  select(delta, NMI, n, Metodo) %>%
  filter(delta == 0.25)

boxplot_delta0.25_nmi <- ggplot(nmi_delta_0.25, aes(x = Metodo, y = NMI, fill = Metodo))+
  geom_boxplot()+
  facet_wrap(~n, scales = "free_x",
             labeller = labeller(n = function(val) paste("n =", val)))+
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1), 
        plot.title = element_text(hjust = 0.5), 
        strip.background = element_rect(fill="lightblue", color="black"), 
        strip.text = element_text(face="bold"), legend.position = "none")
print(boxplot_delta0.25_nmi)
ggsave(boxplot_delta0.25_nmi, file = "boxplot_delta0.25_nmi.pdf", 
       width = 6, height = 8, units = "in")


nmi_delta_0.4 <- ris_long %>%
  select(delta, NMI, n, Metodo) %>%
  filter(delta == 0.4)

boxplot_delta0.4_nmi <- ggplot(nmi_delta_0.4, aes(x = Metodo, y = NMI, fill = Metodo))+
  geom_boxplot()+
  facet_wrap(~n, scales = "free_x",
             labeller = labeller(n = function(val) paste("n =", val)))+
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1), 
        plot.title = element_text(hjust = 0.5), 
        strip.background = element_rect(fill="lightblue", color="black"), 
        strip.text = element_text(face="bold"), legend.position = "none")
print(boxplot_delta0.4_nmi)
ggsave(boxplot_delta0.4_nmi, file = "boxplot_delta0.4_nmi.pdf", 
       width = 6, height = 8, units = "in")


nmi_delta_0.55 <- ris_long %>%
  select(delta, NMI, n, Metodo) %>%
  filter(delta == 0.55)

boxplot_delta0.55_nmi <- ggplot(nmi_delta_0.55, aes(x = Metodo, y = NMI, fill = Metodo))+
  geom_boxplot()+
  facet_wrap(~n, scales = "free_x",
             labeller = labeller(n = function(val) paste("n =", val)))+
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1), 
        plot.title = element_text(hjust = 0.5), 
        strip.background = element_rect(fill="lightblue", color="black"), 
        strip.text = element_text(face="bold"), legend.position = "none")
print(boxplot_delta0.55_nmi)
ggsave(boxplot_delta0.55_nmi, file = "boxplot_delta0.55_nmi.pdf", 
       width = 6, height = 8, units = "in")


nmi_delta_0.7 <- ris_long %>%
  select(delta, NMI, n, Metodo) %>%
  filter(delta == 0.7)

boxplot_delta0.7_nmi <- ggplot(nmi_delta_0.7, aes(x = Metodo, y = NMI, fill = Metodo))+
  geom_boxplot()+
  facet_wrap(~n, scales = "free_x",
             labeller = labeller(n = function(val) paste("n =", val)))+
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1), 
        plot.title = element_text(hjust = 0.5), 
        strip.background = element_rect(fill="lightblue", color="black"), 
        strip.text = element_text(face="bold"), legend.position = "none")
print(boxplot_delta0.7_nmi)
ggsave(boxplot_delta0.7_nmi, file = "boxplot_delta0.7_nmi.pdf", 
       width = 6, height = 8, units = "in")

