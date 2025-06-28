library(tidyverse)
library(xtable)

#.1.2.3 indica il cambio di delta
#2.,3., 4. indica il cambio di n
setwd("G:/My Drive/Università/Magistrale/Tesi/Studio di simulazione/Risultati/Label Switch")
load("ls_1.1.RData")
ls_1.1 <- df_results
load("ls_1.2.RData")
ls_1.2 <- df_results
load("ls_1.3.RData")
ls_1.3 <- df_results
load("ls_2.1.RData")
ls_2.1 <- df_results
load("ls_2.2.RData")
ls_2.2 <- df_results
load("ls_2.3.RData")
ls_2.3 <- df_results
load("ls_3.1.RData")
ls_3.1 <- df_results
load("ls_3.2.RData")
ls_3.2 <- df_results
load("ls_3.3.RData")
ls_3.3 <- df_results
load("ls_4.1.RData")
ls_4.1 <- df_results
load("ls_4.2.RData")
ls_4.2 <- df_results
load("ls_4.3.RData")
ls_4.3 <- df_results
load("ls_5.1.RData")
ls_5.1 <- df_results # delta = 0.7 e n = 800
load("ls_5.2.RData") 
ls_5.2 <- df_results # delta = 0.7 e n = 400
load("ls_5.3.RData")
ls_5.3 <- df_results # delta = 0.7 e n = 200
load("ls_5.4.RData")
ls_5.4 <- df_results # delta = 0.7 e n = 100

ls1. <- rbind(cbind(ls_1.1, "delta" = 0.25), cbind(ls_1.2, "delta" = 0.4),
              cbind(ls_1.3, "delta" = 0.55), cbind(ls_5.4, "delta" = 0.7))
ls1 <- cbind("n" = 100, ls1.)
ls1

ls2. <- rbind(cbind(ls_2.1, "delta" = 0.25), cbind(ls_2.2, "delta" = 0.4), 
              cbind(ls_2.3, "delta" = 0.55), cbind(ls_5.3, "delta" = 0.7))
ls2 <- cbind("n" = 200, ls2.)
ls2

ls3. <- rbind(cbind(ls_3.1, "delta" = 0.25), cbind(ls_3.2, "delta" = 0.4),
              cbind(ls_3.3, "delta" = 0.55), cbind(ls_5.2, "delta" = 0.7))
ls3 <- cbind("n" = 400, ls3.)
ls3

ls4. <- rbind(cbind(ls_4.1, "delta" = 0.25), cbind(ls_4.2, "delta" = 0.4), 
              cbind(ls_4.3, "delta" = 0.55), cbind(ls_5.1, "delta" = 0.7))
ls4 <- cbind("n" = 800, ls4.)
ls4


ris <- rbind(ls1, ls2, ls3, ls4)
summary(ris)
colonne_tempo <- names(ris)[str_detect(names(ris), "^Time_")]
names(ris)[3] <- "NMI_stephens"


ris <- ris %>%
  mutate(across(all_of(colonne_tempo), ~as.numeric(str_replace(.x, "_secs", ""))))
# save(ris, file = "risultati_ls.RData")

ris_long <- ris %>%
  pivot_longer(
    cols = -c(n,delta),
    names_to = c(".value", "Metodo"), 
    names_pattern = "(ARI|NMI|Time)_(.+)" 
  ) %>%
  mutate(Metodo = str_to_upper(Metodo))

summary_sim1 <- ris_long %>%
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
summary_sim1





digits_colonne <- c(0,
                    0,  # Per la colonna 'n' (nessun decimale)
                    0,  #Per delta
                    2,  # Per 'Metodo' (carattere, i decimali sono ignorati ma serve un placeholder)
                    6,  # Per ARI_Media
                    6,  # Per ARI_Mediana
                    6,  # Per ARI_DevStandard
                    6,  # Per NMI_Media
                    6,  # Per NMI_Mediana
                    6,  # Per NMI_DevStandard
                    6)  # Per Tempo_Medio_sec
tabella_sim1 <- xtable(summary_sim1, digits = digits_colonne)
print(tabella_sim1, include.rownames = FALSE, type = "latex")


delta_0.25 <- summary_sim1%>%
  filter(delta == 0.25)

plot_0.25 <- ggplot(delta_0.25, aes(x = reorder(Metodo, Tempo_Medio_sec), y = Tempo_Medio_sec, fill = Metodo)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  facet_wrap(~n, scales = "free_x", labeller = labeller(n = function(val) paste("n =", val))) + 
  labs(
    title = "",
    x = "Metodo",
    y = "Tempo Medio (Secondi)"
  ) +
  theme_minimal(base_size = 12) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1), 
        plot.title = element_text(hjust = 0.5), 
        strip.background = element_rect(fill="lightblue", color="black"), 
        strip.text = element_text(face="bold")) +
  scale_fill_brewer(palette = "Paired")
print(plot_0.25)


delta_0.4 <- summary_sim1%>%
  filter(delta == 0.4)

plot_0.4 <- ggplot(delta_0.4, aes(x = reorder(Metodo, Tempo_Medio_sec), y = Tempo_Medio_sec, fill = Metodo)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  facet_wrap(~n, scales = "free_x", labeller = labeller(n = function(val) paste("n =", val))) + 
  labs(
    title = "",
    x = "Metodo",
    y = "Tempo Medio (Secondi)"
  ) +
  theme_minimal(base_size = 12) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1), 
        plot.title = element_text(hjust = 0.5), 
        strip.background = element_rect(fill="lightblue", color="black"), 
        strip.text = element_text(face="bold")) +
  scale_fill_brewer(palette = "Paired")
print(plot_0.4)


delta_0.55 <- summary_sim1%>%
  filter(delta == 0.55)

plot_0.55 <- ggplot(delta_0.55, aes(x = reorder(Metodo, Tempo_Medio_sec), y = Tempo_Medio_sec, fill = Metodo)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  facet_wrap(~n, scales = "free_x", labeller = labeller(n = function(val) paste("n =", val))) + 
  labs(
    title = "",
    x = "Metodo",
    y = "Tempo Medio (Secondi)"
  ) +
  theme_minimal(base_size = 12) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1), 
        plot.title = element_text(hjust = 0.5), 
        strip.background = element_rect(fill="lightblue", color="black"), 
        strip.text = element_text(face="bold")) +
  scale_fill_brewer(palette = "Paired")
print(plot_0.55)




delta_0.7 <- summary_sim1%>%
  filter(delta == 0.7)

plot_0.7 <- ggplot(delta_0.7, aes(x = reorder(Metodo, Tempo_Medio_sec), y = Tempo_Medio_sec, fill = Metodo)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  facet_wrap(~n, scales = "free_x", labeller = labeller(n = function(val) paste("n =", val))) + 
  labs(
    title = "",
    x = "Metodo",
    y = "Tempo Medio (Secondi)"
  ) +
  theme_minimal(base_size = 15) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1), 
        plot.title = element_text(hjust = 0.5), 
        strip.background = element_rect(fill="lightblue", color="black"), 
        strip.text = element_text(face="bold")) +
  scale_fill_brewer(palette = "Paired")
print(plot_0.7)




n_100 <- summary_sim1 %>%
  filter(n == 100)


plot_n100 <- ggplot(n_100, aes(x = reorder(Metodo, Tempo_Medio_sec), y = Tempo_Medio_sec, fill = Metodo)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  facet_wrap(~delta, scales = "free_x", labeller = labeller(delta = function(val) paste("\u03B4 =", val))) + 
  labs(
    title = "",
    x = "Metodo",
    y = "Tempo Medio (Secondi)"
  ) +
  theme_minimal(base_size = 12) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1), 
        plot.title = element_text(hjust = 0.5), 
        strip.background = element_rect(fill="lightblue", color="black"), 
        strip.text = element_text(face="bold")) +
  scale_fill_brewer(palette = "Paired")
print(plot_n100)


n_200 <- summary_sim1 %>%
  filter(n == 200)


plot_n200 <- ggplot(n_200, aes(x = reorder(Metodo, Tempo_Medio_sec), y = Tempo_Medio_sec, fill = Metodo)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  facet_wrap(~delta, scales = "free_x", labeller = labeller(delta = function(val) paste("\u03B4 =", val))) + 
  labs(
    title = "",
    x = "Metodo",
    y = "Tempo Medio (Secondi)"
  ) +
  theme_minimal(base_size = 12) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1), 
        plot.title = element_text(hjust = 0.5), 
        strip.background = element_rect(fill="lightblue", color="black"), 
        strip.text = element_text(face="bold")) +
  scale_fill_brewer(palette = "Paired")
print(plot_n200)

n_400 <- summary_sim1 %>%
  filter(n == 400)


plot_n400 <- ggplot(n_400, aes(x = reorder(Metodo, Tempo_Medio_sec), y = Tempo_Medio_sec, fill = Metodo)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  facet_wrap(~delta, scales = "free_x", labeller = labeller(delta = function(val) paste("\u03B4 =", val))) + 
  labs(
    title = "",
    x = "Metodo",
    y = "Tempo Medio (Secondi)"
  ) +
  theme_minimal(base_size = 12) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1), 
        plot.title = element_text(hjust = 0.5), 
        strip.background = element_rect(fill="lightblue", color="black"), 
        strip.text = element_text(face="bold")) +
  scale_fill_brewer(palette = "Paired")
print(plot_n400)


n_800 <- summary_sim1 %>%
  filter(n == 800)


plot_n800 <- ggplot(n_800, aes(x = reorder(Metodo, Tempo_Medio_sec), y = Tempo_Medio_sec, fill = Metodo)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  facet_wrap(~delta, scales = "free_x", labeller = labeller(delta = function(val) paste("\u03B4 =", val))) + 
  labs(
    title = "",
    x = "Metodo",
    y = "Tempo Medio (Secondi)"
  ) +
  theme_minimal(base_size = 12) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1), 
        plot.title = element_text(hjust = 0.5), 
        strip.background = element_rect(fill="lightblue", color="black"), 
        strip.text = element_text(face="bold")) +
  scale_fill_brewer(palette = "Paired")
print(plot_n800)


#Ora osservo i valori di ARI e NMI

ari_n100 <- ris_long %>%
  select(delta, ARI, n, Metodo) %>%
  filter(n == 100)


boxplot_ari <- ggplot(ari_n100, aes(x = Metodo, y = ARI, fill = Metodo))+
  geom_boxplot()+
  facet_wrap(~delta, scales = "free_x", 
             labeller = labeller(delta = function(val) paste("\u03B4 =", val)))+
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1), 
                         plot.title = element_text(hjust = 0.5), 
                         strip.background = element_rect(fill="lightblue", color="black"), 
                         strip.text = element_text(face="bold"), legend.position = "none")

print(boxplot_ari)


ari_n200 <- ris_long %>%
  select(delta, ARI, n, Metodo) %>%
  filter(n == 200)


boxplot_ari <- ggplot(ari_n200, aes(x = Metodo, y = ARI, fill = Metodo))+
  geom_boxplot()+
  facet_wrap(~delta, scales = "free_x", 
             labeller = labeller(delta = function(val) paste("\u03B4 =", val)))+
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1), 
        plot.title = element_text(hjust = 0.5), 
        strip.background = element_rect(fill="lightblue", color="black"), 
        strip.text = element_text(face="bold"), legend.position = "none")

print(boxplot_ari)


ari_n400 <- ris_long %>%
  select(delta, ARI, n, Metodo) %>%
  filter(n == 400)


boxplot_ari <- ggplot(ari_n400, aes(x = Metodo, y = ARI, fill = Metodo))+
  geom_boxplot()+
  facet_wrap(~delta, scales = "free_x", 
             labeller = labeller(delta = function(val) paste("\u03B4 =", val)))+
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1), 
        plot.title = element_text(hjust = 0.5), 
        strip.background = element_rect(fill="lightblue", color="black"), 
        strip.text = element_text(face="bold"), legend.position = "none")

print(boxplot_ari)

ari_n800 <- ris_long %>%
  select(delta, ARI, n, Metodo) %>%
  filter(n == 800)


boxplot_ari <- ggplot(ari_n800, aes(x = Metodo, y = ARI, fill = Metodo))+
  geom_boxplot()+
  facet_wrap(~delta, scales = "free_x", 
             labeller = labeller(delta = function(val) paste("\u03B4 =", val)))+
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1), 
        plot.title = element_text(hjust = 0.5), 
        strip.background = element_rect(fill="lightblue", color="black"), 
        strip.text = element_text(face="bold"), legend.position = "none")

print(boxplot_ari)

nmi_n100 <- ris_long %>%
  select(delta, NMI, n, Metodo) %>%
  filter(n == 100)


boxplot_nmi100 <- ggplot(nmi_n100, aes(x = Metodo, y = NMI, fill = Metodo))+
  geom_boxplot(show.legend = F)+
  facet_wrap(~delta, scales = "free_x", 
             labeller = labeller(delta = function(val) paste("\u03B4 =", val)))+
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1), 
        plot.title = element_text(hjust = 0.5), 
        strip.background = element_rect(fill="lightblue", color="black"), 
        strip.text = element_text(face="bold"), legend.position = "none")

print(boxplot_nmi100)


nmi_n200 <- ris_long %>%
  select(delta, NMI, n, Metodo) %>%
  filter(n == 200)


boxplot_nmi <- ggplot(nmi_n200, aes(x = Metodo, y = NMI, fill = Metodo))+
  geom_boxplot()+
  facet_wrap(~delta, scales = "free_x", 
             labeller = labeller(delta = function(val) paste("\u03B4 =", val)))+
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1), 
        plot.title = element_text(hjust = 0.5), 
        strip.background = element_rect(fill="lightblue", color="black"), 
        strip.text = element_text(face="bold"), legend.position = "none")

print(boxplot_nmi)


nmi_n400 <- ris_long %>%
  select(delta, NMI, n, Metodo) %>%
  filter(n == 400)


boxplot_nmi <- ggplot(nmi_n400, aes(x = Metodo, y = NMI, fill = Metodo))+
  geom_boxplot()+
  facet_wrap(~delta, scales = "free_x", 
             labeller = labeller(delta = function(val) paste("\u03B4 =", val)))+
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1), 
        plot.title = element_text(hjust = 0.5), 
        strip.background = element_rect(fill="lightblue", color="black"), 
        strip.text = element_text(face="bold"), legend.position = "none")

print(boxplot_nmi)

nmi_n800 <- ris_long %>%
  select(delta, NMI, n, Metodo) %>%
  filter(n == 800)


boxplot_nmi <- ggplot(nmi_n800, aes(x = Metodo, y = NMI, fill = Metodo))+
  geom_boxplot()+
  facet_wrap(~delta, scales = "free_x", 
             labeller = labeller(delta = function(val) paste("\u03B4 =", val)))+
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1), 
        plot.title = element_text(hjust = 0.5), 
        strip.background = element_rect(fill="lightblue", color="black"), 
        strip.text = element_text(face="bold"))

print(boxplot_nmi)



#Osservo al variare di n

ari_delta25 <- ris_long %>%
  select(delta, ARI, n, Metodo) %>%
  filter(delta == 0.25)


boxplot_ari <- ggplot(ari_delta25, aes(x = Metodo, y = ARI, fill = Metodo))+
  geom_boxplot()+
  facet_wrap(~n, scales = "free_x", 
             labeller = labeller(n = function(val) paste("n =", val)))+
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1), 
        plot.title = element_text(hjust = 0.5), 
        strip.background = element_rect(fill="lightblue", color="black"), 
        strip.text = element_text(face="bold"), legend.position = "none")

print(boxplot_ari)

ari_delta04 <- ris_long %>%
  select(delta, ARI, n, Metodo) %>%
  filter(delta == 0.4)


boxplot_ari <- ggplot(ari_delta04, aes(x = Metodo, y = ARI, fill = Metodo))+
  geom_boxplot()+
  facet_wrap(~n, scales = "free_x", 
             labeller = labeller(n = function(val) paste("n =", val)))+
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1), 
        plot.title = element_text(hjust = 0.5), 
        strip.background = element_rect(fill="lightblue", color="black"), 
        strip.text = element_text(face="bold"), legend.position = "none")

print(boxplot_ari)

ari_delta55 <- ris_long %>%
  select(delta, ARI, n, Metodo) %>%
  filter(delta == 0.55)


boxplot_ari <- ggplot(ari_delta55, aes(x = Metodo, y = ARI, fill = Metodo))+
  geom_boxplot()+
  facet_wrap(~n, scales = "free_x", 
             labeller = labeller(n = function(val) paste("n =", val)))+
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1), 
        plot.title = element_text(hjust = 0.5), 
        strip.background = element_rect(fill="lightblue", color="black"), 
        strip.text = element_text(face="bold"), legend.position = "none")

print(boxplot_ari)


ari_delta7 <- ris_long %>%
  select(delta, ARI, n, Metodo) %>%
  filter(delta == 0.7)


boxplot_ari <- ggplot(ari_delta7, aes(x = Metodo, y = ARI, fill = Metodo))+
  geom_boxplot()+
  facet_wrap(~n, scales = "free_x", 
             labeller = labeller(n = function(val) paste("n =", val)))+
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1), 
        plot.title = element_text(hjust = 0.5), 
        strip.background = element_rect(fill="lightblue", color="black"), 
        strip.text = element_text(face="bold"), legend.position = "none")

print(boxplot_ari)


#NMI



nmi_delta25 <- ris_long %>%
  select(delta, NMI, n, Metodo) %>%
  filter(delta == 0.25)


boxplot_ari <- ggplot(nmi_delta25, aes(x = Metodo, y = NMI, fill = Metodo))+
  geom_boxplot()+
  facet_wrap(~n, scales = "free_x", 
             labeller = labeller(n = function(val) paste("n =", val)))+
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1), 
        plot.title = element_text(hjust = 0.5), 
        strip.background = element_rect(fill="lightblue", color="black"), 
        strip.text = element_text(face="bold"), legend.position = "none")

print(boxplot_ari)

nmi_delta04 <- ris_long %>%
  select(delta, NMI, n, Metodo) %>%
  filter(delta == 0.4)


boxplot_ari <- ggplot(nmi_delta04, aes(x = Metodo, y = NMI, fill = Metodo))+
  geom_boxplot()+
  facet_wrap(~n, scales = "free_x", 
             labeller = labeller(n = function(val) paste("n =", val)))+
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1), 
        plot.title = element_text(hjust = 0.5), 
        strip.background = element_rect(fill="lightblue", color="black"), 
        strip.text = element_text(face="bold"), legend.position = "none")

print(boxplot_ari)

nmi_delta55 <- ris_long %>%
  select(delta, NMI, n, Metodo) %>%
  filter(delta == 0.55)


boxplot_ari <- ggplot(nmi_delta55, aes(x = Metodo, y = NMI, fill = Metodo))+
  geom_boxplot()+
  facet_wrap(~n, scales = "free_x", 
             labeller = labeller(n = function(val) paste("n =", val)))+
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1), 
        plot.title = element_text(hjust = 0.5), 
        strip.background = element_rect(fill="lightblue", color="black"), 
        strip.text = element_text(face="bold"), legend.position = "none")

print(boxplot_ari)


nmi_delta7 <- ris_long %>%
  select(delta, NMI, n, Metodo) %>%
  filter(delta == 0.7)


boxplot_ari <- ggplot(nmi_delta7, aes(x = Metodo, y = NMI, fill = Metodo))+
  geom_boxplot()+
  facet_wrap(~n, scales = "free_x", 
             labeller = labeller(n = function(val) paste("n =", val)))+
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1), 
        plot.title = element_text(hjust = 0.5), 
        strip.background = element_rect(fill="lightblue", color="black"), 
        strip.text = element_text(face="bold"), legend.position = "none")

print(boxplot_ari)





