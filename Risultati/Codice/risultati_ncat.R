rm(list=ls());graphics.off()
setwd("G:/My Drive/Università/Magistrale/Tesi/Studio di simulazione/Risultati/n_categorie")
library(tidyverse)
library(xtable)


load("n_cat1.1.RData") # n = 100,tau = rep(1/3, 3),delta = 0.25, m = c(10,15,13,11,4,6)
load("n_cat1.2.RData") # n = 100,tau = rep(1/3, 3),delta = 0.4, m = c(10,15,13,11,4,6)
load("n_cat1.3.RData") # n = 100,tau = rep(1/3, 3),delta = 0.55, m = c(10,15,13,11,4,6)
load("n_cat1.4.RData") # n = 100,tau = rep(1/3, 3),delta = 0.4, m = c(10,15,13,11,4,6)

n_cat_1 <- rbind(cbind(simulazione_1.1, "delta" = 0.25), 
                 cbind(simulazione_1.2, "delta" = 0.4),
                 cbind(simulazione_1.3, "delta" = 0.55),
                 cbind(simulazione_1.4, "delta" = 0.7))

load("n_cat2.1.RData") # n = 200,tau = rep(1/3, 3),delta = 0.25, m = c(10,15,13,11,4,6)
load("n_cat2.2.RData") # n = 200,tau = rep(1/3, 3),delta = 0.4, m = c(10,15,13,11,4,6)
load("n_cat2.3.RData") # n = 200,tau = rep(1/3, 3),delta = 0.55, m = c(10,15,13,11,4,6)
load("n_cat2.4.RData") # n = 200,tau = rep(1/3, 3),delta = 0.4, m = c(10,15,13,11,4,6)

n_cat_2 <- rbind(cbind(simulazione_2.1, "delta" = 0.25), 
                 cbind(simulazione_2.2, "delta" = 0.4),
                 cbind(simulazione_2.3, "delta" = 0.55),
                 cbind(simulazione_2.4, "delta" = 0.7))

load("n_cat3.1.RData") # n = 400,tau = rep(1/3, 3),delta = 0.25, m = c(10,15,13,11,4,6)
load("n_cat3.2.RData") # n = 400,tau = rep(1/3, 3),delta = 0.4, m = c(10,15,13,11,4,6)
load("n_cat3.3.RData") # n = 400,tau = rep(1/3, 3),delta = 0.55, m = c(10,15,13,11,4,6)
load("n_cat3.4.RData") # n = 400,tau = rep(1/3, 3),delta = 0.7, m = c(10,15,13,11,4,6)

n_cat_3 <- rbind(cbind(simulazione_3.1, "delta" = 0.25), 
                 cbind(simulazione_3.2, "delta" = 0.4),
                 cbind(simulazione_3.3, "delta" = 0.55),
                 cbind(simulazione_3.4, "delta" = 0.7))



n_cat <- rbind(cbind(n_cat_1, "n" = 100),
               cbind(n_cat_2, "n" = 200),
               cbind(n_cat_3, "n" = 400))

load("G:/My Drive/Università/Magistrale/Tesi/Studio di simulazione/Risultati/Numerosità/simulazione_numerosita.RData")

sim <- rbind(cbind(sim_n, "m" = "(4,4,4,3,3,3)"),
             cbind(n_cat, "m" = "(10,15,13,11,4,6)"))


names(sim)[3] <-"Time_Kmodes"

colonne_tempo <- names(sim)[str_detect(names(sim), "^Time_")]
sim <- sim %>%
  mutate(across(all_of(colonne_tempo), ~as.numeric(str_replace(.x, "_secs", ""))))

ris_long <- sim %>%
  pivot_longer(
    cols = -c(n,delta, m),
    names_to = c(".value", "Metodo"), 
    names_pattern = "(ARI|NMI|Time)_(.+)" 
  ) %>%
  mutate(Metodo = str_to_upper(Metodo))


summary_mix <- ris_long %>%
  group_by(n, Metodo, delta, m) %>%
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

# Analisi grafica ---------------------------------------------------------

ris_long$m <- as.factor(ris_long$m)
levels(ris_long$m) <- c("m = (10,15,13,11,4,6)", "m = (4,4,4,3,3,3)")

setwd("G:/My Drive/Università/Magistrale/Tesi/Studio di simulazione/Grafici/n_cat")

# ARI ---------------------------------------------------------------------

# delta = 0.25

ris_delta_.25 <- ris_long %>%
  select(m,delta, n, ARI, Metodo)%>%
  filter(delta == 0.25, n == 100) 

boxplot_delta.25 <- ggplot(ris_delta_.25, aes(x = Metodo, y = ARI, fill = Metodo))+
  geom_boxplot()+
  facet_wrap(~ m, scales = "free_x")+
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1), 
        plot.title = element_text(hjust = 0.5), 
        strip.background = element_rect(fill="lightblue", color="black"), 
        strip.text = element_text(face="bold"), legend.position = "none")
print(boxplot_delta.25)
ggsave(boxplot_delta.25, file = "boxplot_delta.25_n100.pdf",
       width = 10, height = 6, units = "in")


ris_delta_.4 <- ris_long %>%
  select(m,delta, n, ARI, Metodo)%>%
  filter(delta == 0.4, n == 100) 

boxplot_delta.4 <- ggplot(ris_delta_.4, aes(x = Metodo, y = ARI, fill = Metodo))+
  geom_boxplot()+
  facet_wrap(~ m, scales = "free_x")+
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1), 
        plot.title = element_text(hjust = 0.5), 
        strip.background = element_rect(fill="lightblue", color="black"), 
        strip.text = element_text(face="bold"), legend.position = "none")
print(boxplot_delta.4)
ggsave(boxplot_delta.4, file = "boxplot_delta.4_n100.pdf",
       width = 10, height = 6, units = "in")

ris_delta_.55 <- ris_long %>%
  select(m,delta, n, ARI, Metodo)%>%
  filter(delta == 0.55, n == 100) 

boxplot_delta.55 <- ggplot(ris_delta_.55, aes(x = Metodo, y = ARI, fill = Metodo))+
  geom_boxplot()+
  facet_wrap(~ m, scales = "free_x")+
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1), 
        plot.title = element_text(hjust = 0.5), 
        strip.background = element_rect(fill="lightblue", color="black"), 
        strip.text = element_text(face="bold"), legend.position = "none")
print(boxplot_delta.55)
ggsave(boxplot_delta.55, file = "boxplot_delta.55_n100.pdf",
       width = 10, height = 6, units = "in")

ris_delta_.7 <- ris_long %>%
  select(m,delta, n, ARI, Metodo)%>%
  filter(delta == 0.7, n == 100) 

boxplot_delta.7 <- ggplot(ris_delta_.7, aes(x = Metodo, y = ARI, fill = Metodo))+
  geom_boxplot()+
  facet_wrap(~ m, scales = "free_x")+
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1), 
        plot.title = element_text(hjust = 0.5), 
        strip.background = element_rect(fill="lightblue", color="black"), 
        strip.text = element_text(face="bold"), legend.position = "none")
print(boxplot_delta.7)
ggsave(boxplot_delta.7, file = "boxplot_delta.7_n100.pdf",
       width = 10, height = 6, units = "in")


#n = 200

ris_delta_.25 <- ris_long %>%
  select(m,delta, n, ARI, Metodo)%>%
  filter(delta == 0.25, n == 200) 

boxplot_delta.25 <- ggplot(ris_delta_.25, aes(x = Metodo, y = ARI, fill = Metodo))+
  geom_boxplot()+
  facet_wrap(~ m, scales = "free_x")+
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1), 
        plot.title = element_text(hjust = 0.5), 
        strip.background = element_rect(fill="lightblue", color="black"), 
        strip.text = element_text(face="bold"), legend.position = "none")
print(boxplot_delta.25)
ggsave(boxplot_delta.25, file = "boxplot_delta.25_n200.pdf",
       width = 10, height = 6, units = "in")


ris_delta_.4 <- ris_long %>%
  select(m,delta, n, ARI, Metodo)%>%
  filter(delta == 0.4, n == 200) 

boxplot_delta.4 <- ggplot(ris_delta_.4, aes(x = Metodo, y = ARI, fill = Metodo))+
  geom_boxplot()+
  facet_wrap(~ m, scales = "free_x")+
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1), 
        plot.title = element_text(hjust = 0.5), 
        strip.background = element_rect(fill="lightblue", color="black"), 
        strip.text = element_text(face="bold"), legend.position = "none")
print(boxplot_delta.4)
ggsave(boxplot_delta.4, file = "boxplot_delta.4_n200.pdf",
       width = 10, height = 6, units = "in")

ris_delta_.55 <- ris_long %>%
  select(m,delta, n, ARI, Metodo)%>%
  filter(delta == 0.55, n == 200) 

boxplot_delta.55 <- ggplot(ris_delta_.55, aes(x = Metodo, y = ARI, fill = Metodo))+
  geom_boxplot()+
  facet_wrap(~ m, scales = "free_x")+
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1), 
        plot.title = element_text(hjust = 0.5), 
        strip.background = element_rect(fill="lightblue", color="black"), 
        strip.text = element_text(face="bold"), legend.position = "none")
print(boxplot_delta.55)
ggsave(boxplot_delta.55, file = "boxplot_delta.55_n200.pdf",
       width = 10, height = 6, units = "in")

ris_delta_.7 <- ris_long %>%
  select(m,delta, n, ARI, Metodo)%>%
  filter(delta == 0.7, n == 200) 

boxplot_delta.7 <- ggplot(ris_delta_.7, aes(x = Metodo, y = ARI, fill = Metodo))+
  geom_boxplot()+
  facet_wrap(~ m, scales = "free_x")+
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1), 
        plot.title = element_text(hjust = 0.5), 
        strip.background = element_rect(fill="lightblue", color="black"), 
        strip.text = element_text(face="bold"), legend.position = "none")
print(boxplot_delta.7)
ggsave(boxplot_delta.7, file = "boxplot_delta.7_n200.pdf",
       width = 10, height = 6, units = "in")


# n = 400


ris_delta_.25 <- ris_long %>%
  select(m,delta, n, ARI, Metodo)%>%
  filter(delta == 0.25, n == 400) 

boxplot_delta.25 <- ggplot(ris_delta_.25, aes(x = Metodo, y = ARI, fill = Metodo))+
  geom_boxplot()+
  facet_wrap(~ m, scales = "free_x")+
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1), 
        plot.title = element_text(hjust = 0.5), 
        strip.background = element_rect(fill="lightblue", color="black"), 
        strip.text = element_text(face="bold"), legend.position = "none")
print(boxplot_delta.25)
ggsave(boxplot_delta.25, file = "boxplot_delta.25_n400.pdf",
       width = 10, height = 6, units = "in")


ris_delta_.4 <- ris_long %>%
  select(m,delta, n, ARI, Metodo)%>%
  filter(delta == 0.4, n == 400) 

boxplot_delta.4 <- ggplot(ris_delta_.4, aes(x = Metodo, y = ARI, fill = Metodo))+
  geom_boxplot()+
  facet_wrap(~ m, scales = "free_x")+
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1), 
        plot.title = element_text(hjust = 0.5), 
        strip.background = element_rect(fill="lightblue", color="black"), 
        strip.text = element_text(face="bold"), legend.position = "none")
print(boxplot_delta.4)
ggsave(boxplot_delta.4, file = "boxplot_delta.4_n400.pdf",
       width = 10, height = 6, units = "in")

ris_delta_.55 <- ris_long %>%
  select(m,delta, n, ARI, Metodo)%>%
  filter(delta == 0.55, n == 400) 

boxplot_delta.55 <- ggplot(ris_delta_.55, aes(x = Metodo, y = ARI, fill = Metodo))+
  geom_boxplot()+
  facet_wrap(~ m, scales = "free_x")+
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1), 
        plot.title = element_text(hjust = 0.5), 
        strip.background = element_rect(fill="lightblue", color="black"), 
        strip.text = element_text(face="bold"), legend.position = "none")
print(boxplot_delta.55)
ggsave(boxplot_delta.55, file = "boxplot_delta.55_n400.pdf",
       width = 10, height = 6, units = "in")

ris_delta_.7 <- ris_long %>%
  select(m,delta, n, ARI, Metodo)%>%
  filter(delta == 0.7, n == 400) 

boxplot_delta.7 <- ggplot(ris_delta_.7, aes(x = Metodo, y = ARI, fill = Metodo))+
  geom_boxplot()+
  facet_wrap(~ m, scales = "free_x")+
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1), 
        plot.title = element_text(hjust = 0.5), 
        strip.background = element_rect(fill="lightblue", color="black"), 
        strip.text = element_text(face="bold"), legend.position = "none")
print(boxplot_delta.7)
ggsave(boxplot_delta.7, file = "boxplot_delta.7_n400.pdf",
       width = 10, height = 6, units = "in")

# NMI ---------------------------------------------------------------------

# delta = 0.25

ris_delta_.25 <- ris_long %>%
  select(m,delta, n, NMI, Metodo)%>%
  filter(delta == 0.25, n == 100) 

boxplot_delta.25 <- ggplot(ris_delta_.25, aes(x = Metodo, y = NMI, fill = Metodo))+
  geom_boxplot()+
  facet_wrap(~ m, scales = "free_x")+
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1), 
        plot.title = element_text(hjust = 0.5), 
        strip.background = element_rect(fill="lightblue", color="black"), 
        strip.text = element_text(face="bold"), legend.position = "none")
print(boxplot_delta.25)
ggsave(boxplot_delta.25, file = "nmi_delta.25_n100.pdf",
       width = 10, height = 6, units = "in")


ris_delta_.4 <- ris_long %>%
  select(m,delta, n, NMI, Metodo)%>%
  filter(delta == 0.4, n == 100) 

boxplot_delta.4 <- ggplot(ris_delta_.4, aes(x = Metodo, y = NMI, fill = Metodo))+
  geom_boxplot()+
  facet_wrap(~ m, scales = "free_x")+
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1), 
        plot.title = element_text(hjust = 0.5), 
        strip.background = element_rect(fill="lightblue", color="black"), 
        strip.text = element_text(face="bold"), legend.position = "none")
print(boxplot_delta.4)
ggsave(boxplot_delta.4, file = "nmi_delta.4_n100.pdf",
       width = 10, height = 6, units = "in")

ris_delta_.55 <- ris_long %>%
  select(m,delta, n, NMI, Metodo)%>%
  filter(delta == 0.55, n == 100) 

boxplot_delta.55 <- ggplot(ris_delta_.55, aes(x = Metodo, y = NMI, fill = Metodo))+
  geom_boxplot()+
  facet_wrap(~ m, scales = "free_x")+
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1), 
        plot.title = element_text(hjust = 0.5), 
        strip.background = element_rect(fill="lightblue", color="black"), 
        strip.text = element_text(face="bold"), legend.position = "none")
print(boxplot_delta.55)
ggsave(boxplot_delta.55, file = "nmi_delta.55_n100.pdf",
       width = 10, height = 6, units = "in")

ris_delta_.7 <- ris_long %>%
  select(m,delta, n, NMI, Metodo)%>%
  filter(delta == 0.7, n == 100) 

boxplot_delta.7 <- ggplot(ris_delta_.7, aes(x = Metodo, y = NMI, fill = Metodo))+
  geom_boxplot()+
  facet_wrap(~ m, scales = "free_x")+
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1), 
        plot.title = element_text(hjust = 0.5), 
        strip.background = element_rect(fill="lightblue", color="black"), 
        strip.text = element_text(face="bold"), legend.position = "none")
print(boxplot_delta.7)
ggsave(boxplot_delta.7, file = "nmi_delta.7_n100.pdf",
       width = 10, height = 6, units = "in")


#n = 200

ris_delta_.25 <- ris_long %>%
  select(m,delta, n, NMI, Metodo)%>%
  filter(delta == 0.25, n == 200) 

boxplot_delta.25 <- ggplot(ris_delta_.25, aes(x = Metodo, y = NMI, fill = Metodo))+
  geom_boxplot()+
  facet_wrap(~ m, scales = "free_x")+
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1), 
        plot.title = element_text(hjust = 0.5), 
        strip.background = element_rect(fill="lightblue", color="black"), 
        strip.text = element_text(face="bold"), legend.position = "none")
print(boxplot_delta.25)
ggsave(boxplot_delta.25, file = "nmi_delta.25_n200.pdf",
       width = 10, height = 6, units = "in")


ris_delta_.4 <- ris_long %>%
  select(m,delta, n, NMI, Metodo)%>%
  filter(delta == 0.4, n == 200) 

boxplot_delta.4 <- ggplot(ris_delta_.4, aes(x = Metodo, y = NMI, fill = Metodo))+
  geom_boxplot()+
  facet_wrap(~ m, scales = "free_x")+
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1), 
        plot.title = element_text(hjust = 0.5), 
        strip.background = element_rect(fill="lightblue", color="black"), 
        strip.text = element_text(face="bold"), legend.position = "none")
print(boxplot_delta.4)
ggsave(boxplot_delta.4, file = "nmi_delta.4_n200.pdf",
       width = 10, height = 6, units = "in")

ris_delta_.55 <- ris_long %>%
  select(m,delta, n, NMI, Metodo)%>%
  filter(delta == 0.55, n == 200) 

boxplot_delta.55 <- ggplot(ris_delta_.55, aes(x = Metodo, y = NMI, fill = Metodo))+
  geom_boxplot()+
  facet_wrap(~ m, scales = "free_x")+
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1), 
        plot.title = element_text(hjust = 0.5), 
        strip.background = element_rect(fill="lightblue", color="black"), 
        strip.text = element_text(face="bold"), legend.position = "none")
print(boxplot_delta.55)
ggsave(boxplot_delta.55, file = "nmi_delta.55_n200.pdf",
       width = 10, height = 6, units = "in")

ris_delta_.7 <- ris_long %>%
  select(m,delta, n, NMI, Metodo)%>%
  filter(delta == 0.7, n == 200) 

boxplot_delta.7 <- ggplot(ris_delta_.7, aes(x = Metodo, y = NMI, fill = Metodo))+
  geom_boxplot()+
  facet_wrap(~ m, scales = "free_x")+
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1), 
        plot.title = element_text(hjust = 0.5), 
        strip.background = element_rect(fill="lightblue", color="black"), 
        strip.text = element_text(face="bold"), legend.position = "none")
print(boxplot_delta.7)
ggsave(boxplot_delta.7, file = "nmi_delta.7_n200.pdf",
       width = 10, height = 6, units = "in")


# n = 400


ris_delta_.25 <- ris_long %>%
  select(m,delta, n, NMI, Metodo)%>%
  filter(delta == 0.25, n == 400) 

boxplot_delta.25 <- ggplot(ris_delta_.25, aes(x = Metodo, y = NMI, fill = Metodo))+
  geom_boxplot()+
  facet_wrap(~ m, scales = "free_x")+
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1), 
        plot.title = element_text(hjust = 0.5), 
        strip.background = element_rect(fill="lightblue", color="black"), 
        strip.text = element_text(face="bold"), legend.position = "none")
print(boxplot_delta.25)
ggsave(boxplot_delta.25, file = "nmi_delta.25_n400.pdf",
       width = 10, height = 6, units = "in")


ris_delta_.4 <- ris_long %>%
  select(m,delta, n, NMI, Metodo)%>%
  filter(delta == 0.4, n == 400) 

boxplot_delta.4 <- ggplot(ris_delta_.4, aes(x = Metodo, y = NMI, fill = Metodo))+
  geom_boxplot()+
  facet_wrap(~ m, scales = "free_x")+
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1), 
        plot.title = element_text(hjust = 0.5), 
        strip.background = element_rect(fill="lightblue", color="black"), 
        strip.text = element_text(face="bold"), legend.position = "none")
print(boxplot_delta.4)
ggsave(boxplot_delta.4, file = "nmi_delta.4_n400.pdf",
       width = 10, height = 6, units = "in")

ris_delta_.55 <- ris_long %>%
  select(m,delta, n, NMI, Metodo)%>%
  filter(delta == 0.55, n == 400) 

boxplot_delta.55 <- ggplot(ris_delta_.55, aes(x = Metodo, y = NMI, fill = Metodo))+
  geom_boxplot()+
  facet_wrap(~ m, scales = "free_x")+
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1), 
        plot.title = element_text(hjust = 0.5), 
        strip.background = element_rect(fill="lightblue", color="black"), 
        strip.text = element_text(face="bold"), legend.position = "none")
print(boxplot_delta.55)
ggsave(boxplot_delta.55, file = "nmi_delta.55_n400.pdf",
       width = 10, height = 6, units = "in")

ris_delta_.7 <- ris_long %>%
  select(m,delta, n, NMI, Metodo)%>%
  filter(delta == 0.7, n == 400) 

boxplot_delta.7 <- ggplot(ris_delta_.7, aes(x = Metodo, y = NMI, fill = Metodo))+
  geom_boxplot()+
  facet_wrap(~ m, scales = "free_x")+
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1), 
        plot.title = element_text(hjust = 0.5), 
        strip.background = element_rect(fill="lightblue", color="black"), 
        strip.text = element_text(face="bold"), legend.position = "none")
print(boxplot_delta.7)
ggsave(boxplot_delta.7, file = "nmi_delta.7_n400.pdf",
       width = 10, height = 6, units = "in")















