library(tidyverse)
library(xtable)

setwd("G:/My Drive/Università/Magistrale/Tesi/Studio di simulazione/Risultati/n_var")

load("n_var_1.1.RData") #n = 100, tau = rep(1/3, 3), delta = 0.25, m = c(4,4,4,4,3,3,3,3)
n_var_1.1 <- df_results
load("n_var_1.2.RData")
n_var_1.2 <- df_results #n = 100, tau = rep(1/3, 3), delta = 0.25, m = c(4,4,4,4,3,3,3,3,4,4,4,4)
load("n_var_1.1.1.RData")
n_var_1.1.1 <- df_results # n = 600, tau = rep(1/3, 3), delta = 0.25, m = c(4,4,4,4,3,3,3,3)
load("n_var_1.2.1.RData")
n_var_1.2.1 <- df_results #n = 600, tau = rep(1/3, 3), delta = 0.25, m = c(4,4,4,4,3,3,3,3,4,4,4,4)

n_var_1 <- rbind(cbind(n_var_1.1, "n" = 100, "nvar" = 8),
                 cbind(n_var_1.2, "n" = 100, "nvar" = 12),
                 cbind(n_var_1.1.1, "n" = 600, "nvar" = 8),
                 cbind(n_var_1.1.1, "n" = 600, "nvar" = 12))

load("n_var_2.1.RData") #n = 100, tau = rep(1/3, 3), delta = 0.4, m = c(4,4,4,4,3,3,3,3)
n_var_2.1 <- df_results
load("n_var_2.2.RData")
n_var_2.2 <- df_results #n = 100, tau = rep(1/3, 3), delta = 0.4, m = c(4,4,4,4,3,3,3,3,4,4,4,4)
load("n_var_2.1.1.RData")
n_var_2.1.1 <- df_results # n = 600, tau = rep(1/3, 3), delta = 0.4, m = c(4,4,4,4,3,3,3,3)
load("n_var_2.2.1.RData")
n_var_2.2.1 <- df_results #n = 600, tau = rep(1/3, 3), delta = 0.4, m = c(4,4,4,4,3,3,3,3,4,4,4,4)

n_var_2 <- rbind(cbind(n_var_2.1, "n" = 100, "nvar" = 8),
                 cbind(n_var_2.2, "n" = 100, "nvar" = 12),
                 cbind(n_var_2.1.1, "n" = 600, "nvar" = 8),
                 cbind(n_var_2.1.1, "n" = 600, "nvar" = 12))

load("n_var_3.1.RData") #n = 100, tau = rep(1/3, 3), delta = 0.55, m = c(4,4,4,4,3,3,3,3)
n_var_3.1 <- df_results
load("n_var_3.2.RData")
n_var_3.2 <- df_results #n = 100, tau = rep(1/3, 3), delta = 0.55, m = c(4,4,4,4,3,3,3,3,4,4,4,4)
load("n_var_3.1.1.RData")
n_var_3.1.1 <- df_results # n = 600, tau = rep(1/3, 3), delta = 0.55, m = c(4,4,4,4,3,3,3,3)
load("n_var_3.2.1.RData")
n_var_3.2.1 <- df_results #n = 600, tau = rep(1/3, 3), delta = 0.55, m = c(4,4,4,4,3,3,3,3,4,4,4,4)

n_var_3 <- rbind(cbind(n_var_3.1, "n" = 100, "nvar" = 8),
                 cbind(n_var_3.2, "n" = 100, "nvar" = 12),
                 cbind(n_var_3.1.1, "n" = 600, "nvar" = 8),
                 cbind(n_var_3.1.1, "n" = 600, "nvar" = 12))


n_var <- rbind(cbind(n_var_1, "delta" = 0.25),
               cbind(n_var_2, "delta" = 0.4),
               cbind(n_var_3, "delta" = 0.55))

names(n_var)[3] <-"Time_Kmodes"

colonne_tempo <- names(n_var)[str_detect(names(n_var), "^Time_")]
n_var <- n_var %>%
  mutate(across(all_of(colonne_tempo), ~as.numeric(str_replace(.x, "_secs", ""))))

ris_long <- n_var %>%
  pivot_longer(
    cols = -c(n,delta, nvar),
    names_to = c(".value", "Metodo"), 
    names_pattern = "(ARI|NMI|Time)_(.+)" 
  ) %>%
  mutate(Metodo = str_to_upper(Metodo))


summary_mix <- ris_long %>%
  group_by(n, Metodo, delta, nvar) %>%
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

ris_long$nvar <- as.factor(ris_long$nvar)
levels(ris_long$nvar) <- c("# Var = 8", "# Var = 12")

setwd("G:/My Drive/Università/Magistrale/Tesi/Studio di simulazione/Grafici/n_var")



# ARI ---------------------------------------------------------------------

ris_long$Metodo <- as.factor(ris_long$Metodo)


# delta = 0.25

ris_delta_.25 <- ris_long %>%
  select(nvar,delta, n, ARI, Metodo)%>%
  filter(delta == 0.25, n == 100) 

boxplot_delta.25 <- ggplot(ris_delta_.25, aes(x = Metodo, y = ARI, fill = Metodo))+
  geom_boxplot()+
  facet_wrap(~ nvar, scales = "free_x")+
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1), 
        plot.title = element_text(hjust = 0.5), 
        strip.background = element_rect(fill="lightblue", color="black"), 
        strip.text = element_text(face="bold"), legend.position = "none")
print(boxplot_delta.25)
ggsave(boxplot_delta.25, file = "boxplot_delta.25_n100.pdf",
       width = 10, height = 6, units = "in")


library(broom)



anova_results <- ris_delta_.25 %>%
  group_by(Metodo) %>%
  do(tidy(aov(ARI ~ nvar, data = .))) %>%
  filter(term == "nvar")

anova <- xtable(anova_results, digits = c(0, 0, 0, 0, 6, 6, 6, 4))
print(anova, include.rownames = F)




ris_delta_.25 <- ris_long %>%
  select(nvar,delta, n, ARI, Metodo)%>%
  filter(delta == 0.25, n == 600)

anova_results <- ris_delta_.25 %>%
  group_by(Metodo) %>%
  do(tidy(aov(ARI ~ nvar, data = .))) %>%
  filter(term == "nvar")

anova <- xtable(anova_results, digits = c(0, 0, 0, 0, 6, 6, 6, 4))
print(anova, include.rownames = F)

boxplot_delta.25 <- ggplot(ris_delta_.25, aes(x = Metodo, y = ARI, fill = Metodo))+
  geom_boxplot()+
  facet_wrap(~ nvar, scales = "free_x")+
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1), 
        plot.title = element_text(hjust = 0.5), 
        strip.background = element_rect(fill="lightblue", color="black"), 
        strip.text = element_text(face="bold"), legend.position = "none")
print(boxplot_delta.25)
ggsave(boxplot_delta.25, file = "boxplot_delta.25_n600.pdf",
       width = 10, height = 6, units = "in")

#delta = 0.4

ris_delta_.4 <- ris_long %>%
  select(nvar,delta, n, ARI, Metodo)%>%
  filter(delta == 0.4, n == 100)


anova_results <- ris_delta_.4 %>%
  group_by(Metodo) %>%
  do(tidy(aov(ARI ~ nvar, data = .))) %>%
  filter(term == "nvar")
anova <- xtable(anova_results, digits = c(0, 0, 0, 0, 6, 6, 6, 4))
print(anova, include.rownames = F)

boxplot_delta.4 <- ggplot(ris_delta_.4, aes(x = Metodo, y = ARI, fill = Metodo))+
  geom_boxplot()+
  facet_wrap(~ nvar, scales = "free_x")+
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1), 
        plot.title = element_text(hjust = 0.5), 
        strip.background = element_rect(fill="lightblue", color="black"), 
        strip.text = element_text(face="bold"), legend.position = "none")
print(boxplot_delta.4)
ggsave(boxplot_delta.4, file = "boxplot_delta.4_n100.pdf",
       width = 10, height = 6, units = "in")



ris_delta_.4 <- ris_long %>%
  select(nvar,delta, n, ARI, Metodo)%>%
  filter(delta == 0.25, n == 600)

boxplot_delta.4 <- ggplot(ris_delta_.4, aes(x = Metodo, y = ARI, fill = Metodo))+
  geom_boxplot()+
  facet_wrap(~ nvar, scales = "free_x")+
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1), 
        plot.title = element_text(hjust = 0.5), 
        strip.background = element_rect(fill="lightblue", color="black"), 
        strip.text = element_text(face="bold"), legend.position = "none")
print(boxplot_delta.4)
ggsave(boxplot_delta.4, file = "boxplot_delta.4_n600.pdf",
       width = 10, height = 6, units = "in")


#Delta = 0.55

ris_delta_.55 <- ris_long %>%
  select(nvar,delta, n, ARI, Metodo)%>%
  filter(delta == 0.55, n == 100)

boxplot_delta.55 <- ggplot(ris_delta_.55, aes(x = Metodo, y = ARI, fill = Metodo))+
  geom_boxplot()+
  facet_wrap(~ nvar, scales = "free_x")+
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1), 
        plot.title = element_text(hjust = 0.5), 
        strip.background = element_rect(fill="lightblue", color="black"), 
        strip.text = element_text(face="bold"), legend.position = "none")
print(boxplot_delta.55)
ggsave(boxplot_delta.55, file = "boxplot_delta.55_n100.pdf",
       width = 10, height = 6, units = "in")



ris_delta_.55 <- ris_long %>%
  select(nvar,delta, n, ARI, Metodo)%>%
  filter(delta == 0.55, n == 600)

boxplot_delta.55 <- ggplot(ris_delta_.55, aes(x = Metodo, y = ARI, fill = Metodo))+
  geom_boxplot()+
  facet_wrap(~ nvar, scales = "free_x")+
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1), 
        plot.title = element_text(hjust = 0.5), 
        strip.background = element_rect(fill="lightblue", color="black"), 
        strip.text = element_text(face="bold"), legend.position = "none")
print(boxplot_delta.55)
ggsave(boxplot_delta.55, file = "boxplot_delta.55_n600.pdf",
       width = 10, height = 6, units = "in")





# NMI ---------------------------------------------------------------------


# delta = 0.25

ris_delta_.25 <- ris_long %>%
  select(nvar,delta, n, NMI, Metodo)%>%
  filter(delta == 0.25, n == 100)

boxplot_delta.25 <- ggplot(ris_delta_.25, aes(x = Metodo, y = NMI, fill = Metodo))+
  geom_boxplot()+
  facet_wrap(~ nvar, scales = "free_x")+
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1), 
        plot.title = element_text(hjust = 0.5), 
        strip.background = element_rect(fill="lightblue", color="black"), 
        strip.text = element_text(face="bold"), legend.position = "none")
print(boxplot_delta.25)
ggsave(boxplot_delta.25, file = "nmi_delta.25_n100.pdf",
       width = 10, height = 6, units = "in")



ris_delta_.25 <- ris_long %>%
  select(nvar,delta, n, NMI, Metodo)%>%
  filter(delta == 0.25, n == 600)

boxplot_delta.25 <- ggplot(ris_delta_.25, aes(x = Metodo, y = NMI, fill = Metodo))+
  geom_boxplot()+
  facet_wrap(~ nvar, scales = "free_x")+
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1), 
        plot.title = element_text(hjust = 0.5), 
        strip.background = element_rect(fill="lightblue", color="black"), 
        strip.text = element_text(face="bold"), legend.position = "none")
print(boxplot_delta.25)
ggsave(boxplot_delta.25, file = "nmi_delta.25_n600.pdf",
       width = 10, height = 6, units = "in")

#delta = 0.4

ris_delta_.4 <- ris_long %>%
  select(nvar,delta, n, NMI, Metodo)%>%
  filter(delta == 0.4, n == 100)

boxplot_delta.4 <- ggplot(ris_delta_.4, aes(x = Metodo, y = NMI, fill = Metodo))+
  geom_boxplot()+
  facet_wrap(~ nvar, scales = "free_x")+
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1), 
        plot.title = element_text(hjust = 0.5), 
        strip.background = element_rect(fill="lightblue", color="black"), 
        strip.text = element_text(face="bold"), legend.position = "none")
print(boxplot_delta.4)
ggsave(boxplot_delta.4, file = "nmi_delta.4_n100.pdf",
       width = 10, height = 6, units = "in")



ris_delta_.4 <- ris_long %>%
  select(nvar,delta, n, NMI, Metodo)%>%
  filter(delta == 0.25, n == 600)

boxplot_delta.4 <- ggplot(ris_delta_.4, aes(x = Metodo, y = NMI, fill = Metodo))+
  geom_boxplot()+
  facet_wrap(~ nvar, scales = "free_x")+
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1), 
        plot.title = element_text(hjust = 0.5), 
        strip.background = element_rect(fill="lightblue", color="black"), 
        strip.text = element_text(face="bold"), legend.position = "none")
print(boxplot_delta.4)
ggsave(boxplot_delta.4, file = "nmi_delta.4_n600.pdf",
       width = 10, height = 6, units = "in")


#Delta = 0.55

ris_delta_.55 <- ris_long %>%
  select(nvar,delta, n, NMI, Metodo)%>%
  filter(delta == 0.55, n == 100)

boxplot_delta.55 <- ggplot(ris_delta_.55, aes(x = Metodo, y = NMI, fill = Metodo))+
  geom_boxplot()+
  facet_wrap(~ nvar, scales = "free_x")+
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1), 
        plot.title = element_text(hjust = 0.5), 
        strip.background = element_rect(fill="lightblue", color="black"), 
        strip.text = element_text(face="bold"), legend.position = "none")
print(boxplot_delta.55)
ggsave(boxplot_delta.55, file = "nmi_delta.55_n100.pdf",
       width = 10, height = 6, units = "in")







ris_delta_.55 <- ris_long %>%
  select(nvar,delta, n, NMI, Metodo)%>%
  filter(delta == 0.55, n == 600)

boxplot_delta.55 <- ggplot(ris_delta_.55, aes(x = Metodo, y = NMI, fill = Metodo))+
  geom_boxplot()+
  facet_wrap(~ nvar, scales = "free_x")+
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1), 
        plot.title = element_text(hjust = 0.5), 
        strip.background = element_rect(fill="lightblue", color="black"), 
        strip.text = element_text(face="bold"), legend.position = "none")
print(boxplot_delta.55)
ggsave(boxplot_delta.55, file = "nmi_delta.55_n600.pdf",
       width = 10, height = 6, units = "in")
