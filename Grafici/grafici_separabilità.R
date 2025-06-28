library(poLCA)
G <- length(tau) #numero dei cluster
d <-  length(m) #numero delle variabili 

#Simulazione dei dati 

G = 4
m = c(4,3, 4,5,6)
delta = 0.25
tau = rep(0.25, 4)
d = length(m)
n = 1000

prob = prob_poLCA(K = G, d = d, m_vec = m, delta = delta)
sim.data <- poLCA.simdata(N = n, probs = prob, P = tau)

df_sim_raw <- sim.data$dat
df_sim_factor <- as.data.frame(lapply(df_sim_raw, factor))
df_sim_factor$ClasseVera <- factor(sim.data$trueclass)
res.mca <- MCA(df_sim_factor, quali.sup = ncol(df_sim_factor), graph = FALSE, ncp = 5)

fviz_mca_ind(res.mca,
              label = "none",
              habillage = "ClasseVera",
              palette = "Set1", # Scegli una palette di colori
              addEllipses = TRUE, ellipse.type = "confidence",
              ggtheme = theme_minimal(),
              title = "",
              xlab = "Prima componente analisi in corrispondenze",
              ylab = "Prima componente analisi in corrispondenze")+
   theme(legend.position = "none")


#delta = 0.55

G = 4
m = c(4,3, 4,5,6)
delta = 0.55
tau = rep(0.25, 4)
d = length(m)
n = 1000

prob = prob_poLCA(K = G, d = d, m_vec = m, delta = delta)
sim.data <- poLCA.simdata(N = n, probs = prob, P = tau)

df_sim_raw <- sim.data$dat
df_sim_factor <- as.data.frame(lapply(df_sim_raw, factor))
df_sim_factor$ClasseVera <- factor(sim.data$trueclass)
res.mca <- MCA(df_sim_factor, quali.sup = ncol(df_sim_factor), graph = FALSE, ncp = 5)

fviz_mca_ind(res.mca,
             label = "none",
             habillage = "ClasseVera",
             palette = "Set1", # Scegli una palette di colori
             addEllipses = TRUE, ellipse.type = "confidence",
             ggtheme = theme_minimal(),
             title = "",
             xlab = "Prima componente analisi in corrispondenze",
             ylab = "Prima componente analisi in corrispondenze")+
  theme(legend.position = "none")


#delta = .7

G = 4
m = c(4,3, 4,5,6)
delta = 0.7
tau = rep(0.25, 4)
d = length(m)
n = 1000

prob = prob_poLCA(K = G, d = d, m_vec = m, delta = delta)
sim.data <- poLCA.simdata(N = n, probs = prob, P = tau)

df_sim_raw <- sim.data$dat
df_sim_factor <- as.data.frame(lapply(df_sim_raw, factor))
df_sim_factor$ClasseVera <- factor(sim.data$trueclass)
res.mca <- MCA(df_sim_factor, quali.sup = ncol(df_sim_factor), graph = FALSE, ncp = 5)

fviz_mca_ind(res.mca,
             label = "none",
             habillage = "ClasseVera",
             palette = "Set1", # Scegli una palette di colori
             addEllipses = TRUE, ellipse.type = "confidence",
             ggtheme = theme_minimal(),
             title = "",
             xlab = "Prima componente analisi in corrispondenze",
             ylab = "Prima componente analisi in corrispondenze")+
  theme(legend.position = "none")



 