library(emmeans)
library(lme4)
library(nlme)
library(flexplot)
library(foreign)
library(tidyr)
library(dplyr)
library(multcomp)
library(effects)
library(sjstats)
library(tm)
library(car)
library(pwr)
library(rstatix)
library(geepack)


# Carregando o banco de dados
original_wide = read.spss("bd_New drug_respiratory&pulse.sav", to.data.frame=TRUE)
original_wide


#Colocando a coluna ID e separando o tempos nos nomes de resp e pulse

bd <- original_wide %>%
  rename_with(~gsub("(resp|pulse)(\\d+)", "\\1_\\2", .), -drug) %>% 
  mutate(ID = row_number())%>%
  dplyr::select(ID, everything())
bd



# Pivotando para formato long
bd_long = pivot_longer(
  bd,
  cols = resp_1:pulse_3,
  names_to = c(".value", "Tempo"),
  names_pattern = "(.+)_(.+)")

bd_long

# Esfericidade da variável "resp" 



resp_mauchly = anova_test(data = bd_long, dv = resp, wid = ID, within = Tempo)
resp_mauchly


# Correção
get_anova_table(resp_mauchly) #auto, neste caso  Greenhouse-Geisser
get_anova_table(resp_mauchly, correction = "GG") # Greenhouse-Geisser
get_anova_table(resp_mauchly, correction = "HF") # Hyunh-Feldt 
```

# Modelos para a variável dependente "resp"


modelo1_resp = glm(resp ~ drug + Tempo + drug*Tempo, data = bd_long)
summary(modelo1_resp)
emmeans(modelo1_resp, pairwise ~ Tempo)
emmeans(modelo1_resp, pairwise ~ drug)
emmeans(modelo1_resp, pairwise ~ drug*Tempo)



# Para configurar o Tempo 2 como referência
glimpse(bd_long)
bd_long$Tempo = as.factor(bd_long$Tempo)
bd_long$Tempo <- relevel(bd_long$Tempo, ref = "2")

modelo1_respb = glm(resp ~ drug + drug*Tempo, data = bd_long)
summary(modelo1_respb)
emmeans(modelo1_respb, pairwise ~ Tempo)
emmeans(modelo1_respb, pairwise ~ drug)
emmeans(modelo1_respb, pairwise ~ drug*Tempo)

# Para voltar como referência o tempo 1
bd_long$Tempo <- relevel(bd_long$Tempo, ref = "1")
modelo1_resp = glm(resp ~ drug + drug*Tempo, data = bd_long)
summary(modelo1_resp)





# Ajuste do modelo GEE (Generalized Estimating Equations)
modelo_gee_resp <- geeglm(resp ~ drug + Tempo + drug*Tempo, 
                          data = bd_long, 
                          id = ID, 
                          family = gaussian, 
                          corstr = "unstructured")

# Visualize os resultados
summary(modelo_gee_resp)

# Calcula as estimativas de margens médias (means) para cada nível de Tratamento e Tempo
emmeans(modelo_gee_resp, pairwise ~ drug)
emmeans(modelo_gee_resp, pairwise ~ Tempo)
emmeans(modelo_gee_resp, pairwise ~ drug + Tempo)


# Outra forma de fazer
# means <- emmeans(modelo_gee_resp, ~ drug * Tempo)
# # Realize comparações específicas, como comparação entre grupos em um determinado Tempo
# comparacoes <- contrast(means, method = "pairwise", by = "drug", at = list(Tempo = c("Tempo1", "Tempo2", "Tempo3")))
# 
# # Visualize as comparações
# summary(comparacoes)

```

### GMM para a VD resp


modelo_gmm_resp = lme(
  fixed = resp ~ drug + Tempo + drug * Tempo,
  random = ~1|ID,
  #correlation = corIdent(form = ~ 1|id),
  data = bd_long
)

summary(modelo_gmm_resp)
emmeans(modelo_gmm_resp, pairwise ~ drug)
emmeans(modelo_gmm_resp, pairwise ~ Tempo)
emmeans(modelo_gmm_resp, pairwise ~ drug*Tempo)

