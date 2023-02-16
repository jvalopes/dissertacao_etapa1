
# pacotes
library(readr)
library(tidyverse)
library(rio) # export .xlsx

# Campinas 2016 (datasus)
setwd('C:/Users/User/Documents/dissertacao_etapa1/raw_data/Campinas/tabnet_datasus/.csv')

cps_2016 <- read_delim("campinas_2016_dt.csv", 
                       delim = ";", escape_double = FALSE, trim_ws = TRUE, 
                       skip = 4)

df <- cps_2016[-c(450:457), -23] # removendo 'total' e as ultimas linhas

df <- df %>% mutate_at(c
                       ('0 a 6 dias', '7 a 27 dias', '28 a 364 dias', '1 a 4 anos',
                         '5 a 9 anos', '10 a 14 anos', '15 a 19 anos', '20 a 24 anos', '25 a 29 anos',
                         '30 a 34 anos', '35 a 39 anos', '40 a 44 anos', '45 a 49 anos', '50 a 54 anos',
                         '55 a 59 anos', '60 a 64 anos', '65 a 69 anos', '70 a 74 anos', '75 a 79 anos',
                         '80 anos e mais', 'Idade ignorada'), as.numeric, NA) # convertendo as idade para 'numeric'

sapply(df, class)

df$'0 a 4 anos' <- rowSums(df[, 2:5], na.rm = T) # agrupando 0 a 4 anos
df <- df[, -c(2:5)] # removendo as colunas que foram agrupadas
df <- df %>% relocate('0 a 4 anos', .before = '5 a 9 anos')
df <- df %>% rename('cid10' = 'Categoria CID-10')

df2 <- df %>% 
  select('cid10',
         '0 a 4 anos', '5 a 9 anos',
         '10 a 14 anos', '15 a 19 anos', 
         '20 a 24 anos', '25 a 29 anos',
         '30 a 34 anos', '35 a 39 anos',
         '40 a 44 anos', '45 a 49 anos',
         '50 a 54 anos', '55 a 59 anos',
         '60 a 64 anos', '65 a 69 anos',
         '70 a 74 anos', '75 a 79 anos',
         '80 anos e mais','Idade ignorada') %>%
  mutate(cid_grupos = case_when(
    substr(cid10, 1,1) == "C" ~ 'Neoplasmas',
    substr(cid10, 1,1) == "D" & as.numeric(substr(cid10,2,4) < 49) ~ 'Neoplasmas',
    substr(cid10, 1,1) == "E" ~ 'Doenças endócrinas, nutricionais e metabólicas',
    substr(cid10, 1,1) == "I" ~ 'Doenças do aparelho circulatório',
    substr(cid10, 1,1) == "J" ~ 'Doenças do aparelho respiratório',
    substr(cid10, 1,1) == "N" ~ 'Doenças do aparelho geniturinário',
    substr(cid10, 1,1) == "U" ~ 'Códigos para propósitos especiais'))

df3 <- df2 %>% relocate(cid_grupos, .after = cid10)

# criando um df com as cids agrupadas
df4 <- filter(df3, cid_grupos == 'Neoplasmas'|
                cid_grupos == 'Doenças endócrinas, nutricionais e metabólicas'|
                cid_grupos == 'Doenças do aparelho circulatório'|
                cid_grupos == 'Doenças do aparelho respiratório'|
                cid_grupos == 'Doenças do aparelho geniturinário'|
                cid_grupos == 'Códigos para propósitos especiais')
df4 <- df4[, -1]

# pivoteando o df
df4 %>% pivot_longer(!cid_grupos) %>% na.omit() %>% 
  group_by(cid_grupos, name) %>% 
  summarise(nr_mortes = sum(value)) %>% 
  pivot_wider(
    names_from = name,
    values_from = nr_mortes
  ) -> df5

df5 <- df5 %>% relocate('5 a 9 anos', .after = '0 a 4 anos')
df5 <- df5 %>% relocate('10 a 14 anos', .after = '5 a 9 anos')

setwd('C:/Users/User/Documents/dissertacao_etapa1/analysis_Campinas/campinas_datasus')
export(df5, "cps2016_dt_c.xlsx")
