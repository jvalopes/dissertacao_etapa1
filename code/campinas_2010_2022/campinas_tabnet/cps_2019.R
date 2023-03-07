
# pacotes
library(readr)
library(tidyverse)
library(rio) # export .xlsx

# Campinas 2019
setwd('C:/Users/User/Documents/dissertacao_etapa1/raw_data/Campinas/tabnet_Campinas/.csv')

cps_2019 <- read_delim("campinas_2019.csv", 
                       delim = ";", escape_double = FALSE, trim_ws = TRUE, 
                       skip = 3)

df <- cps_2019[-487, -24] # removendo 'total' e as ultimas linhas

sapply(df, class)

df <- df %>% rename('cid10' = 'Causa (CID10 3C)')

df2 <- df %>% 
  select('cid10',
         '0-4', '5-9', '10-14', '15-19', '20-24',
         '25-29', '30-34', '35-39', '40-44', '45-49',
         '50-54', '55-59', '60-64', '65-69', '70-74',
         '75-79', '80-84', '85-89', '90-94', '95-99', '100e+', 'Ign') %>%
  mutate(cid_grupos = case_when(
    substr(cid10, 1,1) == "C" ~ 'Neoplasmas',
    substr(cid10, 1,1) == "D" & as.numeric(substr(cid10,2,4) < 49) ~ 'Neoplasmas',
    substr(cid10, 1,1) == "E" ~ 'Doenças endócrinas, nutricionais e metabólicas',
    substr(cid10, 1,1) == "I" ~ 'Doenças do aparelho circulatório',
    substr(cid10, 1,1) == "J" ~ 'Doenças do aparelho respiratório',
    substr(cid10, 1,1) == "N" ~ 'Doenças do aparelho geniturinário',
    substr(cid10, 1,1) == "U" ~ 'Códigos para propósitos especiais',
    TRUE ~ 'Outros'))

df3 <- df2 %>% relocate(cid_grupos, .after = cid10)

# criando um df com as cids agrupadas
df4 <- filter(df3, cid_grupos == 'Neoplasmas'|
                cid_grupos == 'Doenças endócrinas, nutricionais e metabólicas'|
                cid_grupos == 'Doenças do aparelho circulatório'|
                cid_grupos == 'Doenças do aparelho respiratório'|
                cid_grupos == 'Doenças do aparelho geniturinário'|
                cid_grupos == 'Códigos para propósitos especiais'|
                cid_grupos == 'Outros')

df4 <- df4[, -1]

# pivoteando o df
df4 %>% pivot_longer(!cid_grupos) %>% na.omit() %>% 
  group_by(cid_grupos, name) %>% 
  summarise(nr_mortes = sum(value)) %>% 
  pivot_wider(
    names_from = name,
    values_from = nr_mortes
  ) -> df5

df5 <- df5 %>% relocate('5-9', .after = '0-4')
df5 <- df5 %>% relocate('100e+', .after = '95-99')

df5 <- df5 %>% rename(
  '0 a 4 anos' = '0-4',
  '5 a 9 anos' = '5-9',
  '10 a 14 anos' = '10-14',
  '15 a 19 anos' = '15-19',
  '20 a 24 anos' = '20-24',
  '25 a 29 anos' = '25-29',
  '30 a 34 anos' = '30-34',
  '35 a 39 anos' = '35-39',
  '40 a 44 anos' = '40-44',
  '45 a 49 anos' = '45-49',
  '50 a 54 anos' = '50-54',
  '55 a 59 anos' = '55-59',
  '60 a 64 anos' = '60-64',
  '65 a 69 anos' = '65-69',
  '70 a 74 anos' = '70-74',
  '75 a 79 anos' = '75-79',
  '80 a 84 anos' = '80-84',
  '85 a 89 anos' = '85-89',
  '90 a 94 anos' = '90-94',
  '95 a 99 anos' = '95-99',
  '100 anos e mais' = '100e+',
  'Idade ignorada' = 'Ign')

# obtendo total
# para as colunas
totais <- colSums(df5[, 2:ncol(df5)], na.rm = T)
totais <- as.list(totais)
df5[nrow(df5) + 1, ] <- c("Total", totais)

# para as linhas
df5$'Total' <- rowSums(df5[, 2:ncol(df5)], na.rm = T)

setwd('C:/Users/User/Documents/dissertacao_etapa1/analysis_Campinas/campinas_tabnet')
export(df5, "cps2019_c.xlsx")
