# pacotes
library(readxl)
library(tidyr)
library(dplyr) # mutate_at, etc

sp_2010 <- read_excel("raw_data/Estado São Paulo/sp_2010_dt_teste.xlsx", 
                               col_names = FALSE)

# limpando 
sp_2010 <- sp_2010[-c(1:5), -24] # primeiras linhas
sp_2010 <- sp_2010[-c(1120:1128), ] # ultimas

# nomeando as colunas
names(sp_2010)[1:23] <- c('cid10', 'x2', 'x3', 'x4', 'x5', 'x6', 'x7', 'x8', 'x9', 'x10',
                          'x11', 'x12', 'x13', 'x14', 'x15', 'x16', 'x17', 'x18', 'x19',
                          'x20', 'x21', 'x22', 'x23')

# verificando classe do df
sapply(sp_2010, class)

# convertendo a classe
sp_2010 <- sp_2010 %>% mutate_at(c('x2', 'x3', 'x4', 'x5', 'x6', 'x7', 'x8', 'x9', 'x10',
                                   'x11', 'x12', 'x13', 'x14', 'x15', 'x16', 'x17', 'x18', 'x19',
                                   'x20', 'x21', 'x22', 'x23'), as.numeric, NA)

sapply(sp_2010, class)

sp_2010$xn <- rowSums(sp_2010[, 2:6], na.rm = T) # agrupando as idades < 1

#sp_2010$xn

# excluindo as colunas que foram agrupadas em xn
sp_2010 <- sp_2010[, -c(2:6)]

# reorganizando
sp_2010 <- sp_2010 %>% relocate(xn, .before = x7)

# renomeando as colunas
sp_2010 <- sp_2010 %>% rename(
  '0-4' = xn,
  '5-9' = x7,
  '10-14' = x8,
  '15-19' = x9,
  '20-24' = x10,
  '25-29' = x11,
  '30-34' = x12,
  '35-39' = x13,
  '40-44' = x14,
  '45-49' = x15,
  '50-54' = x16,
  '55-59' = x17,
  '60-64' = x18,
  '65-69' = x19,
  '70-74' = x20,
  '75-79' = x21,
  '80+' = x22,
  'Ign' = x23)

#sp_2010 %>% 
  #separate(cid10, into = c('cod', 'nome'))

df <- sp_2010 %>% 
  select(cid10, '0-4', '5-9', '10-14', '15-19',
         '20-24', '25-29', '30-34', '35-39', '40-44', '45-49', 
         '50-54', '55-59', '60-64', '65-69', '70-74', '75-79',
         '80+', 'Ign') %>% 
  mutate(cid_grupos = case_when(
    substr(cid10, 1,1) == "C" ~ 'Neoplasmas',
    substr(cid10, 1,1) == "D" & as.numeric(substr(cid10,2,4) < 49) ~ 'Neoplasmas',
    substr(cid10, 1,1) == "E" ~ 'Doenças endócrinas, nutricionais e metabólicas',
    substr(cid10, 1,1) == "I" ~ 'Doenças do aparelho circulatório',
    substr(cid10, 1,1) == "J" ~ 'Doenças do aparelho respiratório',
    substr(cid10, 1,1) == "N" ~ 'Doenças do aparelho geniturinário',
    substr(cid10, 1,1) == "U" ~ 'Códigos para propósitos especiais'))
    
df <- df %>% relocate(cid_grupos, .after = cid10 )

df2 <- filter(df, cid_grupos == 'Neoplasmas'|
         cid_grupos == 'Doenças endócrinas, nutricionais e metabólicas'|
         cid_grupos == 'Doenças do aparelho circulatório'|
         cid_grupos == 'Doenças do aparelho respiratório'|
         cid_grupos == 'Doenças do aparelho geniturinário'|
         cid_grupos == 'Códigos para propósitos especiais')

df2 <- df2[, -1]

df2 %>% pivot_longer(!cid_grupos) %>% na.omit() %>% 
  group_by(cid_grupos, name) %>% 
 summarise(nr_mortes = sum(value)) %>% 
  pivot_wider(
    names_from = name,
    values_from = nr_mortes
  )-> df4


sum(df3$cid_grupos == 'Neoplasmas')
sum(df3$cid_grupos == 'Doenças endócrinas, nutricionais e metabólicas') 
sum(df3$cid_grupos == 'Doenças do aparelho circulatório') 
sum(df3$cid_grupos == 'Doenças do aparelho respiratório')
sum(df3$cid_grupos == 'Doenças do aparelho geniturinário')
sum(df$cid_grupos == 'Códigos para propósitos especiais')
