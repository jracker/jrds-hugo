---
title: "Mineração de dados - regra de associação apriori"
date: "2022-06-21"
excerpt: Aplicação do algoritmo apriori
tags:
- Regras de associação
- Mineração de dados
- R
---



## Mineração de dados

Inicialmente são carregadas as bibliotecas que serão usadas.


```r
if(!require(tidyverse)) install.packages("tidyverse")
if(!require(arulesViz)) install.packages("arulesViz")
if(!require(skimr)) install.packages("skimr")
if(!require(readr)) install.packages("readr")
# Bibliotecas usadas
library(arules)
library(tidyverse)
library(arulesViz)
library(skimr)
library(readr)
library(here)
```

## Descrição dos dados


Os dados disponibilizados descrevem resultados de múltiplas partidas com descrição dos jogadores presentes. O objetivo é extrair informações "relevantes" como os melhores jogadores ou melhores duplas/trios aplicando o algoritmo apriori. 



## Pré-processamento dos dados

 A próxima etapa é importar os dados. Uma vez que os dados já foram baixados e estão localizados no diretório de trabalho, não foi necessário especificar o caminho. Nota-se no pedaço de código abaixo que as células sem informação foram tratadas como `NA`.



```r
# Importa o arquivo csv e trata as linhas em branco como NA
path <- here("content/blog/21062022-miningpadel/_ASSOC_PadelStars.csv")
dat <- read.csv(path) %>%
  na_if("")
# View(dat) # Visualizar o formato dos dados
```

As colunas dos jogadores foram renomeadas para facilitar a análise. A  coluna que contém os jogadores foi em seguida separada em três variáveis.


```r
# Renomeia colunas para facilitar o pré-processamento
dat_v1 <- dat %>%
  rename(
    Jogadores = Jogadore.a.s,
    Jogador_4 = Jogadore.a.s.1
  )
# Separa os jogadores em colunas distintas
dat_v2 <- dat_v1 %>%
  separate(Jogadores, c("Jogador_1", "Jogador_2", "Jogador_3"), sep = ",")
```


Na coluna `Jogador_4` há nomes de jogadores repetidos em relação as células das demais colunas e foram transformados para `NA`, já que são redundantes na análise.


```r
# Adiciona NA nos nomes duplicados (considerando que 
#não há mais de um jogador com mesmo nome)
dat_v2$Jogador_4[1] <- NA # Alejandro
dat_v2$Jogador_4[3] <- NA # Carlos
dat_v2$Jogador_4[7] <- NA # Paula
```

Os nomes com caracteres diferentes do esperado são corrigidos e as células sem informação  substituídas por `NA`.


```r
# Corrige nomes com caracteres diferentes do esperado
dat_v3 <- dat_v2 %>%
  mutate(
    across(-Partida, ~ str_replace_all(., c(
      "alejandro" = "Alejandro",
      "alejandro\\*" = "Alejandro",
      "Ariana\\*" = "Ariana",
      "LÃºcia" = "Lucia",
      "Juam" = "Juan",
      "ALEJANDRO" = "Alejandro",
      "GANHOU" = "Ganhou",
      "Alejandro\\*" = "Alejandro"
    ))) %>%
      mutate_all(na_if, "") # subs esp.  em branco por NA
  ) %>%
  select(-c("Partida", "X"))
```


A função `get_pnames` extrai os nomes de todos os jogadores disponíveis nos dados. Os dados foram ordenados de modo que cada jogador seja uma coluna e a ocorrência deste em uma partida é preenchido com 1 e a ausência com 0. De modo similar, na coluna resultado a ocorrência de uma vitória é caracterizada pelo número 1 e a derrota por 0. A função `get_fdata` retorna as colunas dos jogadores neste formato.


```r
get_pnames <- function(data) {
  # Retorna nomes dos jogadores
  data %>%
    distinct(Jogador_1) %>%
    pull()
}
get_fdata <- function(data, nome) {
  # Retorna os dados mais uma coluna com o nome do jogador
  # Preenchidas com 1 (presença) e 0 (ausência).
  dat_bin <- data %>%
    mutate(
      !!as.character(nome) := across(
        starts_with("Jogador"),
        ~ if_else(grepl(nome, .), 1, 0)
      )
    )
  datbin_v2 <- dat_bin %>%
    mutate(
      !!as.character(nome) := rowSums(select(., contains(nome)))
    ) %>%
    pull(!!as.character(nome)) %>%
    as_tibble(.)

  datbin_v2 %>%
    rename(!!as.character(nome) := value)
}
```

A seguir as funções para o tratamento dos dados são usadas e são aplicadas as  
mudanças finais da coluna resultado como a transformação de "Ganhou" para 1 e "Perdeu" para 0, além da transformação das variáveis para a classe fator. 


```r
# Nome dos jogadores
nomes <- get_pnames(dat_v3)
  # Tratamento dos dados para o formato das transações
# Retorna dado + nomes dos jogadores preenchidos com 1 e 0
dat_v4 <- dat_v3
for (jogador in nomes) {
  dat_v4 <- dat_v4 %>%
    add_column(get_fdata(., jogador))
}
# Vitória = 1 Derrota = 0
dat_v5 <- dat_v4 %>%
  mutate(
    across(starts_with("Resultado"), ~ if_else(grepl("Ganhou", .), 1, 0))
  ) %>%
  select(all_of(nomes), Resultado)
# Transformação das colunas para fator
dat_v6 <- dat_v5 %>%
  mutate(across(everything(), .fns = as.factor))

# Exporta e importa o csv
#write_csv(dat_v6,"assoc_p.csv")
# Importa csv
#dat_v6 <- read_csv("assoc_p.csv") %>% 
# mutate(across(everything(), .fns = as.factor))

#str(dat_v6)
```

## Aplicação do algoritmo Apriori

A mineração dos itens e das regras foi feita através do algoritmo Apriori que trabalha através da busca de itens frequentes dado um suporte e confiança mínima. O Apriori permite reduzir o número de itens que serão analisados para gerar as regras de associação.

.
Os critérios usados foram o suporte de `\(10\%\)` e confiança mínima de `\(60\%\)`. Inicialmente, há o interesse de descobrir quais as três melhores combinações. Isto foi feito definindo o lado `rhs = "Resultado=1",` (vitórias) em um dos parâmetros da função `apriori`.


```r
regras <- apriori(dat_v6,
  parameter = list(
    supp = 0.1,
    conf = 0.6,
    minlen = 2,
    #maxlen = 10,
    target = "rules"
  ),
  appearance = list(
    default = "lhs",
    rhs = "Resultado=1"
  )
)

#inspect(regras)

#reg1_df <- as(regras,"data.frame")
# reg1_df %>% 
#   arrange(desc(support,confidence)) %>% View()
```

Os resultados são filtrados para os jogadores do lado esquerdo que estavam presentes na partida (LHS = 1). Para melhorar a visualização as regras são transformadas em um quadro de dados ordenados pelos maiores valores de suporte e confiança.


```r
# Regras com lhs = 1 e Resultado=1
reg1_lhs1 <- regras[sapply(as(items(regras), "list"), function(x) all(grepl("=1$", x)))]
# Combinações vencedoras
r1lhs1_df <- as(reg1_lhs1,"data.frame") %>% 
  arrange(desc(confidence)) 
(r1lhs1_df)
```

```
##                                    rules   support confidence  coverage
## 1 {Paula=1,Alejandro=1} => {Resultado=1} 0.1192053  0.7200000 0.1655629
## 2  {Juan=1,Alejandro=1} => {Resultado=1} 0.1192053  0.6666667 0.1788079
## 3         {Alejandro=1} => {Resultado=1} 0.2847682  0.6615385 0.4304636
##       lift count
## 1 1.309880    18
## 2 1.212851    18
## 3 1.203522    43
```

 De acordo com as três combinações vencedoras Alejandro obteve a vitória em `\(66\%\)` das partidas que participou, tendo participado em cerca de `\(28\%\)` do total de partidas. A melhor dupla é formada por Alejandro e Paula, que obtiveram a vitória em `\(72\%\)` das partidas que participaram e estiveram presentes em cerca de `\(12\%\)` do total de partidas.


O jogador com os piores resultados é encontrado filtrando as regras para `rhs = "Resultado=0"` na função `apriori` e selecionando os jogadores que estiveram presentes nestas derrotas (LHS = 1).  Fora o `rhs`, os outros parâmetros não foram alterados.


```r
regras_perd <- apriori(dat_v6,
  parameter = list(
    supp = 0.1,
    conf = 0.2,
    minlen = 2,
    #maxlen = 10,
    target = "rules"
  ),
  appearance = list(
    default = "lhs",
    rhs = "Resultado=0"
  )
)
#inspect(regras_perd)
reg0_df <- as(regras_perd,"data.frame")
# reg0_df %>% 
#   arrange(desc(support,confidence)) %>% View()
```

 Inicialmente os resultados são filtrados para as associações com no máximo um jogador, isto foi feito a partir do controle de caracteres de cada string. Por fim, é verificado que os resultados que apresentam os jogadores presentes (LHS=1) estão nas últimas sete linhas.


```r
reg0_df %>% 
   # Filtra LHS com no máximo um jogador
  filter(str_length(rules) < 32) %>%
  arrange(desc(confidence)) %>% 
  filter(row_number() >= (n() - 6)) 
```

```
##                            rules   support confidence  coverage      lift count
## 1    {Carlos=0} => {Resultado=0} 0.3178808  0.4403670 0.7218543 0.9778737    48
## 2    {Alonso=0} => {Resultado=0} 0.4039735  0.4357143 0.9271523 0.9675420    61
## 3     {Lúcia=0} => {Resultado=0} 0.4172185  0.4344828 0.9602649 0.9648073    63
## 4     {Paula=1} => {Resultado=0} 0.1788079  0.4285714 0.4172185 0.9516807    27
## 5      {Juan=1} => {Resultado=0} 0.1788079  0.4218750 0.4238411 0.9368107    27
## 6  {Fernando=0} => {Resultado=0} 0.3178808  0.4033613 0.7880795 0.8956995    48
## 7 {Alejandro=1} => {Resultado=0} 0.1456954  0.3384615 0.4304636 0.7515837    22
```

Conforme mostrado na tabela o jogador Carlos apresentou o pior resultado, sendo que obteve derrota em `\(44\%\)` das partidas que participou e esteve presente em aproximadamente `\(20\%\)` do total de partidas.

