---
title: "Mineração de dados - regra de associação apriori"
date: "2022-06-21"
excerpt: Aplicação do algoritmo apriori
tags:
- Regras de associação
categories:
- Mineração de dados
links:
- icon: dropbox
  icon_pack: fab
  name: dados
  url: https://www.dropbox.com/sh/2eqreyru6qq076g/AADvfI7XRGKShpxekJUC9gWya?dl=0
---



## Mineração de dados

Inicialmente são carregadas/instaladas as bibliotecas que serão usadas.


```r
pacotes <- c(
  "arules",
  "tidyverse",
  "arulesViz",
  "skimr",
  "readr",
  "here",
  "readr",
  "here"
)

for(pckgs in pacotes){
  if(!require(pckgs,character.only = TRUE)) install.packages(pckgs)
  library(pckgs,character.only = TRUE)
}
```

## Descrição dos dados


Os dados disponibilizados descrevem resultados de múltiplas partidas com informações dos jogadores presentes. O objetivo é extrair informações  como os melhores/piores jogadores aplicando o algoritmo apriori. 



## Pré-processamento dos dados

Os dados são importados e as células sem informação são tratadas como `NA`.







```r
# Importa o arquivo csv e trata as linhas em branco como NA
fpath <- here(paste0(mpath,"_ASSOC_PadelStars.csv"))
dat <- read.csv(fpath) %>%
  na_if("")
# View(dat) # Visualizar o formato dos dados

print(dat)
```

```
##     Partida                Jogadore.a.s Resultado  X Jogadore.a.s.1
## 1         1   Carlos, alejandro, Alonso    Perdeu NA      Alejandro
## 2         2     Alonso, alejandro, Juan    Perdeu NA         Ariana
## 3         3     Juan, Carlos, alejandro    GANHOU NA         Carlos
## 4         4      Juan, alejandro, Paula    GANHOU NA       Fernando
## 5         5        Paula, Ariana, Lucia    Perdeu NA           Juan
## 6         6 Fernando, Carlos, alejandro    GANHOU NA          Lucia
## 7         7        Juan, Paula, Ariana     GANHOU NA          Paula
## 8         8     Fernando, Carlos, Lucia    Perdeu NA           <NA>
## 9         9    alejandro, Paula, Carlos    GANHOU NA           <NA>
## 10       10     Juan, alejandro, Carlos    Perdeu NA           <NA>
## 11       11     Fernando, Lucia, Ariana    Perdeu NA           <NA>
## 12       12         Juan, Carlos, Paula    GANHOU NA           <NA>
## 13       13     Juan , alejandro, Lucia    Perdeu NA           <NA>
## 14       14            alejandro, Paula    GANHOU NA           <NA>
## 15       15             Juan, alejandro    GANHOU NA           <NA>
## 16       16    alejandro, Paula, Alonso    Perdeu NA           <NA>
## 17       17     Juan, alejandro, Ariana    GANHOU NA           <NA>
## 18       18         Juan, Carlos, Lúcia    GANHOU NA           <NA>
## 19       19        Lucia, Paula, Carlos    Perdeu NA           <NA>
## 20       20             Juan, alejandro    GANHOU NA           <NA>
## 21       21     Fernando, Carlos, Lúcia    Perdeu NA           <NA>
## 22       22     Lucia, Paula, alejandro    Perdeu NA           <NA>
## 23       23     Juan, alejandro, Carlos    Perdeu NA           <NA>
## 24       24    alejandro, Paula, Alonso    GANHOU NA           <NA>
## 25       25     Juan, alejandro, Ariana    GANHOU NA           <NA>
## 26       26         Juan, Carlos, Lúcia    Perdeu NA           <NA>
## 27       27        Lúcia, Paula, Carlos    Perdeu NA           <NA>
## 28       28                 Juan, Paula    GANHOU NA           <NA>
## 29       29             Fernando, Lúcia    Perdeu NA           <NA>
## 30       30     Lucia, Paula, alejandro    GANHOU NA           <NA>
## 31       31         Juan, Carlos, Paula    Perdeu NA           <NA>
## 32       32    Ariana*, Juan, alejandro    GANHOU NA           <NA>
## 33       33         Paula, Juan, Ariana    GANHOU NA           <NA>
## 34       34    Ariana, alejandro, Paula    GANHOU NA           <NA>
## 35       35    Ariana, Paula, alejandro    GANHOU NA           <NA>
## 36       36        Paula, Ariana, Lucia    GANHOU NA           <NA>
## 37       37   Ariana, Carlos, alejandro    GANHOU NA           <NA>
## 38       38    Lucia, alejandro, Carlos    Perdeu NA           <NA>
## 39       39    Paula, Ariana, alejandro    GANHOU NA           <NA>
## 40       40         Juan, Paula, Carlos    GANHOU NA           <NA>
## 41       41    Paula, Ariana, alejandro    GANHOU NA           <NA>
## 42       42        Lucia, Paula, Carlos    Perdeu NA           <NA>
## 43       43    Paula, Ariana, alejandro    Perdeu NA           <NA>
## 44       44       Juan, Fernando, Lucia    Perdeu NA           <NA>
## 45       45     Juan, ALEJANDRO, Ariana    Perdeu NA           <NA>
## 46       46     Fernando, Lucia, Ariana    GANHOU NA           <NA>
## 47       47                 Juan, Paula    GANHOU NA           <NA>
## 48       48                 Juan, Paula    Perdeu NA           <NA>
## 49       49      Juan, Fernando, Ariana    Perdeu NA           <NA>
## 50       50     Fernando, Carlos, Lucia    GANHOU NA           <NA>
## 51       51    alejandro, Paula, Carlos    Perdeu NA           <NA>
## 52       52     Juan, alejandro, Carlos    GANHOU NA           <NA>
## 53       53     Fernando, Lucia, Ariana    Perdeu NA           <NA>
## 54       54      Juan, Carlos, Ariana      GANHOU NA           <NA>
## 55       55      Juan, alejandro, Lucia    GANHOU NA           <NA>
## 56       56            Fernando, Carlos    Perdeu NA           <NA>
## 57       57     Lucia, Paula, alejandro    GANHOU NA           <NA>
## 58       58        Juan, Carlos, Ariana    GANHOU NA           <NA>
## 59       59              Juan, Fernando    Perdeu NA           <NA>
## 60       60            Fernando, Carlos    Perdeu NA           <NA>
## 61       61           alejandro*, Paula    GANHOU NA           <NA>
## 62       62             Juan, alejandro    Perdeu NA           <NA>
## 63       63            Fernando, Carlos    GANHOU NA           <NA>
## 64       64                Ariana, Juan    GANHOU NA           <NA>
## 65       65                 Lucia, Juan    Perdeu NA           <NA>
## 66       66           Ariana, alejandro    Perdeu NA           <NA>
## 67       67       Ariana, Paula, Alonso    Perdeu NA           <NA>
## 68       68               Paula, Ariana    Perdeu NA           <NA>
## 69       69              Ariana, Carlos    Perdeu NA           <NA>
## 70       70     Ariana, Juan, alejandro    GANHOU NA           <NA>
## 71       71         Lucia, Juan, Ariana    GANHOU NA           <NA>
## 72       72    Ariana, alejandro, Paula    GANHOU NA           <NA>
## 73       73    Ariana, Paula, alejandro    Perdeu NA           <NA>
## 74       74      Paula, Ariana  , Lúcia    Perdeu NA           <NA>
## 75       75   Ariana, Carlos, alejandro    GANHOU NA           <NA>
## 76       76    Lucia, alejandro, Carlos    GANHOU NA           <NA>
## 77       77    Paula, Ariana, alejandro    GANHOU NA           <NA>
## 78       78         Juan, Paula, Carlos    Perdeu NA           <NA>
## 79       79    Paula, Ariana, alejandro    Perdeu NA           <NA>
## 80       80           Juan, alejandro      GANHOU NA           <NA>
## 81       81            Fernando, Carlos    GANHOU NA           <NA>
## 82       82                Ariana, Juan    GANHOU NA           <NA>
## 83       83                 Lucia, Juan    GANHOU NA           <NA>
## 84       84           Ariana, alejandro    GANHOU NA           <NA>
## 85       85               Ariana, Paula    GANHOU NA           <NA>
## 86       86               Paula, Ariana    GANHOU NA           <NA>
## 87       87              Ariana, Carlos    GANHOU NA           <NA>
## 88       88             Juan, alejandro    GANHOU NA           <NA>
## 89       89              Paula, Ariana*    GANHOU NA           <NA>
## 90       90            Fernando, Carlos    Perdeu NA           <NA>
## 91       91        Ariana, Juan, Alonso    Perdeu NA           <NA>
## 92       92                Lucia, Juan     Perdeu NA           <NA>
## 93       93           Ariana, alejandro    Perdeu NA           <NA>
## 94       94               Ariana, Paula    Perdeu NA           <NA>
## 95       95               Paula, Ariana    Perdeu NA           <NA>
## 96       96              Ariana, Carlos    GANHOU NA           <NA>
## 97       97            Lucia, alejandro    GANHOU NA           <NA>
## 98       98               Paula, Ariana    GANHOU NA           <NA>
## 99       99                 Juan, Paula    Perdeu NA           <NA>
## 100     100               Paula, Ariana    GANHOU NA           <NA>
## 101     101                Lucia, Paula    GANHOU NA           <NA>
## 102     102               Paula, Ariana    Perdeu NA           <NA>
## 103     103              Juan, Fernando    Perdeu NA           <NA>
## 104     104            Juan, alejandro     GANHOU NA           <NA>
## 105     105             Fernando, Lucia    GANHOU NA           <NA>
## 106     106               Juan, Carlos     Perdeu NA           <NA>
## 107     107             Juan, alejandro    GANHOU NA           <NA>
## 108     108            alejandro, Paula    GANHOU NA           <NA>
## 109     109             Juan, alejandro    GANHOU NA           <NA>
## 110     110               Paula, Ariana    Perdeu NA           <NA>
## 111     111            Fernando, Carlos    GANHOU NA           <NA>
## 112     112        Ariana, Juan, Alonso    GANHOU NA           <NA>
## 113     113                 Lucia, Juan    GANHOU NA           <NA>
## 114     114           Ariana, alejandro    Perdeu NA           <NA>
## 115     115               Ariana, Paula    Perdeu NA           <NA>
## 116     116    Paula, Ariana, alejandro    GANHOU NA           <NA>
## 117     117       Ariana, Carlos, Lucia    Perdeu NA           <NA>
## 118     118   Lucia, alejandro., Carlos    GANHOU NA           <NA>
## 119     119     Paula, Ariana, Fernando    Perdeu NA           <NA>
## 120     120       Juam, Paula, Fernando    Perdeu NA           <NA>
## 121     121     Paula, Ariana, Fernando    GANHOU NA           <NA>
## 122     122     Lucia, Paula, alejandro    GANHOU NA           <NA>
## 123     123     Paula, Ariana, Fernando    GANHOU NA           <NA>
## 124     124       Juam, Fernando,Ariana    GANHOU NA           <NA>
## 125     125             Juan, alejandro    Perdeu NA           <NA>
## 126     126             Fernando, Lucia    Perdeu NA           <NA>
## 127     127                Juam, Carlos    GANHOU NA           <NA>
## 128     128           Juam, alejandro      Perdeu NA           <NA>
## 129     129            alejandro, Paula    Perdeu NA           <NA>
## 130     130             Juan, alejandro    GANHOU NA           <NA>
## 131     131              Paula, Ariana.    GANHOU NA           <NA>
## 132     132   Fernando, Carlos, Alonso.    Perdeu NA           <NA>
## 133     133               Ariana, Juan.    Perdeu NA           <NA>
## 134     134                Lucia, Juan.    GANHOU NA           <NA>
## 135     135         Ariana, alejandro      Perdeu NA           <NA>
## 136     136              Ariana, Paula.    Perdeu NA           <NA>
## 137     137              Paula, Ariana.    GANHOU NA           <NA>
## 138     138             Ariana, Carlos.    GANHOU NA           <NA>
## 139     139           Lucia, alejandro.    GANHOU NA           <NA>
## 140     140     Paula, Ariana, Fernando    GANHOU NA           <NA>
## 141     141     Lucia, Paula, alejandro    GANHOU NA           <NA>
## 142     142     Paula, Ariana, Fernando    Perdeu NA           <NA>
## 143     143      Juan, Fernando,Ariana.    Perdeu NA           <NA>
## 144     144   Juam, alejandro., Alonso.    GANHOU NA           <NA>
## 145     145             Fernando, Lucia    Perdeu NA           <NA>
## 146     146             Juan, alejandro    GANHOU NA           <NA>
## 147     147             Fernando, Lucia    Perdeu NA           <NA>
## 148     148        Juan, Carlos, Alonso    GANHOU NA           <NA>
## 149     149    Juan, alejandro, Alonso.    Perdeu NA           <NA>
## 150     150           alejandro, Paula.    GANHOU NA           <NA>
## 151     151               Juan, Ariana.    Perdeu NA           <NA>
```

As colunas dos jogadores foram renomeadas e a coluna que contém nomes de mais de um jogador foram separadas em três variáveis.


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




Há nomes de jogadores repetidos (Alejandro, Carlos e Paula) em algumas das sete primeiras linhas. 



```
##   Partida Jogador_1  Jogador_2  Jogador_3 Resultado  X Jogador_4
## 1       1    Carlos  alejandro     Alonso    Perdeu NA Alejandro
## 2       2    Alonso  alejandro       Juan    Perdeu NA    Ariana
## 3       3      Juan     Carlos  alejandro    GANHOU NA    Carlos
## 4       4      Juan  alejandro      Paula    GANHOU NA  Fernando
## 5       5     Paula     Ariana      Lucia    Perdeu NA      Juan
## 6       6  Fernando     Carlos  alejandro    GANHOU NA     Lucia
## 7       7      Juan      Paula    Ariana     GANHOU NA     Paula
```


Assim, considerando que não há mais de um jogador com o mesmo nome as células com nomes repetidos foram transformadas em `NA`.



```r
# Adiciona NA nos nomes duplicados
dat_v2$Jogador_4[1] <- NA # Alejandro
dat_v2$Jogador_4[3] <- NA # Carlos
dat_v2$Jogador_4[7] <- NA # Paula
```

Os nomes com caracteres diferentes do esperado são corrigidos e as células sem informação substituídas por `NA`.


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
      mutate_all(na_if, "") # subs esp. em branco por NA
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

As funções para o tratamento dos dados são usadas e são aplicadas as  
mudanças finais da coluna resultado como a transformação de "Ganhou" para 1 e "Perdeu" para 0, além da transformação das variáveis para a classe fator, necessário para aplicação do algoritmo de mineração usado.


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


Os critérios usados foram o suporte de `\(10\%\)` e confiança mínima de `\(60\%\)`. Inicialmente, há o interesse de descobrir combinações de jogadores resultaram em vitória. Isto foi feito definindo o lado `rhs = "Resultado=1",` (vitórias) em um dos parâmetros da função `apriori`.


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

Os resultados são filtrados para os jogadores que estavam presentes na partida (LHS = 1). Para melhorar a visualização as regras são transformadas em um quadro de dados ordenados pelos maiores valores de lift.


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

O jogador Alejandro obteve a vitória em `\(66\%\)` das partidas que participou, tendo participado em cerca de `\(28\%\)` do total de partidas. A melhor dupla é formada por  Paula e Alejandro, que obtiveram a vitória em `\(72\%\)` das partidas que participaram e estiveram presentes em cerca de `\(12\%\)` do total de partidas.


Os jogadore com os piores resultados é encontrado filtrando as regras para `rhs = "Resultado=0"` na função `apriori` e selecionando os jogadores que estiveram presentes nestas derrotas (LHS = 1).  Fora o `rhs`, os outros parâmetros não foram alterados.


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

 Os resultados são filtrados para as associações com no máximo um jogador, isto foi feito a partir do controle de caracteres de cada string. Por fim, é verificado que os resultados que apresentam os jogadores presentes (LHS=1) estão nas últimas sete linhas.


```r
reg0_df %>% 
   # Filtra LHS com no máximo um jogador
  filter(str_length(rules) < 32) %>%
  arrange(desc(support),desc(confidence)) %>% 
  filter(row_number() >= (n() - 6)) 
```

```
##                            rules   support confidence  coverage      lift count
## 1    {Ariana=1} => {Resultado=0} 0.1986755  0.4411765 0.4503311 0.9796713    30
## 2     {Paula=1} => {Resultado=0} 0.1788079  0.4285714 0.4172185 0.9516807    27
## 3      {Juan=1} => {Resultado=0} 0.1788079  0.4218750 0.4238411 0.9368107    27
## 4 {Alejandro=1} => {Resultado=0} 0.1456954  0.3384615 0.4304636 0.7515837    22
## 5  {Fernando=1} => {Resultado=0} 0.1324503  0.6250000 0.2119205 1.3878676    20
## 6    {Carlos=1} => {Resultado=0} 0.1324503  0.4761905 0.2781457 1.0574230    20
## 7     {Lucia=1} => {Resultado=0} 0.1059603  0.4571429 0.2317881 1.0151261    16
```


