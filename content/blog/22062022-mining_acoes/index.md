---
title: "Análise de ações do mercado brasileiro usando Apriori e clustering"
author: "jrds"
date: "2022-06-21"
excerpt: Análise de ações usando algoritmo apriori e clustering
tags:
- Regras de associação
- R
- Clustering
categories:
- Mineração de dados
links:
- icon: dropbox
  icon_pack: fab
  name: dados
  url: https://www.dropbox.com/sh/akx1mjachn2zow9/AACrA5OzTd2k68cp8TeaEvGga?dl=0
---

## Análise de ações do mercado brasileiro usando Apriori e clustering

Instalação/carregamento das bibliotecas necessárias.

``` r
pacotes <- c(
  "arules",
  "tidyverse",
  "arulesViz",
  "stringr",
  "factoextra",
  "lubridate",
  "timetk",
  "clValid",
  "knitr",
  "tidyquant",
  "NbClust",
  "here",
  "psych",
  "NbClust",
  "tidyquant",
  "clValid"
)

for(pckgs in pacotes){
  if(!require(pckgs,character.only = TRUE)) install.packages(pckgs)
  library(pckgs,character.only = TRUE)
}
```

## Pré-processamento dos dados

``` r
# Local do arquivo csv
closePrice_path <- here(paste0(mpath,"2020closePrice SAX-240_60_10.csv"))

# Leitura dos dados
dat <- read.csv(closePrice_path) %>%
  select(-c(X, X45))


# Leitura do arquivo com o nome das colunas
acoes_path <- here("content/blog/22062022-mining_acoes/data/TopAcoes.txt")
dat.cols <- read.table(
  acoes_path,
  header = FALSE,
  sep = "\t",
  fileEncoding = "latin1"
)

# ncol(dat.cols)
# ncol(dat)
```

As colunas do quadro de dado `dat` são renomeadas com os nomes das empresas presentes em `dat.cols`. As células com informações de ações que tiveram fechamento abaixo e acima da média são substituídas por zero e um respectivamente.

``` r
# Transforma a primeira linha em um vetor de caracteres
dat.cols.v2 <- as.character(as.vector(dat.cols[1,]))

# Redefine o nome das colunas do quadro de dados dat usando o dat.cols.v2
names(dat) <- dat.cols.v2

# View(dat) # Visualizar o formato dos dados

# Valores com ações abaixo da média
abaixo.media <- c("a", "b", "c", "d", "e")

# Substitui as ações abaixo da média por 0 e acima da média por 1 
dat.v2 <- dat %>%
  mutate(across(
    .cols = everything(),
    .fns = ~ if_else(. %in% abaixo.media, 0, 1)
  ))

# Transformação das colunas para fator para uso do algoritmo  apriori
dat.v3 <- dat.v2 %>%
  mutate(across(everything(), .fns = as.factor))
# str(dat.v3)

# Salva com os dados tratados
#write.csv(dat.v3,'dados-limpos.csv')
```

## Aplicação do algoritmo Apriori para regras relativas as ações da Petrobrás

Seleciona as regras na qual o preço de fechamento da ação da petrobrás estando em alta implica outra ação estando em alta.

``` r
# regras.v2 <- apriori(dat.v3,
#   parameter = list(
#     supp = 0.45,
#     conf = 0.6,
#     minlen = 2,
#     maxlen = 5
# ))

# Transforma "rules" em um quadro de dados
#regrasv2.df <- as(regras.v2,"data.frame")

# Seleciona regra no qual a ação da petrobrás em alta implica em outra ação estando em alta
# reg.df.petrobras <- regrasv2.df %>%
#   filter(
#     str_detect(rules,"^(\\{)PETR4.SA=1\\}")
#   )

# write.csv(reg.df.petrobras,'regras-petrobrás.csv')
regras_path <- here("content/blog/22062022-mining_acoes/data/regras-petrobrás.csv")
reg.df.petrobras <- read.csv(regras_path)
```

Mostra três regras que associam a alta do fechamento das ações da petrobrás com outras ações.

``` r
reg.df.petrobras %>% 
  #select(-X) %>% 
  arrange(desc(lift)) %>%
  slice(1:3)    # Três primeiras regras 
```

    ##   X                        rules   support confidence  coverage     lift count
    ## 1 1 {PETR4.SA=1} => {POSI3.SA=1} 0.4833333  1.0000000 0.4833333 2.000000    29
    ## 2 3 {PETR4.SA=1} => {CSNA3.SA=1} 0.4666667  0.9655172 0.4833333 1.810345    28
    ## 3 2 {PETR4.SA=1} => {RAIL3.SA=1} 0.4500000  0.9310345 0.4833333 1.745690    27

As três combinações selecionadas foram selecionadas de acordo com o maior valor da métrica lift, que indica quais relações tem maior associação. A alta do preço de fechamento das ações da petrobrás (PETR4.SA) está relacionada a alta de fechamento das ações da Positivo Tecnologia (POSI3.SA), Companhia Siderúrgica Nacional (CSNA3.SA) e RUMO S.A.(RAIL3.SA).

## Análise de clustering

Os dados lidos e renomeados apresentam os valores de fechamento das ações das principais empresas do mercado brasileiro.

``` r
# Lê os dados com os preços das ações de cada empresa
stpath<- here("content/blog/22062022-mining_acoes/data/2020closePrice_work.csv")
dat.st <- read_csv(stpath,show_col_types = FALSE)

# Nomes das colunas que serão substituídas
old.names<- names(dat.st[,2:length(dat.st)])

# Redefine o nome das colunas do quadro de dados dat.st usando o dat.cols.v2
colnames(dat.st)[colnames(dat.st) %in% old.names] <- dat.cols.v2
# check colnames(dat.st)

# Transforma a coluna para classe "Date"
dat.st$Date <- dmy(dat.st$Date)

# Descrição estatística dos preços de fechamento das ações de cada empresa
describ_dat.st <- dat.st %>% 
  select(-Date) %>% 
  psych::describe()

slice(describ_dat.st,1:5)
```

    ##          vars   n  mean    sd median trimmed   mad   min    max range  skew
    ## ABEV3.SA    1 247 14.07  2.04  13.68   13.82  1.73 11.03  19.21  8.18  0.93
    ## AMER3.SA    2 247 85.36 20.45  83.84   85.17 19.98 42.86 126.00 83.14  0.17
    ## B3SA3.SA    3 247 17.12  2.86  17.75   17.24  2.85 10.15  22.47 12.32 -0.39
    ## BBAS3.SA    4 247 35.33  7.33  33.44   34.54  4.34 22.13  53.80 31.67  1.09
    ## BBDC4.SA    5 247 21.76  4.54  19.97   21.20  2.44 15.41  34.10 18.69  1.14
    ##          kurtosis   se
    ## ABEV3.SA     0.23 0.13
    ## AMER3.SA    -0.80 1.30
    ## B3SA3.SA    -0.82 0.18
    ## BBAS3.SA     0.17 0.47
    ## BBDC4.SA     0.18 0.29

A análise das componentes principais foi usada com a finalidade de estimar quantas variáveis são necessárias para descrever o conjunto dos dados das ações. As variáveis usadas na análise foram estatísticas descritivas como a média e desvio padrão para as ações de cada empresa.

``` r
dat.st.pca <- dat.st %>% 
  select(-Date) %>% 
  broom::tidy() %>% 
  select(-n) %>% 
  column_to_rownames("column")
```

A curva do gráfico a seguir mostra quantas das onze medidas estatísticas são necessárias para explicar a variância no conjunto de dados. Pode-se observar que as primeiras duas variáveis (média e desvio padrão) são suficientes para explicar `\(93.2 \%\)` da variância.

``` r
dat.st.pr <- prcomp(dat.st.pca, 
                  center = TRUE, 
                  scale = TRUE)
summary(dat.st.pr)
```

    ## Importance of components:
    ##                           PC1    PC2     PC3     PC4     PC5     PC6     PC7
    ## Standard deviation     2.9320 1.2838 0.60143 0.54722 0.27849 0.10133 0.07257
    ## Proportion of Variance 0.7815 0.1498 0.03288 0.02722 0.00705 0.00093 0.00048
    ## Cumulative Proportion  0.7815 0.9314 0.96426 0.99148 0.99853 0.99947 0.99995
    ##                            PC8      PC9      PC10      PC11
    ## Standard deviation     0.02430 0.002613 3.263e-16 1.062e-16
    ## Proportion of Variance 0.00005 0.000000 0.000e+00 0.000e+00
    ## Cumulative Proportion  1.00000 1.000000 1.000e+00 1.000e+00

``` r
cumpro <- cumsum(dat.st.pr$sdev ^ 2 / sum(dat.st.pr$sdev ^ 2))
plot(cumpro[0:15],
     xlab = "PC",
     ylab = "Amount of explained variance",
     main = "Cumulative variance plot")
abline(v = 2, col = "blue", lty = 5)
abline(h = 0.932, col = "blue", lty = 5)
legend(
  "topleft",
  legend = c("PC2"),
  col = c("blue"),
  lty = 5,
  cex = 0.6
)
```

<img src="{{< blogdown/postref >}}index_files/figure-html/Redução das várias por PCA-1.png" width="672" />

Extrai as colunas de interesse média e desvio padrão.

``` r
dat.st.estatisticas <- dat.st %>% 
  select(-Date) %>% 
  broom::tidy() %>%  
  select(column,mean,sd) %>% 
  column_to_rownames("column")
head(dat.st.estatisticas)
```

    ##              mean        sd
    ## ABEV3.SA 14.06794  2.044120
    ## AMER3.SA 85.36034 20.452055
    ## B3SA3.SA 17.11538  2.860553
    ## BBAS3.SA 35.33243  7.325316
    ## BBDC4.SA 21.76462  4.539080
    ## BEEF3.SA 11.78888  1.874560

O pacote `NbClust` fornece 30 índices para determinar o número ótimo de clusters. O número escolhido no estudo foi `\(K = 4\)`, no qual 5 dos 30 índices indicaram ser a melhor escolha.

``` r
nbc_scaled <- NbClust(
  dat.st.estatisticas,
  distance = "euclidean",
  min.nc = 2,
  max.nc = 20,
  method = "complete",
  index = "all"
)
```

``` r
#factoextra::fviz_nbclust(nbc_scaled) + 
#  theme_bw() + 
#  ggtitle("NbClust's optimal number of clusters")
```

O agrupamento K-means é um método simples que particiona um conjuntos de dados em K grupos distintos e não sobrepostos. É necessário especificar o número desejados de clusters K e então o algoritmo atribui cada observação a um dos K grupos. De acordo com o resultado anterior o número selecionado no estudo foi `\(K = 4\)` que é especificado na função `kmeans` como mostrado no bloco de código a seguir.

``` r
# Aplica o algoritmo k-means usando k = 4
dat.kmeans <- kmeans(dat.st.estatisticas,centers = 4, nstart = 30)
 
# Salva para que o número inteiro associado a cada cluster não mude
#saveRDS(dat.kmeans, "kmeans.rds")

# Lê modelo
# dat.kmeans <- readRDS("kmeans.rds")

# Visualiza os clusters 
fviz_cluster(dat.kmeans, 
             data = dat.st.estatisticas, 
             palette = c("#2E9FDF", 
                         "#00AFBB", 
                         "#E7B800", 
                         "#FC4E07"),
             show.clust.cent = TRUE, 
             ggtheme = theme_minimal(),
             repel = TRUE)
```

<img src="{{< blogdown/postref >}}index_files/figure-html/Aplicação e visualização dos clusters-1.png" width="672" />

É feito o tratamento dos dados para visualização das ações pertencentes a cada um dos quatro grupos encontrados, foi optado por utilizar as séries temporais das ações de fechamentos mais próximas aos centros de cada grupo.

``` r
# Transforma o quadro de dados em um objeto da classe xts
dat.st.v2 <- tk_xts(dat.st,
                    date_var = Date) 

# Subset das ações representantes de cada cluster
# Respectivos grupos: 2,4, 1 e 3 
dat.st.aux <- dat.st.v2[,c("USIM5.SA","BBDC4.SA","LREN3.SA","BTOW3.SA")]
 
# Arruma os dados para a visualização
dat.st.aux.v2 <- broom::tidy(dat.st.aux)
```

As séries temporais representando o preço de fechamento ao longo do período de `02/01/2020` a `30/12/2020` das empresas Usiminas(USIM5.SA, Grupo 2), Banco Bradesco(BBDC4.SA, Grupo 4), Lojas Renner (LREN3.SA, Grupo 1) e B2W DIGITAL ON (BTOW3.SA, Grupo 3).

``` r
# Visualização das ações de três empresas
dat.st.aux.v2 %>%
    ggplot(aes(x = index, y = value, color = series)) +
    geom_line(size = 0.5, color = tidyquant::palette_light()[[1]]) +
    facet_grid(series~.,scales = "free")+
    #stat_summary(fun.data = "mean_cl_boot", geom = "smooth") +
    labs(title = "Séries temporais ",
         subtitle = " Preço de fechamento das ações do mercado brasileiro",
         caption = "Bolsa de valores brasileira",
         x = "Data", y = "Valores em reais") +
    tidyquant::theme_tq() +
    tidyquant::scale_color_tq() 
```

<img src="{{< blogdown/postref >}}index_files/figure-html/Visualização das séries temporais-1.png" width="672" />

As ações selecionadas de cada cluster apresentaram uma queda no preço de fechamento no período de Janeiro de 2020 a Abril de 2020, mês em que a pandemia teve início no país. O sumário estatístico apresenta a média de cada grupo em que é possível observar a média do preço da ação de cada grupo e desvio padrão que indica o quão próximo os conjuntos de dados estão da média.

``` r
# número de cluster deve ser igual o número de empresas (44)
# sum(table(dat.kmeans$cluster))
dat.grupos <- dat.st.estatisticas %>%
  as_tibble() %>% 
  mutate(Grupo = dat.kmeans$cluster) %>% 
  bind_cols(Ações = names(dat.st.v2))

dat.grupos.summary <- dat.grupos %>%
  group_by(Grupo) %>%
  summarise_at(.vars = vars(mean, sd),
               .funs = "mean")
```

<div id="tcjsovfdmw" style="overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>@import url("https://fonts.googleapis.com/css2?family=Montserrat:ital,wght@0,100;0,200;0,300;0,400;0,500;0,600;0,700;0,800;0,900;1,100;1,200;1,300;1,400;1,500;1,600;1,700;1,800;1,900&display=swap");
html {
  font-family: Montserrat, -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Oxygen, Ubuntu, Cantarell, 'Helvetica Neue', 'Fira Sans', 'Droid Sans', Arial, sans-serif;
}

#tcjsovfdmw .gt_table {
  display: table;
  border-collapse: collapse;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 16px;
  font-weight: 600;
  font-style: italic;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}

#tcjsovfdmw .gt_heading {
  background-color: #FFFFFF;
  text-align: left;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#tcjsovfdmw .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}

#tcjsovfdmw .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 0;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}

#tcjsovfdmw .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#tcjsovfdmw .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#tcjsovfdmw .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: 600;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}

#tcjsovfdmw .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: 600;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}

#tcjsovfdmw .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#tcjsovfdmw .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#tcjsovfdmw .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 5px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}

#tcjsovfdmw .gt_group_heading {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
}

#tcjsovfdmw .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}

#tcjsovfdmw .gt_from_md > :first-child {
  margin-top: 0;
}

#tcjsovfdmw .gt_from_md > :last-child {
  margin-bottom: 0;
}

#tcjsovfdmw .gt_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}

#tcjsovfdmw .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
}

#tcjsovfdmw .gt_stub_row_group {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
  vertical-align: top;
}

#tcjsovfdmw .gt_row_group_first td {
  border-top-width: 2px;
}

#tcjsovfdmw .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#tcjsovfdmw .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}

#tcjsovfdmw .gt_first_summary_row.thick {
  border-top-width: 2px;
}

#tcjsovfdmw .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#tcjsovfdmw .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#tcjsovfdmw .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#tcjsovfdmw .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#tcjsovfdmw .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#tcjsovfdmw .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#tcjsovfdmw .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-left: 4px;
  padding-right: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#tcjsovfdmw .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#tcjsovfdmw .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#tcjsovfdmw .gt_left {
  text-align: left;
}

#tcjsovfdmw .gt_center {
  text-align: center;
}

#tcjsovfdmw .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#tcjsovfdmw .gt_font_normal {
  font-weight: normal;
}

#tcjsovfdmw .gt_font_bold {
  font-weight: bold;
}

#tcjsovfdmw .gt_font_italic {
  font-style: italic;
}

#tcjsovfdmw .gt_super {
  font-size: 65%;
}

#tcjsovfdmw .gt_two_val_uncert {
  display: inline-block;
  line-height: 1em;
  text-align: right;
  font-size: 60%;
  vertical-align: -0.25em;
  margin-left: 0.1em;
}

#tcjsovfdmw .gt_footnote_marks {
  font-style: italic;
  font-weight: normal;
  font-size: 75%;
  vertical-align: 0.4em;
}

#tcjsovfdmw .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}

#tcjsovfdmw .gt_slash_mark {
  font-size: 0.7em;
  line-height: 0.7em;
  vertical-align: 0.15em;
}

#tcjsovfdmw .gt_fraction_numerator {
  font-size: 0.6em;
  line-height: 0.6em;
  vertical-align: 0.45em;
}

#tcjsovfdmw .gt_fraction_denominator {
  font-size: 0.6em;
  line-height: 0.6em;
  vertical-align: -0.05em;
}
</style>
<table class="gt_table" style="table-layout: fixed;">
  <colgroup>
    <col/>
    <col style="width:240px;"/>
    <col style="width:240px;"/>
  </colgroup>
  <thead class="gt_header">
    <tr>
      <th colspan="3" class="gt_heading gt_title gt_font_normal" style>Sumário estatístico dos grupos</th>
    </tr>
    <tr>
      <th colspan="3" class="gt_heading gt_subtitle gt_font_normal gt_bottom_border" style>Algoritmo k-means</th>
    </tr>
  </thead>
  <thead class="gt_col_headings">
    <tr>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1">Grupos</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">Preço de fechamento médio das ações (R$)</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">Desvio padrão do preço de fechamento das ações</th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr><td class="gt_row gt_right gt_stub">1</td>
<td class="gt_row gt_right">21.78</td>
<td class="gt_row gt_right">4.25</td></tr>
    <tr><td class="gt_row gt_right gt_stub">2</td>
<td class="gt_row gt_right">8.95</td>
<td class="gt_row gt_right">2.65</td></tr>
    <tr><td class="gt_row gt_right gt_stub">3</td>
<td class="gt_row gt_right">45.05</td>
<td class="gt_row gt_right">8.11</td></tr>
    <tr><td class="gt_row gt_right gt_stub">4</td>
<td class="gt_row gt_right">85.36</td>
<td class="gt_row gt_right">20.45</td></tr>
  </tbody>
  
  
</table>
</div>

A tabela mostra que o primeiro grupo apresentou a segunda maior média e um valor mais elevado para o desvio padrão. O segundo grupo é caracterizado pelos baixos valores nos preços de fechamento das ações e junto com o grupo quatro possui uma maior uniformidade em relação a média considerando o menor valor de desvio padrão . Por fim, o grupo três possui ações com valores bastante superiores aos preços de fechamento médio dos outros grupos.
