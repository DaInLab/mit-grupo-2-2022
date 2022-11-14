# Instalando biblioteca readxl para importar para o R arquivos com extensão .xls ou .xlsx.
if (!("readxl") %in% installed.packages()) install.packages("readxl")

# Carregando a biblioteca
library(readxl)

# Importando o arquivo do exercício 1
df.ex1 <- read_excel("./dados/exercicio1.xls",skip = 1, col_names = "tx.juros", col_types = c("numeric"))
#view(df.ex1)
# calculando a media
ex1.media <- mean(df.ex1$tx.juros)
print(paste("Média das taxas de juros:", ex1.media))

# calculando a mediana
ex1.mediana = median(df.ex1$tx.juros)
print(paste("Médiana das taxas de juros:", ex1.mediana))

# calculando o desvio padrão
ex1_desviop = round(sd(df.ex1$tx.juros),6)
print(paste("Desvio Padrão das taxas de juros:", ex1_desviop))

# calculando a variância
ex1_varianca <- round(var(df.ex1$tx.juros),6)
print(paste("Variância das taxas de juros:", ex1_varianca))

# valor mínimo
ex1.minimo <- min(df.ex1$tx.juros)
print(paste("Valor mínimo das taxas de juros:", ex1.minimo))

# valor máximo
ex1.maximo <- max(df.ex1$tx.juros)
print(paste("Valor máximo das taxas de juros:", ex1.maximo))

# calculando os quartis
ex1.quartis <- round(quantile(df.ex1$tx.juros),2)
q1 <- ex1.quartis[2]
print(paste("Q1:", q1))

q3 <- ex1.quartis[4]
print(paste("Q3:", q3))

# O melhor gráfico para representar os valores é o do "boxplot"!
boxplot(df.ex1)

# Uma versão mais "sofisticada"!
boxplot(df.ex1$tx.juros,
        main = "Taxas de Juros Recebidas em Ações",
        xlab = "Taxas de Juros",
        ylab = "Ações",
        col = "orange",
        border = "brown",
        horizontal = TRUE,
        notch = T
)

# Uma versão dois, com retas indicando os pontos no gráfico
# get quartile in r code (single line)
media = round(mean(df.ex1$tx.juros),2)
print(media)
mediana = round(median(df.ex1$tx.juros),2)
print(mediana)
menorv = round(min(df.ex1$tx.juros),2)
print(menorv)
maiorv = round(max(df.ex1$tx.juros),2)
print(maiorv)

quartiz = round(quantile(df.ex1$tx.juros, prob=c(.25,.5,.75)),2)
quartiz
# 25%  50%  75% 
#2.59 2.61 2.63 

round(summary(df.ex1$tx.juros),2)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#2.50    2.59    2.61    2.60    2.63    2.64 

boxplot(df.ex1$tx.juros)

abline(a = media, b = 0L, h = NULL, v = T)
abline(a = mediana, b = 0L, h = NULL, v = T)
abline(a = menorv, b = 0L, h = NULL, v = T)
abline(a = maiorv, b = 0L, h = NULL, v = T)
abline(a = quartiz[1], b = 0L, h = NULL, v = T)
abline(a = quartiz[3], b = 0L, h = NULL, v = T)


# Experiências ....

#Outro gráfico
plot(df.ex1$tx.juros, main = "Taxas de Juros Recebidas em Ações",
     xlab = "Número da Ação",
     ylab = "Taxa de Juros")
barplot(table(df.ex1$tx.juros))

plot(df.ex1$tx.juros)
hist(df.ex1$tx.juros)
lines(df.ex1$tx.juros)

stripchart(df.ex1$tx.juros, vertical = T)
