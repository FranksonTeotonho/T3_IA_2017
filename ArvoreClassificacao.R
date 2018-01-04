library("rpart")
library("rpart.plot")

dados <- read.table("Wine_Data_Set_adaptado.csv",header = TRUE, sep = ",")

exemplos_separados <- c(sample(1:48,12), sample(49:96,12), sample(97:144, 12))
dados_teste <-  dados[exemplos_separados,] 

dados_treinamento <-  dados[-exemplos_separados, ] 

arvore_classificacao  <- rpart(Classe ~ Alcohol + Malic_acid + Ash + Alcalinity_of_ash + Magnesium + Total_phenols + Flavanoids + Nonflavanoid_phenols + Proanthocyanins + Color_intensity + Hue + OD280_OD315_of_diluted_wines + Proline ,
                               data = dados_treinamento,
                               method = "class", 
                               control = rpart.control(minsplit = 1),
                               parms = list(split = "Information"))

plot_arvore_classificacao <- rpart.plot(arvore_classificacao, type = 3)

y_estimado <- predict(arvore_classificacao, dados_teste, "class")


Matriz_confusao <- table(dados_teste$Classe, y_estimado)

tp_tf = c()
fp_fn = c()

nMatriz <- nrow(Matriz_confusao)
print(nMatriz)
for (i in 1:nMatriz){   # para cada classe
  tp_tf = c(tp_tf ,Matriz_confusao[i,i])
  
  predicoes_erradas <- 0
  
  for (j in 1:nMatriz) {
    if (j != i) {
      predicoes_erradas <- predicoes_erradas + Matriz_confusao[i, j]
    }
  }
  fp_fn = c(fp_fn, predicoes_erradas)
}

acc <- sum(tp_tf) / (sum(tp_tf) + sum(fp_fn))
#sum(fp_fn)/2 pode ser tanto o valor de fp como de fn
precision <- sum(tp_tf) / (sum(tp_tf) + sum(fp_fn)/2)
recall <- sum(tp_tf) / (sum(tp_tf) + sum(fp_fn)/2)
f1_score <- 2 * ((precision * recall) / (precision + recall))

print(Matriz_confusao)
print(paste0("Acuracia: ", acc))
print(paste0("Precisao: ", precision))
print(paste0("Revocacao: ", recall))
print(paste0("F measure: ", f1_score))

