library("rpart")
library("rpart.plot")

dados <- read.table("Concrete_Slump_adaptado.csv",header = TRUE, sep = ",")

exemplos_separados <- c(sample(1:33,8), sample(34:66,8), sample(66:100, 9))
dados_teste <-  dados[exemplos_separados,] 

dados_treinamento <-  dados[-exemplos_separados, ] 

arvore_regressao  <- rpart(FLOW_cm ~ Cement + Slag + Fly_ash + Water + SP + Coarse_Aggr + Fine_Aggr + SLUMP_cm + Compressive_Strength_Mpa ,
                               data = dados_treinamento,
                               method = "anova", 
                               control = rpart.control(minsplit = 4),
                               parms = list(split = "Information"))

plot_arvore_regressao <- rpart.plot(arvore_regressao, type = 3)

y_estimado <- predict(arvore_regressao, dados_teste, "vector")

nPreditos <- length(y_estimado)

somatorio <- 0

for(i in 1:nPreditos)
  somatorio <- somatorio + (y_estimado[i] - dados_teste$FLOW_cm[i])

somaQuadratica <- somatorio * somatorio
ErroQuadratico <- (1/nPreditos) * (somaQuadratica) 

print(paste0("Erro Medio Quadratico: ", ErroQuadratico))

