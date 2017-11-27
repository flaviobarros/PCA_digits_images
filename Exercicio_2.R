######################## Preparação de dados ########################

library(pixmap)

path_treino <- 'treino/'

setwd(path_treino)

files <- dir()

classes <- as.factor(substring(files,first=1,last=1))
treino <- as.data.frame(matrix(rep(0,length(files)*64*64), nrow=length(files)))

for (i in 1:length(files)) {
  x <- read.pnm(files[i])
  treino[i,] <- as.vector(x@grey, mode='integer')
}


path_teste <- '../teste/'

setwd(path_teste)

files <- dir()

predic <- as.factor(substring(files,first=1,last=1))

teste <- as.data.frame(matrix(rep(0,length(files)*64*64), nrow=length(files)))

for (i in 1:length(files)) {
  x <- read.pnm(files[i])
  teste[i,] <- as.vector(x@grey, mode='integer')
}

save(treino, file = 'treino.rda')
save(teste, file = 'teste.rda')

setwd('../')

################# Fim da prepara??o ############################

################# Testes com modelagem nos dados crus ##########
## Teste com k-nn
## Carrega pacote class com o k-nn
library(class)

## Utilizando o k-nn para previs?o do d?gitos nas imagens de teste
predito <- knn(train=treino, test=teste, cl=classes, k=1, prob=T)

## Resultado
result <- data.frame(cbind(predic, predito, acerto = predic==predito))

## C?lculo da taxa de acerto
sum(result$acerto)/nrow(result)

#################################################################
## Teste com ?rvore de decis?o
library(rpart)
arvore <- rpart(formula = classes~., data = treino)
predito <- predict(object = arvore, newdata = teste, type = 'class')

## Resultado
result <- data.frame(cbind(predic, predito, acerto = predic==predito))

## C?lculo da taxa de acerto
sum(result$acerto)/nrow(result)

#################################################################
## Teste com o randomForest
rf <- randomForest(formula = classes~., data = treino)
predito <- predict(object = rf, newdata = teste, type = 'class')

## Resultado
result <- data.frame(cbind(predic, predito, acerto = predic==predito))

## C?lculo da taxa de acerto
sum(result$acerto)/nrow(result)

#################################################################
## Procedimento para todos os impares de 1 a 101 com k-nn

## Data.frame com todos os resultados
resultado <- data.frame(k = rep(0,101), taxa=rep(0.00,101))

for (i in seq(from=1, to=101, by=2)) {
  
  print(i)
  
  predito <- knn(train=treino, test=teste, cl=classes, k=i, prob=T)
  
  result <- data.frame(cbind(predic, predito, acerto = predic==predito))
  
  resultado[i,] <- c(i,sum(result$acerto)/nrow(result))
}

resultado <- subset(resultado, subset=resultado$taxa!=0)

## Imprime gráfico
png(file='valores_k.png')
plot(resultado$taxa~resultado$k, type='o', main='Taxa de Acerto para o k-nn', xlab='Valores de K', ylab='Taxa de acerto')
text(6,0.78, '78%')
text(7,0.62, '62%')
text(12,0.25, '28%')
dev.off()

##################################################################
############ APlicação do PCA ####################################
## Obtendo componentes principais
pc <- prcomp(treino, center = F)
treino_pc <- pc$x

## Obtendo as variâncias acumuladas
vars = pc$sdev^2
props = vars/sum(vars)
varAcum = cumsum(props)
which.min(varAcum < 0.95)

## Aplicando a rotação nos dados de teste
teste_pc <- predict(pc, newdata = teste)

## Salvando treino e teste com PCA
save(treino_pc, file = 'treino_pc.rda')
save(teste_pc, file = 'teste_pc.rda')

##################################################################
## Treinando o knn com as 341 vari?veis
library(class)
predito <- knn(train=treino_pc[,1:544], test=teste_pc[,1:544], cl=classes, k=1, prob=T)

result <- data.frame(cbind(predic, predito, acerto = predic==predito))

sum(result$acerto)/nrow(result)

resultado2 <- data.frame(k = rep(0,1000-55), taxa=rep(0.00,1000-55))

for (i in seq(from=55, to=1000, by=1)) {
  
  print(i)
  
  predito <- knn(train=pc$x[,1:i], test=test.p[,1:i], cl=classes, k=1, prob=T)
  
  result <- data.frame(cbind(predic, predito, acerto = predic==predito))
  
  resultado2[i,] <- c(i,sum(result$acerto)/nrow(result))
}

resultado2 <- resultado2[55:1000,]

## Imprime gr?fico
png(file='valores_dim.png')
plot(resultado2$taxa~resultado2$k, type='o', main='Taxa de Acerto por dimens?es', xlab='Dimens?es', ylab='Taxa de acerto')
text(120,0.84, '84%')
dev.off()

resultado3 <- data.frame(k = rep(0,21), taxa=rep(0.00,21))

for (i in 1:81) {
  
  print(i)
  
  predito <- knn(train=pc$x[,1:70], test=test.p[,1:70], cl=classes, k=i, prob=T)
  
  result <- data.frame(cbind(predic, predito, acerto = predic==predito))
  
  resultado3[i,] <- c(i,sum(result$acerto)/nrow(result))
  
}

## Imprime gr?fico
png(file='valores_k_70.png')
plot(resultado3$taxa~resultado3$k, type='o', main='Taxa de Acerto para o k-nn', xlab='Valores de K', ylab='Taxa de acerto')
dev.off()

#################################################################
## Teste com regressão linear
## Data frame de treino e teste
treino_tratado = as.data.frame(cbind(treino_pc[,1:544]))
treino_tratado$classes = classes

teste_tratado = as.data.frame(teste_pc[,1:544])

## Salvando treino e teste tratados
save(treino_tratado, file = 'treino_tratado.rda')

## Criando uma árvore
reg <- lm(classes ~ ., data = treino_arvore)

## Fazendo a previsão no conjunto de teste
predito <- predict(object = fit, newdata = teste_arvore)
predito <- round(predito, 0)

## Resultado
result <- data.frame(cbind(predic, predito, acerto = predic==predito))

## Cálculo da taxa de acerto
sum(result$acerto)/nrow(result)

#################################################################
## Teste com árvore de decisão
library(rpart)

## Criando uma árvore
arvore <- rpart(classes ~ ., data = treino_arvore)

## Fazendo a previsão no conjunto de teste
predito <- predict(object = arvore, newdata = teste_arvore, type = 'class')

## Resultado
result <- data.frame(cbind(predic, predito, acerto = predic==predito))

## C?lculo da taxa de acerto
sum(result$acerto)/nrow(result)

#################################################################
## Teste com logística multinomial
library(nnet)

## Ajustando um modelo
multinomial <- multinom(classes ~ ., data = treino_arvore, MaxNWts = 5460)

## Fazendo a previsão no conjunto de teste
predito <- predict(object = multinomial, newdata = teste_arvore, type = 'class')

## Resultado
result <- data.frame(cbind(predic, predito, acerto = predic==predito))

## Cálculo da taxa de acerto
sum(result$acerto)/nrow(result)

#################################################################
## Teste com randomForest
library(randomForest)

## Ajustando um modelo
rf <- randomForest(classes ~ ., data = treino_arvore)

## Fazendo a previs?o no conjunto de teste
predito <- predict(object = rf, newdata = teste_arvore, type = 'class')

## Resultado
result <- data.frame(cbind(predic, predito, acerto = predic==predito))

## C?lculo da taxa de acerto
sum(result$acerto)/nrow(result)

#################################################################
## Teste com Boosting
library(gbm)

## Ajustando um modelo
gbmfit <- gbm(classes ~ ., data = treino_arvore, n.trees = 1500)

## Fazendo a previs?o no conjunto de teste
predito <- predict(object = gbmfit, n.trees = 1500, newdata = teste_arvore, type = 'class')

## Resultado
result <- data.frame(cbind(predic, predito, acerto = predic==predito))

## C?lculo da taxa de acerto
sum(result$acerto)/nrow(result)
