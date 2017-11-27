## Leitura dos conjuntos de dados de treino e de teste
treino = read.csv('train.csv', header = T)
teste = read.csv('test.csv', header = T)

############ Explorando as imagens ###############################
## Contando o número de imagens por dígito
barplot(table(treino$label), ylim = c(0,5000))

## Transformando em uma matriz
treino <- as.matrix(treino)

## Imprimindo uma imagem
matriz_imagem <- matrix(treino[1000,-1], ncol = 28)
matriz_imagem <- matriz_imagem[,28:1] ## invertendo a imagem
image(1:28, 1:28, matriz_imagem, col = c('white', 'black'))

## Plotando uma imagem m?dia para cada d?gito
## Definindo uma escala de cor, indo do branco ao preto
colors <- c('white','black')
cus_col <- colorRampPalette(colors=colors)

## Plot de cada imagem m?dia
## Divindo a tela
par(mfrow=c(4,3),pty='s',mar=c(1,1,1,1),xaxt='n',yaxt='n')

## Criando um array para armazenar as matrizes de cada imagem m?dia
all_img <- array(dim=c(10,28*28))

## Recuperando todas as imagens por dígito e calculando a média
for(di in 0:9) {
  print(di)
  all_img[di+1,] <- apply(treino[treino[,1]==di,-1],2,sum)
  all_img[di+1,] <- all_img[di+1,]/max(all_img[di+1,])*255
  
  z<-array(all_img[di+1,],dim=c(28,28))
  z<-z[,28:1] ##right side up
  image(1:28,1:28,z,main=di,col=cus_col(256))
}
par(mfrow = c(1,1))

############ APlicação do PCA ####################################
## Obtendo componentes principais
pc <- prcomp(treino[,-1])
treino_pc <- pc$x

## Obtendo as vari?ncias acumuladas
vars = pc$sdev^2
props = vars/sum(vars)
varAcum = cumsum(props)
which.min(varAcum < 0.95)

## Aplicando a rota??o nos dados de teste
teste_pc <- predict(pc, newdata = teste)