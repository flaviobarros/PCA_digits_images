## Carregando as bibliotecas necessárias
library(jpeg)

## Lendo a imagem e determiando o número de linhas e colunas
cat <- readJPEG('cat.jpg')
ncol(cat)
nrow(cat)

## Mostrando a imagem
knitr::include_graphics(path = 'cat.jpg')

## Separando as componentes das matrizes rgb
r <- cat[,,1]
g <- cat[,,2]
b <- cat[,,3]

## Cálculo das componentes principais
cat.r.pca <- prcomp(r, center = FALSE)
cat.g.pca <- prcomp(g, center = FALSE)
cat.b.pca <- prcomp(b, center = FALSE)

## Salvando o pca de cada componente do RGB
rgb.pca <- list(cat.r.pca, cat.g.pca, cat.b.pca)

## Juntando os dados
for (i in seq.int(3, round(nrow(cat) - 10), length.out = 10)) {
  pca.img <- sapply(rgb.pca, function(j) {
    compressed.img <- j$x[,1:i] %*% t(j$rotation[,1:i])
  }, simplify = 'array')
  writeJPEG(pca.img, paste('compressed/cat_compressed_', round(i,0), '_components.jpg', sep = ''))
}

## Plotando as imagens
knitr::include_graphics(path = 'compressed/cat_compressed_3_components.jpg')
knitr::include_graphics(path = 'compressed/cat_compressed_122_components.jpg')
knitr::include_graphics(path = 'compressed/cat_compressed_281_components.jpg')
knitr::include_graphics(path = 'compressed/cat_compressed_321_components.jpg')

## Obtendo o nível de compressão
original <- file.info('cat.jpg')$size / 1000
imgs <- dir('compressed/')

for (i in imgs) {
  full.path <- paste('compressed/', i, sep='')
  print(paste(i, ' size: ', file.info(full.path)$size / 1000, ' original: ', original, ' % diff: ', round((file.info(full.path)$size / 1000 - original) / original, 2) * 100, '%', sep = ''))
}