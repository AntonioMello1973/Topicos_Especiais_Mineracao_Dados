# Instale e carregue a biblioteca 'wavelets'
library(wavelets)

# Crie uma série temporal de exemplo (substitua pelo seu próprio conjunto de dados)
set.seed(123)
dados <- ts(rnorm(100), start = 1)

# Realize a transformada de wavelet usando a função wt
transformacao_wavelet <- dwt(dados, filter = "haar")

# Visualize os coeficientes de wavelet
plot(transformacao_wavelet)

# Defina um limiar para identificar anomalias
limiar <- 2.0  # Ajuste o valor do limiar conforme necessário

# Identifique anomalias comparando os coeficientes de wavelet com o limiar
anomalias <- transformacao_wavelet@W > limiar

# Visualize as anomalias detectadas na série temporal original
plot(dados, type = "l", ylim = c(min(dados), max(dados)), xlab = "Tempo", ylab = "Valor", main = "Série Temporal com Anomalias")
points(which(anomalias), dados[anomalias], col = "red", pch = 19)

# Exemplo de como você pode identificar os índices das anomalias
indices_anomalias <- which(anomalias)
print("Índices das Anomalias:")
print(indices_anomalias)
