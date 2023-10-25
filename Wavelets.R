library(wavelets)
library(ggplot2)
library("harbinger")
data(har_examples)

data <- har_examples$example6$serie # Tratar exemplos 3 e 6 eliminando "anomaly_wavelet" nas primeiras e últimas posições
plot(data, main = "Dados originais", type="l")

# Realizando a Transformada Wavelet Discreta
wt <- dwt(data, filter="la8", n.levels=2) # "haar", "d4", "la8", "bl14", "c6"

# Analisando os coeficientes Wavelet
plot(wt@W$W1, main = "Coeficientes Wavelet (W1)")

print(paste("Dica de limiar:", max(abs(wt@W$W1))))

threshold <- 0.3
anomaly_wavelet <- which(abs(wt@W$W1) > threshold)

print(paste("anomalia Wavelet (coeficientes Wavelet)=", anomaly_wavelet))
# W1 tem metade dos elementos que a série temporal original, desta forma, para representar corretamente no gráfico,
# devemos multiplicar os índices de anomalia por 2
anomaly <- (anomaly_wavelet * length(data) / length(wt@W$W1)) - 4
anomaly <-  anomaly[anomaly > 0]
print(paste("anomalia ajustada=", anomaly))

t <- 1:length(data)
ggplot() + 
  geom_line(aes(x=t, y=data), color="blue") +
  geom_point(aes(x=t[anomaly], y=data[anomaly]), color="red", size=3) +
  labs(title="Detecção de Anomalias com Transformada Wavelet & Coeficientes Wavelet (W1)", x="Tempo", y="Valor") +
  theme_minimal()










# Analisando os coeficientes de escala
plot(wt@V$V1, main = "Coeficientes de escala (V1)")

# Identificando anomalias a partir dos coeficientes de escala
threshold_high <- 1.7
threshold_low  <- -1.7
anomaly_scale <- which(wt@V$V1 > threshold_high | wt@W$W1 < threshold_low)
print(paste("anomalia Wavelet (coeficientes de escala)=", anomaly_scale))
# V1 tem metade dos elementos que a série temporal original, desta forma, para representar corretamente no gráfico,
# devemos multiplicar os índices de anomalia por 2
anomaly <- (anomaly_scale * length(data) / length(wt@V$V1)) - 4
print(paste("anomalia ajustada=", anomaly))

t <- 1:length(data)
ggplot() + 
  geom_line(aes(x=t, y=data), color="blue") +
  geom_point(aes(x=t[anomaly], y=data[anomaly]), color="red", size=3) +
  labs(title="Detecção de Anomalias com Transformada Wavelet & Coeficientes de escala (V1)", x="Tempo", y="Valor") +
  theme_minimal()
