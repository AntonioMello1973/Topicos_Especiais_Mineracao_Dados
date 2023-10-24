library(wavelets)
library(ggplot2)
set.seed(123)

# Gerando dados sintéticos
n <- 550
t <- 1:n
data <- sin(2*pi*t/50) + rnorm(n)*0.5
data[241:260] <- data[241:260] + 5  # Adiciona uma anomalia

plot(data, main = "Dados originais")

# Realizando a Transformada Wavelet Discreta
wt <- dwt(data, filter="la8", n.levels=2) # "haar", "d4", "la8", "bl14", "c6"

# Analisando os coeficientes Wavelet
plot(wt@W$W1, main = "Coeficientes Wavelet (W1)")
boxplot_wavelet <- boxplot(wt@W$W1, main = "Coeficientes Wavelet (W1)")
print(boxplot_wavelet$stats)

# Identificando anomalias a partir dos coeficientes Wavelet
threshold_high <- boxplot_wavelet$stats[5]
threshold_low  <- boxplot_wavelet$stats[1]
#anomaly_wavelet <- which(abs(wt@W$W1) > threshold)
anomaly_wavelet <- which(wt@W$W1 > threshold_high | wt@W$W1 < threshold_low)

print(paste("anomalia Wavelet (coeficientes Wavelet)=", anomaly_wavelet))
# W1 tem metade dos elementos que a série temporal original, desta forma, para representar corretamente no gráfico,
# devemos multiplicar os índices de anomalia por 2
anomaly <- (anomaly_wavelet * n / 275) - 3
print(paste("anomalia ajustada=", anomaly))

ggplot() + 
  geom_line(aes(x=t, y=data), color="blue") +
  geom_point(aes(x=t[anomaly], y=data[anomaly]), color="red", size=3) +
  labs(title="Detecção de Anomalias com Transformada Wavelet & Coeficientes Wavelet (W1)", x="Tempo", y="Valor") +
  theme_minimal()


# Analisando os coeficientes de escala
plot(wt@V$V1, main = "Coeficientes de escala (V1)")
boxplot_escala <- boxplot(wt@V$V1, main = "Coeficientes de escala (V1)")
print(boxplot_escala$stats)

# Identificando anomalias a partir dos coeficientes de escala
threshold_high <- boxplot_escala$stats[5]
threshold_low  <- boxplot_escala$stats[1]
anomaly_scale <- which(wt@V$V1 > threshold_high | wt@W$W1 < threshold_low)
print(paste("anomalia Wavelet (coeficientes de escala)=", anomaly_scale))
# V1 tem metade dos elementos que a série temporal original, desta forma, para representar corretamente no gráfico,
# devemos multiplicar os índices de anomalia por 2
anomaly <- (anomaly_scale * length(data) / 275) - 3
print(paste("anomalia ajustada=", anomaly))


ggplot() + 
  geom_line(aes(x=t, y=data), color="blue") +
  geom_point(aes(x=t[anomaly], y=data[anomaly]), color="red", size=3) +
  labs(title="Detecção de Anomalias com Transformada Wavelet & Coeficientes de escala (V1)", x="Tempo", y="Valor") +
  theme_minimal()
