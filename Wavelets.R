library(wavelets)
library(ggplot2)

set.seed(123)
n <- 550
t <- 1:n
data <- sin(2*pi*t/50) + rnorm(n)*0.5
data[240:260] <- data[240:260] + 5  # Adiciona uma anomalia
plot(data)

wt <- dwt(data, filter="la8", n.levels=2) # "haar", "d4", "la8", "bl14", "c6"


threshold <- 3.5
anomaly <- which(abs(wt@V$V1) > threshold)
print(paste("anomalia=", anomaly))
# V1 tem metade dos elementos que a série temporal original, desta forma, para representar corretamente no gráfico,
# devemos multiplicar os índices de anomalia por 2
anomaly <- (anomaly-1) * 2
print(paste("anomalia ajustada=", anomaly))


ggplot() + 
  geom_line(aes(x=t, y=data), color="blue") +
  geom_point(aes(x=t[anomaly], y=data[anomaly]), color="red", size=3) +
  labs(title="Detecção de Anomalias com Transformada Wavelet", x="Tempo", y="Valor") +
  theme_minimal()

boxplot_obj <- boxplot(wt@W$W1)
print(boxplot_obj$out)
