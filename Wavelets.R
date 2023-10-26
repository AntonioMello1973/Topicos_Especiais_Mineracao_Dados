library(wavelets)
library(ggplot2)
library("harbinger")
data(har_examples)

data <- har_examples$example1$serie 

exemplos <- list(har_examples$example1$serie, har_examples$example2$serie, har_examples$example3$serie,
                 har_examples$example4$serie, har_examples$example5$serie, har_examples$example6$serie,
                 har_examples$example7$serie, har_examples$example8$serie)

for (exemplo in exemplos){
  data <- exemplo
    # Realizando a Transformada Wavelet Discreta
    wt <- dwt(data, filter="la8", n.levels=2) # "haar", "d4", "la8", "bl14", "c6"
    
    # Analisando os coeficientes Wavelet
    plot(wt@W$W1, main = "Coeficientes Wavelet (W1)")
    
    #Tratanto os coeficientes Wavelet
    CoefWavelet <- wt@W$W1
    # Desconsiderando os 5% iniciais e os 5% finais dos coeficientes
    descarte <- ceiling(length(CoefWavelet)*0.05)
    CoefWavelet <-  CoefWavelet[-c(1:descarte)]
    CoefWavelet <-  CoefWavelet[-c((length(CoefWavelet)-descarte+1) : length(CoefWavelet))]
    
    # Estimando o valor do limiar
    print(paste("Dica de limiar:", max(abs(CoefWavelet))))
    
    threshold <- max(abs(CoefWavelet)) - max(abs(CoefWavelet))* 0.2
    anomaly_wavelet <- which(abs(wt@W$W1) >= threshold)
    anomaly_wavelet <- anomaly_wavelet[anomaly_wavelet > descarte]
    anomaly_wavelet <- anomaly_wavelet[anomaly_wavelet < length(CoefWavelet) -descarte]
    
    print(paste("anomalia Wavelet (coeficientes Wavelet)=", anomaly_wavelet))
    # W1 tem metade dos elementos que a série temporal original
    anomaly <- (anomaly_wavelet * length(data) / length(wt@W$W1)) - 4
    anomaly <-  anomaly[anomaly > 0]
    print(paste("anomalia ajustada=", anomaly))
    
    t <- 1:length(data)
    anomalia <- ggplot() + 
                geom_line(aes(x=t, y=data), color="blue") +
                geom_point(aes(x=t[anomaly], y=data[anomaly]), color="red", size=3) +
                labs(title=paste("Detecção de Anomalias com Transformada Wavelet:"), x="Tempo", y="Valor") +
                theme_minimal()
    plot(anomalia)
}


##################################################################################################################

##################################################################################################################

data <- har_examples$example15$serie
# Realizando a Transformada Wavelet Discreta
wt <- dwt(data, filter="d4", n.levels=2) # "haar", "d4", "la8", "bl14", "c6"

# Analisando os coeficientes Wavelet
plot(wt@W$W1, main = "Coeficientes Wavelet (W1)")

#Tratanto os coeficientes Wavelet
CoefWavelet <- wt@W$W1
# Desconsiderando os 5% iniciais e os 5% finais dos coeficientes
descarte <- ceiling(length(CoefWavelet)*0.05)
CoefWavelet <-  CoefWavelet[-c(1:descarte)]
CoefWavelet <-  CoefWavelet[-c((length(CoefWavelet)-descarte+1) : length(CoefWavelet))]

# Estimando o valor do limiar
print(paste("Dica de limiar:", max(abs(CoefWavelet))))

threshold <- 0.15
anomaly_wavelet <- which(abs(wt@W$W1) >= threshold)
anomaly_wavelet <- anomaly_wavelet[anomaly_wavelet > descarte]
anomaly_wavelet <- anomaly_wavelet[anomaly_wavelet < length(CoefWavelet) -descarte]

print(paste("anomalia Wavelet (coeficientes Wavelet)=", anomaly_wavelet))
# W1 tem metade dos elementos que a série temporal original
anomaly <- (anomaly_wavelet * length(data) / length(wt@W$W1)) - 4
anomaly <-  anomaly[anomaly > 0]
print(paste("anomalia ajustada=", anomaly))

t <- 1:length(data)
anomalia <- ggplot() + 
  geom_line(aes(x=t, y=data), color="blue") +
  geom_point(aes(x=t[anomaly], y=data[anomaly]), color="red", size=3) +
  labs(title=paste("Detecção de Anomalias com Transformada Wavelet:"), x="Tempo", y="Valor") +
  theme_minimal()
plot(anomalia)

