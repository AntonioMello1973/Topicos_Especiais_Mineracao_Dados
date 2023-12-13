# Harbinger Package
# version 1.0.737
source("https://raw.githubusercontent.com/cefet-rj-dal/harbinger-examples/main/jupyter.R")
source("header.R")
#loading Harbinger

("daltoolbox") 
load_library("harbinger")



#loading the example database
data(har_examples)



#Using the time series 1 
dataset <- har_examples$example1
head(dataset)

plot_ts(x = 1:length(dataset$serie), y = dataset$serie)



# establishing arima method 
#model <- hanr_arima()
source("hanr_wavelet.R")
model <- hanr_wavelet()

# fitting the model
model <- fit(model, dataset$serie)

# making detections using hanr_fbiad
detection <- detect(model, dataset$serie)



# filtering detected events
print(detection |> dplyr::filter(event==TRUE))

# evaluating the detections
evaluation <- evaluate(model, detection$event, dataset$event)
print(evaluation$confMatrix)

grf <- har_plot(model, dataset$serie, detection, dataset$event)
plot(grf)
