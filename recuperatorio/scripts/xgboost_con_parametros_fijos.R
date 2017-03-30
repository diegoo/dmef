library(xgboost)
library(caret)

## PARAMETERS --------------------------------------------------------------------------------

imputation.method <- -9999999999
vmax.depth = 14
vmin.child.weight = 19
vnround <- 2000
probability.threshold <- 0.03125
use.weights <- FALSE ## TRUE

semillas <- c(102191) ## , 892237) ## 200177, 410551, 552581, 

## READ DATA --------------------------------------------------------------------------------

archivo.entrada <- "data/dataset.xgboost.tsv"
dataset <- read.table(archivo.entrada, header=TRUE, sep="\t", row.names="id_cliente")
dataset[is.na(dataset)] <- imputation.method

## --------------------------------------------------------------------------------

calcular.ganancia <- function(testing.prediccion, dataset.testing, probability.threshold) {
    total <- 0;
    ids.elegidos <- 0;
    for (i in 1:nrow(dataset.testing)) {
        if (testing.prediccion[i*2] > probability.threshold) {
            ids.elegidos <- ids.elegidos + 1
            acierto <- dataset.testing[i, c("binaria.clase")] == 1
            total <- total + if (acierto) { 7750 } else { -250 }
        }
    }
    cat("ids.elegidos: ", ids.elegidos, ',')
    return(total);
}

elegir.clientes <- function(prediccion, clientes, probability.threshold) {
    ids <- c()
    for (i in 1:length(clientes)) {
        if (prediccion[i*2] > probability.threshold) {
            ids <- append(ids, clientes[i])
        }
    }
    return(ids);
}

## TRAIN --------------------------------------------------------------------------------

for (semilla in semillas) {

    set.seed(semilla)
    inTraining <- createDataPartition(dataset$binaria.clase, p = .70, list = FALSE)
    dataset.training <- dataset[ inTraining,]
    dataset.testing  <- dataset[-inTraining,]
    dataset.training.sinclase <- dataset.training[, !(names(dataset.training) %in% c("binaria.clase"))]
    dataset.testing.sinclase <- dataset.testing[, !(names(dataset.testing) %in% c("binaria.clase"))]
    dataset.testing.sinclase.m <- as.matrix(dataset.testing.sinclase)
    vweights <- ifelse(dataset.training$binaria.clase == 1, 31, 1)
    train.data <- if(use.weights) {
        xgb.DMatrix(data = data.matrix(dataset.training.sinclase), label = dataset.training$binaria.clase, weight = vweights)
    } else {
        xgb.DMatrix(data = data.matrix(dataset.training.sinclase), label = dataset.training$binaria.clase)
    }
    
    modelo <- xgboost(
        data = train.data,
        eta = 0.01, subsample = 1.0, colsample_bytree = 0.6, alpha = 0, lambda = 0.1, gamma = 0.01,
        min_child_weight = vmin.child.weight, max_depth = vmax.depth, nround = vnround,
        eval_metric = "merror", objective = 'multi:softprob', num_class = 2, nthread = 8)
    xgb.save(modelo, paste("xgboost", vmax.depth, vmin.child.weight, vnround, semilla, imputation.method, use.weights, "model", sep="."))
}

## PREDICT --------------------------------------------------------------------------------

ganancias <- c()
for (semilla in semillas) {

    set.seed(semilla)
    inTraining <- createDataPartition(dataset$binaria.clase, p = .70, list = FALSE)
    dataset.training <- dataset[ inTraining,]
    dataset.testing  <- dataset[-inTraining,]
    dataset.training.sinclase <- dataset.training[, !(names(dataset.training) %in% c("binaria.clase"))]
    dataset.testing.sinclase <- dataset.testing[, !(names(dataset.testing) %in% c("binaria.clase"))]
    dataset.testing.sinclase.m <- as.matrix(dataset.testing.sinclase)

    model.name <- paste("xgboost", vmax.depth, vmin.child.weight, vnround, semilla, imputation.method, use.weights, "model", sep=".") ## , imputation.method
    modelo.loaded <- xgb.load(model.name)

    for (i in seq(400, vnround, 10)) { 
        ntree.limit <- i
        testing.prediccion = predict(modelo.loaded, dataset.testing.sinclase.m, ntreelimit = ntree.limit)
        id.cliente <- rownames(dataset.testing)
        predicciones.archivo.salida <- paste("prediccion", vmax.depth, vmin.child.weight, vnround, probability.threshold, "txt", sep=".")
        total <- calcular.ganancia(testing.prediccion, dataset.testing, probability.threshold)
        cat("semilla: ", semilla, "ntree.limit: ", ntree.limit, "total", total, "sample.total: ", total / 0.30, '\n')
        ganancias <- rbind(ganancias, data.frame(semilla=semilla, ntree.limit=ntree.limit, sample.total=total, total=total / 0.30))
    }
}

max.ganancia <- ganancias[which.max(ganancias$total),]
max.ganancia

mean.ganancias <- aggregate(ganancias, list(ganancias$semilla, ganancias$ntree.limit), FUN=mean, na.rm=TRUE)[,c('semilla', 'ntree.limit','total')]
best.mean.ganancias <- subset(mean.ganancias, total > 2200000)
library(dplyr); arrange(best.mean.ganancias, desc(total))
s1 <- subset(best.mean.ganancias, semilla == semillas[1])
s5 <- subset(best.mean.ganancias, semilla == semillas[5])
plot(s1$ntree.limit, s1$total, type='l')
plot(s5$ntree.limit, s5$total, type='l')

## PLOT --------------------------------------------------------------------------------

importance.matrix <-xgb.importance(names(dataset.testing), model = modelo.loaded) ## , data = dataset.training.sinclase, label = dataset.training$binaria.clase)
xgb.plot.importance(importance.matrix)
xgb.dump(modelo, with.stats=T, filename_dump="tree.dump")
## xgb.plot.tree(modelo.loaded, feature_names=names(dataset.training.sinclase), filename_dump="tree.dump")

## FINAL PREDICTION --------------------------------------------------------------------------------

archivo.entrada <- "data/dataset.xgboost.FUTURO.tsv"
dataset.futuro <- read.table(archivo.entrada, header=TRUE, sep="\t", row.names="id_cliente")
dataset.futuro[is.na(dataset.futuro)] <- imputation.method
semilla <- 102191

## assert!
## length(rownames(dataset.testing.sinclase)) == length(testing.prediccion) / 2

ids.elegidos <- elegir.clientes(testing.prediccion, rownames(dataset.testing.sinclase), 0.03125)


## --------------------------------------------------------------------------------

## el mejor promedio entre semillas es ntree.limit 520:
## > blah <- for (i in seq(400, 700, 10)) { cat(i, mean(subset(ganancias, ntree.limit==i)$total), '\n') }
## 400 2086000 
## 410 2180000 
## 420 2206667 
## 430 2245667 
## 440 2251333 
## 450 2281833 
## 460 2288667 
## 470 2260333 
## 480 2293833 
## 490 2297333 
## 500 2284500 
## 510 2305667 
## 520 2307833 
## 530 2285667 
## 540 2280500 
## 550 2288667 
## 560 2284500 
## 570 2281333 
## 580 2251333 
## 590 2263500 
## 600 2255000 
## 610 2260833 
## 620 2256833 
## 630 2256833 
## 640 2247500 
## 650 2251000 

## --------------------------------------------------------------------------------

## con umbral 0.027

ids.elegidos:  3990 ,semilla:  102191 ntree.limit:  400 total 690500 sample.total:  2301667 
ids.elegidos:  3713 ,semilla:  102191 ntree.limit:  410 total 727750 sample.total:  2425833 
ids.elegidos:  3500 ,semilla:  102191 ntree.limit:  420 total 757000 sample.total:  2523333 
ids.elegidos:  3312 ,semilla:  102191 ntree.limit:  430 total 788000 sample.total:  2626667 
ids.elegidos:  3179 ,semilla:  102191 ntree.limit:  440 total 813250 sample.total:  2710833 
ids.elegidos:  3053 ,semilla:  102191 ntree.limit:  450 total 836750 sample.total:  2789167 
ids.elegidos:  2945 ,semilla:  102191 ntree.limit:  460 total 855750 sample.total:  2852500 
ids.elegidos:  2862 ,semilla:  102191 ntree.limit:  470 total 836500 sample.total:  2788333 
ids.elegidos:  2785 ,semilla:  102191 ntree.limit:  480 total 855750 sample.total:  2852500 
ids.elegidos:  2719 ,semilla:  102191 ntree.limit:  490 total 856250 sample.total:  2854167 
ids.elegidos:  2660 ,semilla:  102191 ntree.limit:  500 total 839000 sample.total:  2796667 
ids.elegidos:  2592 ,semilla:  102191 ntree.limit:  510 total 808000 sample.total:  2693333 
ids.elegidos:  2532 ,semilla:  102191 ntree.limit:  520 total 815000 sample.total:  2716667 

## con umbral 0.028

ids.elegidos:  3712 ,semilla:  102191 ntree.limit:  400 total 728000 sample.total:  2426667
ids.elegidos:  3484 ,semilla:  102191 ntree.limit:  410 total 761000 sample.total:  2536667
ids.elegidos:  3269 ,semilla:  102191 ntree.limit:  420 total 790750 sample.total:  2635833
ids.elegidos:  3134 ,semilla:  102191 ntree.limit:  430 total 824500 sample.total:  2748333
ids.elegidos:  3003 ,semilla:  102191 ntree.limit:  440 total 841250 sample.total:  2804167
ids.elegidos:  2883 ,semilla:  102191 ntree.limit:  450 total 823250 sample.total:  2744167
ids.elegidos:  2798 ,semilla:  102191 ntree.limit:  460 total 836500 sample.total:  2788333
ids.elegidos:  2702 ,semilla:  102191 ntree.limit:  470 total 836500 sample.total:  2788333
ids.elegidos:  2630 ,semilla:  102191 ntree.limit:  480 total 846500 sample.total:  2821667
ids.elegidos:  2573 ,semilla:  102191 ntree.limit:  490 total 844750 sample.total:  2815833
ids.elegidos:  2516 ,semilla:  102191 ntree.limit:  500 total 819000 sample.total:  2730000
ids.elegidos:  2449 ,semilla:  102191 ntree.limit:  510 total 827750 sample.total:  2759167
ids.elegidos:  2392 ,semilla:  102191 ntree.limit:  520 total 842000 sample.total:  2806667

## con umbral 0.029

ids.elegidos:  3467 ,semilla:  102191 ntree.limit:  400 total 757250 sample.total:  2524167 
ids.elegidos:  3252 ,semilla:  102191 ntree.limit:  410 total 803000 sample.total:  2676667 
ids.elegidos:  3078 ,semilla:  102191 ntree.limit:  420 total 830500 sample.total:  2768333 
ids.elegidos:  2934 ,semilla:  102191 ntree.limit:  430 total 834500 sample.total:  2781667 
ids.elegidos:  2820 ,semilla:  102191 ntree.limit:  440 total 823000 sample.total:  2743333 
ids.elegidos:  2718 ,semilla:  102191 ntree.limit:  450 total 840500 sample.total:  2801667 
ids.elegidos:  2621 ,semilla:  102191 ntree.limit:  460 total 832750 sample.total:  2775833 
ids.elegidos:  2547 ,semilla:  102191 ntree.limit:  470 total 843250 sample.total:  2810833 
ids.elegidos:  2478 ,semilla:  102191 ntree.limit:  480 total 836500 sample.total:  2788333 
ids.elegidos:  2420 ,semilla:  102191 ntree.limit:  490 total 827000 sample.total:  2756667 
ids.elegidos:  2354 ,semilla:  102191 ntree.limit:  500 total 827500 sample.total:  2758333 
ids.elegidos:  2298 ,semilla:  102191 ntree.limit:  510 total 809500 sample.total:  2698333 
ids.elegidos:  2251 ,semilla:  102191 ntree.limit:  520 total 813250 sample.total:  2710833

## con umbral 0.03

ids.elegidos:  3251 ,ntree.limit:  400 total 795250 sample.total:  2650833 
ids.elegidos:  3051 ,ntree.limit:  410 total 837250 sample.total:  2790833 
ids.elegidos:  2901 ,ntree.limit:  420 total 826750 sample.total:  2755833 
ids.elegidos:  2772 ,ntree.limit:  430 total 835000 sample.total:  2783333 
ids.elegidos:  2665 ,ntree.limit:  440 total 853750 sample.total:  2845833 
ids.elegidos:  2565 ,ntree.limit:  450 total 838750 sample.total:  2795833 
ids.elegidos:  2479 ,ntree.limit:  460 total 828250 sample.total:  2760833 
ids.elegidos:  2420 ,ntree.limit:  470 total 835000 sample.total:  2783333 
ids.elegidos:  2351 ,ntree.limit:  480 total 820250 sample.total:  2734167 
ids.elegidos:  2294 ,ntree.limit:  490 total 810500 sample.total:  2701667 
ids.elegidos:  2236 ,ntree.limit:  500 total 809000 sample.total:  2696667 
ids.elegidos:  2196 ,ntree.limit:  510 total 811000 sample.total:  2703333 
ids.elegidos:  2155 ,ntree.limit:  520 total 813250 sample.total:  2710833 

## con umbral 0.03125

ids.elegidos:  2994 ,semilla:  102191 ntree.limit:  400 total 827500 sample.total:  2758333 
ids.elegidos:  2819 ,semilla:  102191 ntree.limit:  410 total 831250 sample.total:  2770833 
ids.elegidos:  2699 ,semilla:  102191 ntree.limit:  420 total 829250 sample.total:  2764167 
ids.elegidos:  2586 ,semilla:  102191 ntree.limit:  430 total 849500 sample.total:  2831667 
ids.elegidos:  2488 ,semilla:  102191 ntree.limit:  440 total 850000 sample.total:  2833333 
ids.elegidos:  2410 ,semilla:  102191 ntree.limit:  450 total 837500 sample.total:  2791667 
ids.elegidos:  2333 ,semilla:  102191 ntree.limit:  460 total 824750 sample.total:  2749167 
ids.elegidos:  2271 ,semilla:  102191 ntree.limit:  470 total 808250 sample.total:  2694167 
ids.elegidos:  2206 ,semilla:  102191 ntree.limit:  480 total 824500 sample.total:  2748333 
ids.elegidos:  2161 ,semilla:  102191 ntree.limit:  490 total 819750 sample.total:  2732500 
ids.elegidos:  2110 ,semilla:  102191 ntree.limit:  500 total 816500 sample.total:  2721667 
ids.elegidos:  2069 ,semilla:  102191 ntree.limit:  510 total 826750 sample.total:  2755833 
ids.elegidos:  2020 ,semilla:  102191 ntree.limit:  520 total 823000 sample.total:  2743333


x  <- c(400,    410,    420,    430,    440,    450,    460,    470,    480,    490,    500,    510,    520)
y1 <- c(2301667,2425833,2523333,2626667,2710833,2789167,2852500,2788333,2852500,2854167,2796667,2693333,2716667) # 0.027
y2 <- c(2426667,2536667,2635833,2748333,2804167,2744167,2788333,2788333,2821667,2815833,2730000,2759167,2806667) # 0.028    
y3 <- c(2524167,2676667,2768333,2781667,2743333,2801667,2775833,2810833,2788333,2756667,2758333,2698333,2710833) # 0.029
y4 <- c(2650833,2790833,2755833,2783333,2845833,2795833,2760833,2783333,2734167,2701667,2696667,2703333,2710833) # 0.03
y5 <- c(2758333,2770833,2764167,2831667,2833333,2791667,2749167,2694167,2748333,2732500,2721667,2755833,2743333) # 0.03125
y6 <- c(2800833,2790833,2735833,2672500,2672500,2703333,2698333,2670833,2706667,2710833,2698333,2699167,2701667) # 0.035
                                                                                                            
plot(x, y1, col="red", type="l")   
lines(x, y2, col="blue")  
lines(x, y3, col="green") 
lines(x, y4, col="yellow")
lines(x, y5, col="grey") 
lines(x, y6, col="brown")
