library(xgboost)
library(caret)

## PARAMETERS --------------------------------------------------------------------------------

vmax.depth = 12
vmin.child.weight = 7
vnround <- 700
imputation.method <- -9999999999
use.weights <- FALSE ## TRUE

semillas <- c(102191) ##, 200177, 410551, 552581, 892237) 

## READ DATA --------------------------------------------------------------------------------

archivo.entrada <- "data/data.train.xgboost.tsv"
dataset <- read.table(archivo.entrada, header=TRUE, sep="\t", row.names="id_cliente")
dataset[is.na(dataset)] <- imputation.method

## --------------------------------------------------------------------------------

calcular.ganancia <- function(testing.prediccion, dataset.testing, probability.threshold, true.SI) {
    total <- 0;
    ids.elegidos <- 0;
    aciertos <- 0;
    for (i in 1:nrow(dataset.testing)) {
        if (testing.prediccion[i] > probability.threshold) { ## *2 para tree model
            ids.elegidos <- ids.elegidos + 1
            acierto <- dataset.testing[i, c("binaria.clase")] == 1
            if(acierto) { aciertos <- aciertos + 1 }
            total <- total + if (acierto) { 7750 } else { -250 }
        }
    }
    cat("ids.elegidos: ", ids.elegidos, "aciertos: ", aciertos, paste("porcentaje aciertos:", aciertos / true.SI * 100, "%", sep=" "), sep=',')
    return(total);
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
    train.data <- if(use.weights) {
        vweights <- ifelse(dataset.training$binaria.clase == 1, 31, 1)
        xgb.DMatrix(data = data.matrix(dataset.training.sinclase), label = dataset.training$binaria.clase, weight = vweights)
    } else {
        xgb.DMatrix(data = data.matrix(dataset.training.sinclase), label = dataset.training$binaria.clase)
    }

    ## tree model (segundo mejor)
    ## modelo <- xgboost(
    ##     data = train.data,
    ##     eta = 0.01, subsample = 1, colsample_bytree = 0.6, alpha = 0, lambda = 0.1, gamma = 0.01,
    ##     min_child_weight = vmin.child.weight, max_depth = vmax.depth, nround = vnround,
    ##     eval_metric = "merror", objective = 'multi:softprob', num_class = 2, nthread = 8)

    ## logistic model (mejor hasta ahora)
    modelo <- xgboost(
        data = train.data,
        booster = "gbtree",
        eta = 0.01, subsample = 0.7, colsample_bytree = 0.75, alpha = 0, lambda = 0.1, gamma = 0.01,
        min_child_weight = vmin.child.weight, max_depth = vmax.depth, nround = vnround,
        eval_metric = "auc", objective = 'binary:logistic', nthread = 8)

    ## model.name <- paste("models", paste("xgboost", vmax.depth, vmin.child.weight, vnround, semilla, imputation.method, use.weights, "final.model", sep="."), sep="/")
    model.name <- "models/final.logistic.3.model"
    xgb.save(modelo, model.name)
}

## PREDICT --------------------------------------------------------------------------------

ganancias <- c()
for (semilla in semillas) {

    set.seed(semilla)
    inTraining <- createDataPartition(dataset$binaria.clase, p = .70, list = FALSE)
    dataset.training <- dataset[ inTraining,]
    dataset.testing  <- dataset[-inTraining,]
    dataset.testing.SI <- nrow(subset(dataset.testing, binaria.clase == 1))
    dataset.training.sinclase <- dataset.training[, !(names(dataset.training) %in% c("binaria.clase"))]
    dataset.testing.sinclase <- dataset.testing[, !(names(dataset.testing) %in% c("binaria.clase"))]
    dataset.testing.sinclase.m <- as.matrix(dataset.testing.sinclase)

    ## model.name <- "xgboost.14.19.550.102191.-9999999999.FALSE.model"
    ## model.name <- paste("xgboost", vmax.depth, vmin.child.weight, vnround, semilla, imputation.method, use.weights, "final.model", sep=".")
    model.name <- "final.logistic.3.model"
    modelo.loaded <- xgb.load(paste("models", model.name, sep="/"))
    
    for (ntree.limit in seq(400, vnround, 10)) { 
        testing.prediccion = predict(modelo.loaded, dataset.testing.sinclase.m, ntreelimit = ntree.limit)
        id.cliente <- rownames(dataset.testing)
        for (probability.threshold in seq(0.0275, 0.04, 0.001)) {
            predicciones.archivo.salida <- paste("prediccion", vmax.depth, vmin.child.weight, vnround, probability.threshold, "txt", sep=".")
            total <- calcular.ganancia(testing.prediccion, dataset.testing, probability.threshold, dataset.testing.SI)
            cat("semilla: ", semilla, "ntree.limit: ", ntree.limit, "total", total, "sample.total: ", total / 0.30, "probability.threshold: ", probability.threshold, '\n')
            ganancias <- rbind(ganancias, data.frame(semilla=semilla, ntree.limit=ntree.limit, probability.threshold=probability.threshold, sample.total=total, total=total / 0.30))
        }
    }
}


## DATA --------------------------------------------------------------------------------

## data train total:
## 1103 SI
## 151489 NO
## proporcion 0.007281057

## data train test:
## 339 SI
## 45438 NO
## proporcion 0.007460716
## modelo entrega 2373 => entregar = SI * 7 aprox => usando data.futuro, espero 1103 * 7 => debería entregar 7721 




## PLOT --------------------------------------------------------------------------------

## importance.matrix <-xgb.importance(names(dataset.testing), model = modelo.loaded) ## , data = dataset.training.sinclase, label = dataset.training$binaria.clase)
## xgb.plot.importance(importance.matrix)
## xgb.dump(modelo, with.stats=T, filename_dump="tree.dump")
## xgb.plot.tree(modelo.loaded, feature_names=names(dataset.training.sinclase), filename_dump="tree.dump")

## MEASURE --------------------------------------------------------------------------------

## max.ganancia <- ganancias[which.max(ganancias$total),]
## max.ganancia

## mean.ganancias <- data.frame(ntree.limit=NULL, total=NULL)
## for (i in seq(400, vnround, 10)) {
##     mean.ganancias <- rbind(mean.ganancias, data.frame(ntree.limit=i, total=mean(subset(ganancias, ntree.limit==i)$total)))
## }

## mean.ganancias <- aggregate(ganancias, list(ganancias$semilla, ganancias$ntree.limit), FUN=mean, na.rm=TRUE)[,c('semilla', 'ntree.limit','total')]
## best.mean.ganancias <- subset(mean.ganancias, total > 2200000)
## library(dplyr); arrange(best.mean.ganancias, desc(total))
## s1 <- subset(best.mean.ganancias, semilla == semillas[1])
## s5 <- subset(best.mean.ganancias, semilla == semillas[5])
## plot(s1$ntree.limit, s1$total, type='l')
## plot(s5$ntree.limit, s5$total, type='l')
## x  <- c(400,    410,    420,    430,    440,    450,    460,    470,    480,    490,    500,    510,    520)
## y1 <- c(2301667,2425833,2523333,2626667,2710833,2789167,2852500,2788333,2852500,2854167,2796667,2693333,2716667) # 0.027
## y2 <- c(2426667,2536667,2635833,2748333,2804167,2744167,2788333,2788333,2821667,2815833,2730000,2759167,2806667) # 0.028    
## y3 <- c(2524167,2676667,2768333,2781667,2743333,2801667,2775833,2810833,2788333,2756667,2758333,2698333,2710833) # 0.029
## y4 <- c(2650833,2790833,2755833,2783333,2845833,2795833,2760833,2783333,2734167,2701667,2696667,2703333,2710833) # 0.03
## y5 <- c(2758333,2770833,2764167,2831667,2833333,2791667,2749167,2694167,2748333,2732500,2721667,2755833,2743333) # 0.03125
## y6 <- c(2800833,2790833,2735833,2672500,2672500,2703333,2698333,2670833,2706667,2710833,2698333,2699167,2701667) # 0.035
## plot(x, y1, col="red", type="l")   
## lines(x, y2, col="blue")  
## lines(x, y3, col="green") 
## lines(x, y4, col="yellow")
## lines(x, y5, col="grey") 
## lines(x, y6, col="brown")
## ymeans <- data.frame(x=x, y=rep(0, times=length(x)))
## for (i in 1:length(x)) { ymeans[i, 2] <- mean(y1[i], y2[i], y3[i], y4[i], y5[i], y6[i]) }
## plot(ymeans, type="l")
## abline(h = max(ymeans$y))

## --------------------------------------------------------------------------------

head(ganancias[order(-ganancias$total),], 40)
    semilla ntree.limit probability.threshold sample.total   total
132  102191         500                0.0285       887750 2959167
71   102191         450                0.0325       882750 2942500
131  102191         500                0.0275       881000 2936667
133  102191         500                0.0295       880750 2935833
94   102191         470                0.0295       880250 2934167
84   102191         460                0.0325       878750 2929167
119  102191         490                0.0285       878750 2929167
59   102191         440                0.0335       878250 2927500
32   102191         420                0.0325       875000 2916667
121  102191         490                0.0305       875000 2916667
120  102191         490                0.0295       874750 2915833
134  102191         500                0.0305       873750 2912500
108  102191         480                0.0305       871250 2904167
47   102191         430                0.0345       871000 2903333
7    102191         400                0.0335       870250 2900833
22   102191         410                0.0355       869000 2896667
20   102191         410                0.0335       868000 2893333
82   102191         460                0.0305       868000 2893333
34   102191         420                0.0345       867500 2891667
97   102191         470                0.0325       867500 2891667
122  102191         490                0.0315       867500 2891667
45   102191         430                0.0325       867250 2890833
70   102191         450                0.0315       867250 2890833
106  102191         480                0.0285       867250 2890833
68   102191         450                0.0295       867000 2890000
56   102191         440                0.0305       866500 2888333
10   102191         400                0.0365       866000 2886667
46   102191         430                0.0335       866000 2886667
55   102191         440                0.0295       864000 2880000
58   102191         440                0.0325       864000 2880000
83   102191         460                0.0315       864000 2880000
72   102191         450                0.0335       863750 2879167
69   102191         450                0.0305       863250 2877500
95   102191         470                0.0305       863250 2877500
60   102191         440                0.0345       863000 2876667
31   102191         420                0.0315       862750 2875833
81   102191         460                0.0295       862500 2875000
96   102191         470                0.0315       862250 2874167
44   102191         430                0.0315       860000 2866667
118  102191         490                0.0275       860000 2866667
