library(xgboost)
library(caret)

## PARAMETERS --------------------------------------------------------------------------------

vmax.depth = 10
vmin.child.weight = 5
vnround <- 550
imputation.method <- -9999999999
use.weights <- FALSE ## TRUE

semillas <- c(102191) ##, 200177, 410551, 552581, 892237) 

## READ DATA --------------------------------------------------------------------------------

archivo.entrada <- "data/data.train.xgboost.tsv"
dataset <- read.table(archivo.entrada, header=TRUE, sep="\t", row.names="id_cliente")
dataset[is.na(dataset)] <- imputation.method

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
        eta = 0.01, subsample = 0.7, colsample_bytree = 0.7, alpha = 0, lambda = 0.1, gamma = 0.01,
        min_child_weight = vmin.child.weight, max_depth = vmax.depth, nround = vnround,
        eval_metric = "auc", objective = 'binary:logistic', nthread = 8)

    ## model.name <- paste("models", paste("xgboost", vmax.depth, vmin.child.weight, vnround, semilla, imputation.method, use.weights, "final.model", sep="."), sep="/")
    model.name <- "models/final.logistic.6.model"
    xgb.save(modelo, model.name)
}

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
    cat("ids.elegidos: ", ids.elegidos, "aciertos: ", aciertos, "total: ", total / 0.30, '\n')
    return(total);
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
    model.name <- "final.logistic.4.model"
    modelo.loaded <- xgb.load(paste("models", model.name, sep="/"))
    
    for (ntree.limit in seq(400, vnround, 10)) { 
        testing.prediccion = predict(modelo.loaded, dataset.testing.sinclase.m, ntreelimit = ntree.limit)
        id.cliente <- rownames(dataset.testing)
        for (probability.threshold in seq(0.0275, 0.045, 0.001)) {
            predicciones.archivo.salida <- paste("prediccion", vmax.depth, vmin.child.weight, vnround, probability.threshold, "txt", sep=".")
            
            total.ganancia <- calcular.ganancia(testing.prediccion, dataset.testing, probability.threshold, dataset.testing.SI)
            result <- total.ganancia / 0.30; 
            if (result > 2900000) {
                cat("ntree.limit: ", ntree.limit, "umbral: ", probability.threshold, '\n')
                ganancias <- rbind(ganancias, data.frame(ntree.limit=ntree.limit, probability.threshold=probability.threshold, total=result))
            }
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

## final.model (tree):

## > head(ganancias[order(-ganancias$total),], 40)
##     semilla ntree.limit probability.threshold sample.total   total
## 33   102191         420                0.0335       870750 2902500
## 45   102191         430                0.0325       867750 2892500
## 67   102191         450                0.0285       865250 2884167
## 44   102191         430                0.0315       857750 2859167
## 32   102191         420                0.0325       856500 2855000
## 105  102191         480                0.0275       856000 2853333
## 21   102191         410                0.0345       854250 2847500
## 92   102191         470                0.0275       853750 2845833
## 18   102191         410                0.0315       853000 2843333
## 80   102191         460                0.0285       852500 2841667
## 68   102191         450                0.0295       851250 2837500
## 8    102191         400                0.0345       850000 2833333
## 71   102191         450                0.0325       849500 2831667
## 20   102191         410                0.0335       849000 2830000
## 6    102191         400                0.0325       848000 2826667
## 10   102191         400                0.0365       846000 2820000
## 56   102191         440                0.0305       845000 2816667
## 55   102191         440                0.0295       844500 2815000
## 22   102191         410                0.0355       843000 2810000
## 46   102191         430                0.0335       842750 2809167
## 79   102191         460                0.0275       842500 2808333
## 54   102191         440                0.0285       840750 2802500
## 31   102191         420                0.0315       840500 2801667
## 19   102191         410                0.0325       840250 2800833
## 30   102191         420                0.0305       840000 2800000
## 93   102191         470                0.0285       839500 2798333
## 131  102191         500                0.0275       839000 2796667
## 9    102191         400                0.0355       838750 2795833
## 43   102191         430                0.0305       838250 2794167
## 69   102191         450                0.0305       837500 2791667
## 57   102191         440                0.0315       837250 2790833
## 118  102191         490                0.0275       837000 2790000
## 7    102191         400                0.0335       836500 2788333
## 42   102191         430                0.0295       836500 2788333
## 59   102191         440                0.0335       835500 2785000
## 84   102191         460                0.0325       833750 2779167
## 119  102191         490                0.0285       833750 2779167
## 70   102191         450                0.0315       833000 2776667
## 106  102191         480                0.0285       832500 2775000
## 58   102191         440                0.0325       831750 2772500

## logistic.2.model:

## head(ganancias[order(-ganancias$total),], 40)
##     semilla ntree.limit probability.threshold sample.total   total
## 132  102191         500                0.0285       887750 2959167
## 71   102191         450                0.0325       882750 2942500
## 131  102191         500                0.0275       881000 2936667
## 133  102191         500                0.0295       880750 2935833
## 94   102191         470                0.0295       880250 2934167
## 84   102191         460                0.0325       878750 2929167
## 119  102191         490                0.0285       878750 2929167
## 59   102191         440                0.0335       878250 2927500
## 32   102191         420                0.0325       875000 2916667
## 121  102191         490                0.0305       875000 2916667
## 120  102191         490                0.0295       874750 2915833
## 134  102191         500                0.0305       873750 2912500
## 108  102191         480                0.0305       871250 2904167
## 47   102191         430                0.0345       871000 2903333
## 7    102191         400                0.0335       870250 2900833
## 22   102191         410                0.0355       869000 2896667
## 20   102191         410                0.0335       868000 2893333
## 82   102191         460                0.0305       868000 2893333
## 34   102191         420                0.0345       867500 2891667
## 97   102191         470                0.0325       867500 2891667
## 122  102191         490                0.0315       867500 2891667
## 45   102191         430                0.0325       867250 2890833
## 70   102191         450                0.0315       867250 2890833
## 106  102191         480                0.0285       867250 2890833
## 68   102191         450                0.0295       867000 2890000
## 56   102191         440                0.0305       866500 2888333
## 10   102191         400                0.0365       866000 2886667
## 46   102191         430                0.0335       866000 2886667
## 55   102191         440                0.0295       864000 2880000
## 58   102191         440                0.0325       864000 2880000
## 83   102191         460                0.0315       864000 2880000
## 72   102191         450                0.0335       863750 2879167
## 69   102191         450                0.0305       863250 2877500
## 95   102191         470                0.0305       863250 2877500
## 60   102191         440                0.0345       863000 2876667
## 31   102191         420                0.0315       862750 2875833
## 81   102191         460                0.0295       862500 2875000
## 96   102191         470                0.0315       862250 2874167
## 44   102191         430                0.0315       860000 2866667
## 118  102191         490                0.0275       860000 2866667

## logistic.3.model:

## > head(ganancias[order(-ganancias$total),], 40)
##     semilla ntree.limit probability.threshold sample.total   total
## 70   102191         450                0.0315       865750 2885833
## 94   102191         470                0.0295       862250 2874167
## 81   102191         460                0.0295       860750 2869167
## 21   102191         410                0.0345       859500 2865000
## 10   102191         400                0.0365       858750 2862500
## 93   102191         470                0.0285       853500 2845000
## 144  102191         510                0.0275       852000 2840000
## 33   102191         420                0.0335       851500 2838333
## 59   102191         440                0.0335       850250 2834167
## 6    102191         400                0.0325       849000 2830000
## 35   102191         420                0.0355       849000 2830000
## 106  102191         480                0.0285       848500 2828333
## 34   102191         420                0.0345       848000 2826667
## 57   102191         440                0.0315       847750 2825833
## 32   102191         420                0.0325       847250 2824167
## 225  102191         570                0.0305       846750 2822500
## 47   102191         430                0.0345       846500 2821667
## 83   102191         460                0.0315       846250 2820833
## 11   102191         400                0.0375       845750 2819167
## 82   102191         460                0.0305       845750 2819167
## 18   102191         410                0.0315       845250 2817500
## 60   102191         440                0.0345       845250 2817500
## 131  102191         500                0.0275       844500 2815000
## 212  102191         560                0.0305       844000 2813333
## 237  102191         580                0.0295       844000 2813333
## 22   102191         410                0.0355       843750 2812500
## 200  102191         550                0.0315       843750 2812500
## 84   102191         460                0.0325       843500 2811667
## 163  102191         520                0.0335       843250 2810833
## 9    102191         400                0.0355       843000 2810000
## 71   102191         450                0.0325       842500 2808333
## 23   102191         410                0.0365       842250 2807500
## 132  102191         500                0.0285       842250 2807500
## 55   102191         440                0.0295       842000 2806667
## 188  102191         540                0.0325       842000 2806667
## 46   102191         430                0.0335       841250 2804167
## 119  102191         490                0.0285       841000 2803333
## 20   102191         410                0.0335       840750 2802500
## 48   102191         430                0.0355       840750 2802500
## 36   102191         420                0.0365       840500 2801667

## logistic.4.model

## > head(ganancias[order(-ganancias$total),], 40)
##     ntree.limit probability.threshold   total
## 8           400                0.0345 3030833
## 78          440                0.0325 2989167
## 27          410                0.0355 2985000
## 61          430                0.0335 2985000
## 113         460                0.0315 2980833
## 79          440                0.0335 2970833
## 130         470                0.0305 2960833
## 129         470                0.0295 2959167
## 111         460                0.0295 2953333
## 43          420                0.0335 2952500
## 97          450                0.0335 2950833
## 60          430                0.0325 2949167
## 96          450                0.0325 2948333
## 44          420                0.0345 2947500
## 77          440                0.0315 2947500
## 147         480                0.0295 2944167
## 23          410                0.0315 2940000
## 26          410                0.0345 2940000
## 59          430                0.0315 2939167
## 146         480                0.0285 2937500
## 6           400                0.0325 2935833
## 25          410                0.0335 2930833
## 163         490                0.0275 2929167
## 9           400                0.0355 2926667
## 128         470                0.0285 2926667
## 45          420                0.0355 2923333
## 93          450                0.0295 2922500
## 94          450                0.0305 2922500
## 187         500                0.0335 2920833
## 95          450                0.0315 2920000
## 164         490                0.0285 2915000
## 76          440                0.0305 2914167
## 41          420                0.0315 2912500
## 148         480                0.0305 2912500
## 165         490                0.0295 2911667
## 7           400                0.0335 2910833
## 203         510                0.0315 2910833
## 40          420                0.0305 2910000
## 92          450                0.0285 2908333
## 114         460                0.0325 2907500
