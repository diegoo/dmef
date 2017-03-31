library(xgboost)
library(caret)

## PARAMETERS --------------------------------------------------------------------------------

vmax.depth = 14
vmin.child.weight =  19
vnround <- 550
probability.threshold <- 0.03125
imputation.method <- -9999999999
use.weights <- FALSE ## TRUE

semillas <- c(102191) ##, 200177, 410551, 552581, 892237) 

## READ DATA --------------------------------------------------------------------------------

archivo.entrada <- "data/data.train.xgboost.tsv"
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

    model.name <- paste("models", paste("xgboost", vmax.depth, vmin.child.weight, vnround, semilla, imputation.method, use.weights, "final.model", sep="."), sep="/")
    xgb.save(modelo, model.name)
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

    ## model.name <- "xgboost.14.19.550.102191.-9999999999.FALSE.model"
    model.name <- paste("xgboost", vmax.depth, vmin.child.weight, vnround, semilla, imputation.method, use.weights, "final.model", sep=".")
    modelo.loaded <- xgb.load(paste("models", model.name, sep="/"))
    
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

## PLOT --------------------------------------------------------------------------------

importance.matrix <-xgb.importance(names(dataset.testing), model = modelo.loaded) ## , data = dataset.training.sinclase, label = dataset.training$binaria.clase)
xgb.plot.importance(importance.matrix)
xgb.dump(modelo, with.stats=T, filename_dump="tree.dump")
## xgb.plot.tree(modelo.loaded, feature_names=names(dataset.training.sinclase), filename_dump="tree.dump")

## MEASURE --------------------------------------------------------------------------------

max.ganancia <- ganancias[which.max(ganancias$total),]
max.ganancia

mean.ganancias <- data.frame(ntree.limit=NULL, total=NULL)
for (i in seq(400, vnround, 10)) {
    mean.ganancias <- rbind(mean.ganancias, data.frame(ntree.limit=i, total=mean(subset(ganancias, ntree.limit==i)$total)))
}

##  ntree.limit   total
##          400 2086000
##          410 2180000
##          420 2206667
##          430 2245667
##          440 2251333
##          450 2281833
##          460 2288667
##          470 2260333
##          480 2293833
##          490 2297333
##          500 2284500
##          510 2305667
##          520 2307833
##          530 2285667
##          540 2280500
##          550 2288667

## y usando umbral de 0.0275
##  ntree.limit   total
##          400 1829167
##          410 1913000
##          420 2001667
##          430 2055333
##          440 2123167
##          450 2160833
##          460 2193167
##          470 2213167
##          480 2243500
##          490 2237167
##          500 2247000
##          510 2265167
##          520 2291167
##          530 2288333
##          540 2301667
##          550 2306167

mean.ganancias <- aggregate(ganancias, list(ganancias$semilla, ganancias$ntree.limit), FUN=mean, na.rm=TRUE)[,c('semilla', 'ntree.limit','total')]
best.mean.ganancias <- subset(mean.ganancias, total > 2200000)
library(dplyr); arrange(best.mean.ganancias, desc(total))
s1 <- subset(best.mean.ganancias, semilla == semillas[1])
s5 <- subset(best.mean.ganancias, semilla == semillas[5])
plot(s1$ntree.limit, s1$total, type='l')
plot(s5$ntree.limit, s5$total, type='l')

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

## ids.elegidos:  3990 ,semilla:  102191 ntree.limit:  400 total 690500 sample.total:  2301667 
## ids.elegidos:  3713 ,semilla:  102191 ntree.limit:  410 total 727750 sample.total:  2425833 
## ids.elegidos:  3500 ,semilla:  102191 ntree.limit:  420 total 757000 sample.total:  2523333 
## ids.elegidos:  3312 ,semilla:  102191 ntree.limit:  430 total 788000 sample.total:  2626667 
## ids.elegidos:  3179 ,semilla:  102191 ntree.limit:  440 total 813250 sample.total:  2710833 
## ids.elegidos:  3053 ,semilla:  102191 ntree.limit:  450 total 836750 sample.total:  2789167 
## ids.elegidos:  2945 ,semilla:  102191 ntree.limit:  460 total 855750 sample.total:  2852500 
## ids.elegidos:  2862 ,semilla:  102191 ntree.limit:  470 total 836500 sample.total:  2788333 
## ids.elegidos:  2785 ,semilla:  102191 ntree.limit:  480 total 855750 sample.total:  2852500 
## ids.elegidos:  2719 ,semilla:  102191 ntree.limit:  490 total 856250 sample.total:  2854167 
## ids.elegidos:  2660 ,semilla:  102191 ntree.limit:  500 total 839000 sample.total:  2796667 
## ids.elegidos:  2592 ,semilla:  102191 ntree.limit:  510 total 808000 sample.total:  2693333 
## ids.elegidos:  2532 ,semilla:  102191 ntree.limit:  520 total 815000 sample.total:  2716667 

## con umbral 0.028

## ids.elegidos:  3712 ,semilla:  102191 ntree.limit:  400 total 728000 sample.total:  2426667
## ids.elegidos:  3484 ,semilla:  102191 ntree.limit:  410 total 761000 sample.total:  2536667
## ids.elegidos:  3269 ,semilla:  102191 ntree.limit:  420 total 790750 sample.total:  2635833
## ids.elegidos:  3134 ,semilla:  102191 ntree.limit:  430 total 824500 sample.total:  2748333
## ids.elegidos:  3003 ,semilla:  102191 ntree.limit:  440 total 841250 sample.total:  2804167
## ids.elegidos:  2883 ,semilla:  102191 ntree.limit:  450 total 823250 sample.total:  2744167
## ids.elegidos:  2798 ,semilla:  102191 ntree.limit:  460 total 836500 sample.total:  2788333
## ids.elegidos:  2702 ,semilla:  102191 ntree.limit:  470 total 836500 sample.total:  2788333
## ids.elegidos:  2630 ,semilla:  102191 ntree.limit:  480 total 846500 sample.total:  2821667
## ids.elegidos:  2573 ,semilla:  102191 ntree.limit:  490 total 844750 sample.total:  2815833
## ids.elegidos:  2516 ,semilla:  102191 ntree.limit:  500 total 819000 sample.total:  2730000
## ids.elegidos:  2449 ,semilla:  102191 ntree.limit:  510 total 827750 sample.total:  2759167
## ids.elegidos:  2392 ,semilla:  102191 ntree.limit:  520 total 842000 sample.total:  2806667

## con umbral 0.029

## ids.elegidos:  3467 ,semilla:  102191 ntree.limit:  400 total 757250 sample.total:  2524167 
## ids.elegidos:  3252 ,semilla:  102191 ntree.limit:  410 total 803000 sample.total:  2676667 
## ids.elegidos:  3078 ,semilla:  102191 ntree.limit:  420 total 830500 sample.total:  2768333 
## ids.elegidos:  2934 ,semilla:  102191 ntree.limit:  430 total 834500 sample.total:  2781667 
## ids.elegidos:  2820 ,semilla:  102191 ntree.limit:  440 total 823000 sample.total:  2743333 
## ids.elegidos:  2718 ,semilla:  102191 ntree.limit:  450 total 840500 sample.total:  2801667 
## ids.elegidos:  2621 ,semilla:  102191 ntree.limit:  460 total 832750 sample.total:  2775833 
## ids.elegidos:  2547 ,semilla:  102191 ntree.limit:  470 total 843250 sample.total:  2810833 
## ids.elegidos:  2478 ,semilla:  102191 ntree.limit:  480 total 836500 sample.total:  2788333 
## ids.elegidos:  2420 ,semilla:  102191 ntree.limit:  490 total 827000 sample.total:  2756667 
## ids.elegidos:  2354 ,semilla:  102191 ntree.limit:  500 total 827500 sample.total:  2758333 
## ids.elegidos:  2298 ,semilla:  102191 ntree.limit:  510 total 809500 sample.total:  2698333 
## ids.elegidos:  2251 ,semilla:  102191 ntree.limit:  520 total 813250 sample.total:  2710833

## con umbral 0.03

## ids.elegidos:  3251 ,ntree.limit:  400 total 795250 sample.total:  2650833 
## ids.elegidos:  3051 ,ntree.limit:  410 total 837250 sample.total:  2790833 
## ids.elegidos:  2901 ,ntree.limit:  420 total 826750 sample.total:  2755833 
## ids.elegidos:  2772 ,ntree.limit:  430 total 835000 sample.total:  2783333 
## ids.elegidos:  2665 ,ntree.limit:  440 total 853750 sample.total:  2845833 
## ids.elegidos:  2565 ,ntree.limit:  450 total 838750 sample.total:  2795833 
## ids.elegidos:  2479 ,ntree.limit:  460 total 828250 sample.total:  2760833 
## ids.elegidos:  2420 ,ntree.limit:  470 total 835000 sample.total:  2783333 
## ids.elegidos:  2351 ,ntree.limit:  480 total 820250 sample.total:  2734167 
## ids.elegidos:  2294 ,ntree.limit:  490 total 810500 sample.total:  2701667 
## ids.elegidos:  2236 ,ntree.limit:  500 total 809000 sample.total:  2696667 
## ids.elegidos:  2196 ,ntree.limit:  510 total 811000 sample.total:  2703333 
## ids.elegidos:  2155 ,ntree.limit:  520 total 813250 sample.total:  2710833 

## con umbral 0.03125

## ids.elegidos:  2994 ,semilla:  102191 ntree.limit:  400 total 827500 sample.total:  2758333 
## ids.elegidos:  2819 ,semilla:  102191 ntree.limit:  410 total 831250 sample.total:  2770833 
## ids.elegidos:  2699 ,semilla:  102191 ntree.limit:  420 total 829250 sample.total:  2764167 
## ids.elegidos:  2586 ,semilla:  102191 ntree.limit:  430 total 849500 sample.total:  2831667 
## ids.elegidos:  2488 ,semilla:  102191 ntree.limit:  440 total 850000 sample.total:  2833333 
## ids.elegidos:  2410 ,semilla:  102191 ntree.limit:  450 total 837500 sample.total:  2791667 
## ids.elegidos:  2333 ,semilla:  102191 ntree.limit:  460 total 824750 sample.total:  2749167 
## ids.elegidos:  2271 ,semilla:  102191 ntree.limit:  470 total 808250 sample.total:  2694167 
## ids.elegidos:  2206 ,semilla:  102191 ntree.limit:  480 total 824500 sample.total:  2748333 
## ids.elegidos:  2161 ,semilla:  102191 ntree.limit:  490 total 819750 sample.total:  2732500 
## ids.elegidos:  2110 ,semilla:  102191 ntree.limit:  500 total 816500 sample.total:  2721667 
## ids.elegidos:  2069 ,semilla:  102191 ntree.limit:  510 total 826750 sample.total:  2755833 
## ids.elegidos:  2020 ,semilla:  102191 ntree.limit:  520 total 823000 sample.total:  2743333


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

ymeans <- data.frame(x=x, y=rep(0, times=length(x)))
for (i in 1:length(x)) { ymeans[i, 2] <- mean(y1[i], y2[i], y3[i], y4[i], y5[i], y6[i]) }
plot(ymeans, type="l")
abline(h = max(ymeans$y))
## mejores picos (promedio de los umbrales entre 0.027 y 0.035, con semilla 102191): 460, 480, 490

## --------------------------------------------------------------------------------

## ids.elegidos:  2994 ,semilla:  102191 ntree.limit:  400 total 827500 sample.total:  2758333 
## ids.elegidos:  2819 ,semilla:  102191 ntree.limit:  410 total 831250 sample.total:  2770833 
## ids.elegidos:  2699 ,semilla:  102191 ntree.limit:  420 total 829250 sample.total:  2764167 
## ids.elegidos:  2586 ,semilla:  102191 ntree.limit:  430 total 849500 sample.total:  2831667 
## ids.elegidos:  2488 ,semilla:  102191 ntree.limit:  440 total 850000 sample.total:  2833333 
## ids.elegidos:  2410 ,semilla:  102191 ntree.limit:  450 total 837500 sample.total:  2791667 
## ids.elegidos:  2333 ,semilla:  102191 ntree.limit:  460 total 824750 sample.total:  2749167 
## ids.elegidos:  2271 ,semilla:  102191 ntree.limit:  470 total 808250 sample.total:  2694167 
## ids.elegidos:  2206 ,semilla:  102191 ntree.limit:  480 total 824500 sample.total:  2748333 
## ids.elegidos:  2161 ,semilla:  102191 ntree.limit:  490 total 819750 sample.total:  2732500 
## ids.elegidos:  2110 ,semilla:  102191 ntree.limit:  500 total 816500 sample.total:  2721667 
## ids.elegidos:  2069 ,semilla:  102191 ntree.limit:  510 total 826750 sample.total:  2755833 
## ids.elegidos:  2020 ,semilla:  102191 ntree.limit:  520 total 823000 sample.total:  2743333 
## ids.elegidos:  1989 ,semilla:  102191 ntree.limit:  530 total 814750 sample.total:  2715833 
## ids.elegidos:  1959 ,semilla:  102191 ntree.limit:  540 total 814250 sample.total:  2714167 
## ids.elegidos:  1934 ,semilla:  102191 ntree.limit:  550 total 812500 sample.total:  2708333 
## ids.elegidos:  3215 ,semilla:  200177 ntree.limit:  400 total 580250 sample.total:  1934167 
## ids.elegidos:  3030 ,semilla:  200177 ntree.limit:  410 total 618500 sample.total:  2061667 
## ids.elegidos:  2898 ,semilla:  200177 ntree.limit:  420 total 635500 sample.total:  2118333 
## ids.elegidos:  2765 ,semilla:  200177 ntree.limit:  430 total 636750 sample.total:  2122500 
## ids.elegidos:  2661 ,semilla:  200177 ntree.limit:  440 total 622750 sample.total:  2075833 
## ids.elegidos:  2574 ,semilla:  200177 ntree.limit:  450 total 636500 sample.total:  2121667 
## ids.elegidos:  2487 ,semilla:  200177 ntree.limit:  460 total 634250 sample.total:  2114167 
## ids.elegidos:  2416 ,semilla:  200177 ntree.limit:  470 total 620000 sample.total:  2066667 
## ids.elegidos:  2339 ,semilla:  200177 ntree.limit:  480 total 631250 sample.total:  2104167 
## ids.elegidos:  2270 ,semilla:  200177 ntree.limit:  490 total 648500 sample.total:  2161667 
## ids.elegidos:  2226 ,semilla:  200177 ntree.limit:  500 total 651500 sample.total:  2171667 
## ids.elegidos:  2173 ,semilla:  200177 ntree.limit:  510 total 664750 sample.total:  2215833 
## ids.elegidos:  2132 ,semilla:  200177 ntree.limit:  520 total 667000 sample.total:  2223333 
## ids.elegidos:  2093 ,semilla:  200177 ntree.limit:  530 total 668750 sample.total:  2229167 
## ids.elegidos:  2058 ,semilla:  200177 ntree.limit:  540 total 645500 sample.total:  2151667 
## ids.elegidos:  2028 ,semilla:  200177 ntree.limit:  550 total 653000 sample.total:  2176667 
## ids.elegidos:  3143 ,semilla:  410551 ntree.limit:  400 total 582250 sample.total:  1940833 
## ids.elegidos:  2968 ,semilla:  410551 ntree.limit:  410 total 618000 sample.total:  2060000 
## ids.elegidos:  2813 ,semilla:  410551 ntree.limit:  420 total 624750 sample.total:  2082500 
## ids.elegidos:  2703 ,semilla:  410551 ntree.limit:  430 total 636250 sample.total:  2120833 
## ids.elegidos:  2567 ,semilla:  410551 ntree.limit:  440 total 646250 sample.total:  2154167 
## ids.elegidos:  2478 ,semilla:  410551 ntree.limit:  450 total 652500 sample.total:  2175000 
## ids.elegidos:  2390 ,semilla:  410551 ntree.limit:  460 total 666500 sample.total:  2221667 
## ids.elegidos:  2322 ,semilla:  410551 ntree.limit:  470 total 651500 sample.total:  2171667 
## ids.elegidos:  2247 ,semilla:  410551 ntree.limit:  480 total 654250 sample.total:  2180833 
## ids.elegidos:  2187 ,semilla:  410551 ntree.limit:  490 total 637250 sample.total:  2124167 
## ids.elegidos:  2144 ,semilla:  410551 ntree.limit:  500 total 624000 sample.total:  2080000
## ids.elegidos:  2096 ,semilla:  410551 ntree.limit:  510 total 636000 sample.total:  2120000 
## ids.elegidos:  2056 ,semilla:  410551 ntree.limit:  520 total 630000 sample.total:  2100000 
## ids.elegidos:  2030 ,semilla:  410551 ntree.limit:  530 total 628500 sample.total:  2095000 
## ids.elegidos:  1996 ,semilla:  410551 ntree.limit:  540 total 637000 sample.total:  2123333 
## ids.elegidos:  1963 ,semilla:  410551 ntree.limit:  550 total 637250 sample.total:  2124167 
## ids.elegidos:  3159 ,semilla:  552581 ntree.limit:  400 total 538250 sample.total:  1794167 
## ids.elegidos:  2996 ,semilla:  552581 ntree.limit:  410 total 563000 sample.total:  1876667 
## ids.elegidos:  2852 ,semilla:  552581 ntree.limit:  420 total 575000 sample.total:  1916667 
## ids.elegidos:  2717 ,semilla:  552581 ntree.limit:  430 total 592750 sample.total:  1975833 
## ids.elegidos:  2635 ,semilla:  552581 ntree.limit:  440 total 605250 sample.total:  2017500 
## ids.elegidos:  2534 ,semilla:  552581 ntree.limit:  450 total 622500 sample.total:  2075000 
## ids.elegidos:  2453 ,semilla:  552581 ntree.limit:  460 total 634750 sample.total:  2115833 
## ids.elegidos:  2378 ,semilla:  552581 ntree.limit:  470 total 629500 sample.total:  2098333 
## ids.elegidos:  2304 ,semilla:  552581 ntree.limit:  480 total 632000 sample.total:  2106667 
## ids.elegidos:  2246 ,semilla:  552581 ntree.limit:  490 total 646500 sample.total:  2155000 
## ids.elegidos:  2186 ,semilla:  552581 ntree.limit:  500 total 637500 sample.total:  2125000 
## ids.elegidos:  2141 ,semilla:  552581 ntree.limit:  510 total 648750 sample.total:  2162500 
## ids.elegidos:  2097 ,semilla:  552581 ntree.limit:  520 total 651750 sample.total:  2172500 
## ids.elegidos:  2068 ,semilla:  552581 ntree.limit:  530 total 627000 sample.total:  2090000 
## ids.elegidos:  2045 ,semilla:  552581 ntree.limit:  540 total 648750 sample.total:  2162500 
## ids.elegidos:  2004 ,semilla:  552581 ntree.limit:  550 total 651000 sample.total:  2170000 
## ids.elegidos:  3133 ,semilla:  892237 ntree.limit:  400 total 600750 sample.total:  2002500 
## ids.elegidos:  2947 ,semilla:  892237 ntree.limit:  410 total 639250 sample.total:  2130833 
## ids.elegidos:  2794 ,semilla:  892237 ntree.limit:  420 total 645500 sample.total:  2151667 
## ids.elegidos:  2667 ,semilla:  892237 ntree.limit:  430 total 653250 sample.total:  2177500 
## ids.elegidos:  2573 ,semilla:  892237 ntree.limit:  440 total 652750 sample.total:  2175833 
## ids.elegidos:  2457 ,semilla:  892237 ntree.limit:  450 total 673750 sample.total:  2245833 
## ids.elegidos:  2365 ,semilla:  892237 ntree.limit:  460 total 672750 sample.total:  2242500 
## ids.elegidos:  2299 ,semilla:  892237 ntree.limit:  470 total 681250 sample.total:  2270833 
## ids.elegidos:  2229 ,semilla:  892237 ntree.limit:  480 total 698750 sample.total:  2329167 
## ids.elegidos:  2184 ,semilla:  892237 ntree.limit:  490 total 694000 sample.total:  2313333 
## ids.elegidos:  2139 ,semilla:  892237 ntree.limit:  500 total 697250 sample.total:  2324167 
## ids.elegidos:  2103 ,semilla:  892237 ntree.limit:  510 total 682250 sample.total:  2274167 
## ids.elegidos:  2072 ,semilla:  892237 ntree.limit:  520 total 690000 sample.total:  2300000 
## ids.elegidos:  2042 ,semilla:  892237 ntree.limit:  530 total 689500 sample.total:  2298333 
## ids.elegidos:  2003 ,semilla:  892237 ntree.limit:  540 total 675250 sample.total:  2250833 
## ids.elegidos:  1987 ,semilla:  892237 ntree.limit:  550 total 679250 sample.total:  2264167 


## fechas <- c()
## for (x in c(20180228,20160831,20170331,20150831,20160630,20170630,20160731,20160430,20180731,20151231,20170731,20160930,20170831,20180131,20161031,20160531,20171130,20180430,20170131,20150930,20151031,20180630,20180531,20170228,20180331,20161231,20170531,20171031,20171231,20080430,20151130,20161130,20170430,20170930,20130630,20160331,20121130,20080228,NA,20070831,20070731,20150731,20160228,20090831,20080630,20130331,20140630,20160131,20130131,20140331,20020630,20180831,20070331,20111231,20100831,20131130,20070630,20130831,20100131,20110331,20100731,20110630,20100630,20120131,20101231,20110430,20080930,20080831,20100331,20120430,20091130,20150630,20140731,20141130,20030731,20090731,20150228,20071031,20120331,20081031,20080531,20080131,20061130,20140831,20140531,20081231,99991231,20120930,20080731,20090331,20070430,20150531,20110228,20090228,20110531,20081130,20140930,20070131,20090531,20070531,20100228,20080331,20150430,20030630,20140228,20150131,20090630,20110831,20120731,20070930,20090131,20110731,20140430,20131231,20110131,20090930,20150331,20141031,20071130,20071231,20100430,20120228,20120630,20130430,20140131,20061231,20091231,20131031,20130228,20111031,20141231,20130731,20120831,20060131,20100930,20090430,20100531,20110930,20130531,20120531,20130930,20121031,20091031,20101031,20111130)) {
##     if (is.na(x)) {
##         fechas <- append(fechas, 999999999)
##     } else {
##         fechas <- append(fechas, as.numeric(as.Date("20150630", "%Y%m%d") - as.Date(as.character(x), "%Y%m%d")))
##     }
## }

