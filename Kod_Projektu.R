setwd("C:/Users/Kasper/OneDrive/Pulpit/Applied Statistics/mn_workshop")
getwd()
load("dane_zaliczenie.RData")

library(dplyr)
library(ggplot2)
library(gridExtra)
library(tree)
library(earth)
library(rpart)
library(rpart.plot)
library(adabag) 
library(randomForestSRC)
library(randomForest)
library(caTools)
library(pander)
library(wrapr)
library(adabag)
library(Hmisc)
library(tidyverse)
library(caret)
library(leaps)
library(kableExtra)

panderOptions("plain.ascii", TRUE)
panderOptions("keep.trailing.zeros", TRUE)
panderOptions("table.style", "simple")
perf_justify <- "lrrr"


### Przygotowanie danych do zadania 1.
set.seed(100) 
n <- nrow(proba_uczaca)
liczby_dzielace <- sample(c(1:n), round(0.75*n), replace = FALSE)

nowa_uczaca <- proba_uczaca[liczby_dzielace,]
proba_walidacyjna <- proba_uczaca[-liczby_dzielace,]

# WSTEPNA EKSPLORACJA DANYCH - proba uczaca
head(nowa_uczaca)
tail(nowa_uczaca)
#typy zmiennych
str(nowa_uczaca)
#transformacja zmiennych

nowa_uczaca$description <- ifelse(nowa_uczaca$description == "", "Brak opisu", nowa_uczaca$description)
nowa_uczaca$description <- ifelse(is.na(nowa_uczaca$description), "Brak opisu", nowa_uczaca$description)

nowa_uczaca <- nowa_uczaca %>%
  select(id, createtime, amount, description, recurringaction, acquirerconnectionmethod, expirymonth, expiryyear, issuer, 
         type, level, countrycode, listtype, mccname, status) %>%
  filter(recurringaction == "AUTO") %>%
  mutate(acquirerconnectionmethod = factor(acquirerconnectionmethod),
         expirymonth = factor(expirymonth),
         description = factor(description),
         expiryyear = factor(expiryyear),
         issuer = factor(issuer),
         type = factor(type),
         level = factor(level),
         countrycode = factor(countrycode),
         listtype = factor(listtype),
         mccname = factor(mccname))

nowa_uczaca$recurringaction <- NULL

#podstawowe statystyki
summary(nowa_uczaca)

#utworzenie nowych zmiennych i modyfikacja istniej?cych
nowa_uczaca$weekDay <- weekdays(as.Date(nowa_uczaca$createtime))
nowa_uczaca$weekDay <- as.factor(nowa_uczaca$weekDay)
nowa_uczaca$status <- ifelse(nowa_uczaca$status == "completed successfully", "success", "fail")
nowa_uczaca$status <- as.factor(nowa_uczaca$status)

nowa_uczaca$weekDay <- factor(nowa_uczaca$weekDay, levels= c("poniedziałek", "wtorek", 
                                         "środa", "czwartek", "piątek", "sobota", "niedziela"))

str(nowa_uczaca)

#podstawowe statystyki
summary(nowa_uczaca)

#wizualizacja zmiennych

#Status
plotStat <- nowa_uczaca %>%
  count(status) %>%
  ggplot() + geom_col(aes(x = status, y = n, fill = status), alpha = 0.8) +
  geom_label(aes(x = status, y = n, label = n)) +
  scale_fill_manual(values = c("tomato3","royalblue2")) +
  theme_minimal() +
  ggtitle('Liczba nieudanych i udanych transakcji') + 
  theme(axis.title.y = element_text(color="Grey23", size=16),
        axis.text.y = element_text(size=10),
        axis.title.x=element_blank(),
        legend.title = element_text(size=12),
        legend.text = element_text(size=8),
        legend.position = "right",
        legend.justification = c(0.94,0.94),
        legend.background = element_rect(fill="gray90",
                                         size=0.5, linetype="solid", 
                                         colour ="gray10"),
        plot.title = element_text(color="gray10", size=22, family="serif"))

plotStat

#Type
plotType <- nowa_uczaca %>%
  count(type) %>%
  ggplot() + geom_col(aes(x = type, y = n, fill = type), alpha = 0.8) +
  geom_label(aes(x = type, y = n, label = n)) +
  scale_fill_manual(values = c("deepskyblue2","darkorange2")) +
  theme_minimal() +
  ggtitle('Liczba transakcji w zale?nożci od typu karty') + 
  theme(axis.title.y = element_text(color="Grey23", size=16),
        axis.text.y = element_text(size=10),
        axis.title.x=element_blank(),
        legend.position = "none",
        plot.title = element_text(color="gray10", size=22, family="serif"))

#Expiryyear
plotExpy <- nowa_uczaca %>%
  count(expiryyear) %>%
  ggplot() + geom_col(aes(x = expiryyear, y = n, fill = expiryyear), alpha = 0.8) +
  geom_label(aes(x = expiryyear, y = n, label = n)) +
  scale_fill_manual(values = c("violetred4","seagreen4", "goldenrod4", "lightskyblue4", "sienna4",
                               "violetred2","seagreen2", "goldenrod2", "lightskyblue2", "sienna2")) +
  theme_minimal() +
  ggtitle("Rok utraty ważności karty") + 
  theme(axis.title.y = element_text(color="Grey23", size=16),
        axis.text.y = element_text(size=10),
        axis.title.x=element_blank(),
        legend.position = "none",
        plot.title = element_text(color="gray10", size=22, family="serif"))

#weekDay
plotWeekd <- nowa_uczaca %>%
  count(weekDay) %>%
  ggplot() + geom_col(aes(x = weekDay, y = n, fill = weekDay), alpha = 0.8) +
  geom_label(aes(x = weekDay, y = n, label = n)) +
  scale_fill_manual(values = c("gray86","firebrick3", "darkseagreen4", "yellow2", "pink3",
                               "mediumpurple4","orange3")) +
  theme_minimal() +
  ggtitle("Dzień dokonania transakcji") + 
  theme(axis.title.y = element_text(color="Grey23", size=16),
        axis.text.y = element_text(size=10),
        axis.title.x=element_blank(),
        legend.position = "none",
        plot.title = element_text(color="gray10", size=22, family="serif"))

grid.arrange(plotType, plotExpy, plotWeekd)

#mccname
plotMccname <- nowa_uczaca %>%
  count(mccname) %>%
  ggplot() + geom_col(aes(x = mccname, y = n, fill = mccname), alpha = 0.7) +
  geom_label(aes(x = mccname, y = n, label = n)) +
  scale_fill_manual(values = c("deeppink3", "gray50", "blue3", "gray50", "gray50", "green4", "red2")) +
  theme_minimal() +
  ggtitle("Mccname") + 
  theme(axis.title.y = element_text(color="Grey23", size=16),
        axis.text.y = element_text(size=10),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        legend.title = element_text(size=8),
        legend.text = element_text(size=6),
        legend.position = "right",
        legend.justification = c(0.8,0.8),
        legend.background = element_rect(fill="gray90",
                                         size=0.5, linetype="solid", 
                                         colour ="gray10"),
        plot.title = element_text(color="gray10", size=19, family="serif"))

#issuer
plotIssuer <- nowa_uczaca %>%
  count(issuer) %>%
  ggplot() + geom_col(aes(x = issuer, y = n, fill = issuer), alpha = 0.7) +
  geom_label(aes(x = issuer, y = n, label = n)) +
  scale_fill_manual(values = c("dodgerblue2", "chocolate2", "slateblue3")) +
  theme_minimal() +
  ggtitle("Issuer") + 
  theme(axis.title.y = element_text(color="Grey23", size=16),
        axis.text.y = element_text(size=10),
        axis.title.x = element_blank(),
        legend.position = "none",
        plot.title = element_text(color="gray10", size=19, family="serif"))

#listtype
plotListt <- nowa_uczaca %>%
  count(listtype) %>%
  ggplot() + geom_col(aes(x = listtype, y = n, fill = listtype), alpha = 0.7) +
  geom_label(aes(x = listtype, y = n, label = n)) +
  scale_fill_manual(values = c("seagreen3", "gold3", "indianred3")) +
  theme_minimal() +
  ggtitle("Listtype") + 
  theme(axis.title.y = element_text(color="Grey23", size=16),
        axis.text.y = element_text(size=10),
        axis.title.x = element_blank(),
        legend.position = "none",
        plot.title = element_text(color="gray10", size=19, family="serif"))

grid.arrange(plotMccname, plotIssuer, plotListt)

#Histogram dla zmiennej amount
plotAmount <- ggplot(nowa_uczaca, aes(x = amount)) + geom_histogram(binwidth = 200, alpha = 0.7, fill = "darkorange3") + theme_minimal() +
  ggtitle('Rozkład zmiennej amount') + 
  theme(axis.title.y = element_text(color="Grey23", size=11),
        axis.text.y = element_text(size=9),
        axis.title.x = element_text(color="Grey23", size=11),
        axis.text.x = element_text(size=9),
        plot.title = element_text(color="gray10", size=19, family="serif"))

#Histogram dla zmiennej amount + status
plotAmStat <- ggplot(nowa_uczaca, aes(x = amount, fill=status)) + geom_histogram(binwidth = 200, alpha = 0.7) + facet_grid(~status) + theme_minimal() +
  scale_fill_manual(values = c("tomato3","royalblue2")) + 
  ggtitle('Rozkład zmiennej amount z podziałem na status transakcji') + 
  theme(axis.title.y = element_text(color="Grey23", size=11),
        axis.text.y = element_text(size=9),
        axis.title.x = element_text(color="Grey23", size=11),
        axis.text.x = element_text(size=9),
        legend.position = "none",
        plot.title = element_text(color="gray10", size=19, family="serif"))

#Histogram dla zmiennej amount + type
plotAmTyp <- ggplot(nowa_uczaca, aes(x = amount, fill=type)) + geom_histogram(binwidth = 200, alpha = 0.7) + facet_grid(~type) + theme_minimal() +
  scale_fill_manual(values = c("deepskyblue2","darkorange2")) + 
  ggtitle('Rozkład zmiennej amount z podziałem na typ karty') + 
  theme(axis.title.y = element_text(color="Grey23", size=11),
        axis.text.y = element_text(size=9),
        axis.title.x = element_text(color="Grey23", size=11),
        axis.text.x = element_text(size=9),
        legend.position = "none",
        plot.title = element_text(color="gray10", size=19, family="serif"))

grid.arrange(plotAmount, plotAmStat, plotAmTyp)

#Histogram dla zmiennej amount + listtype

doHist <- nowa_uczaca[nowa_uczaca$listtype != "PUBLIC" & nowa_uczaca$issuer != "MAESTRO",]

plotAmListt <- ggplot(doHist, aes(x = amount, fill=listtype)) + geom_histogram(binwidth = 200, alpha = 0.7) + facet_grid(~listtype) + theme_minimal() +
  scale_fill_manual(values = c("seagreen3", "gold3")) + 
  ggtitle('Rozkład zmiennej amount z podziałem sektor działania partnera') + 
  theme(axis.title.y = element_text(color="Grey23", size=11),
        axis.text.y = element_text(size=9),
        axis.title.x = element_text(color="Grey23", size=11),
        axis.text.x = element_text(size=9),
        legend.position = "none",
        plot.title = element_text(color="gray10", size=19, family="serif"))

#Histogram dla zmiennej amount + issuer
plotAmIss <- ggplot(doHist, aes(x = amount, fill=issuer)) + geom_histogram(binwidth = 200, alpha = 0.7) + facet_grid(~issuer) + theme_minimal() +
  scale_fill_manual(values = c("chocolate2", "slateblue3")) + 
  ggtitle('Rozkład zmiennej amount z podziałem na wystawcę karty') + 
  theme(axis.title.y = element_text(color="Grey23", size=11),
        axis.text.y = element_text(size=9),
        axis.title.x = element_text(color="Grey23", size=11),
        axis.text.x = element_text(size=9),
        legend.position = "none",
        plot.title = element_text(color="gray10", size=19, family="serif"))

grid.arrange(plotAmListt, plotAmIss)

# WSTEPNA EKSPLORACJA DANYCH - proba walidacyjna
head(proba_walidacyjna)
tail(proba_walidacyjna)
#typy zmiennych
str(proba_walidacyjna)
#transformacja zmiennych

proba_walidacyjna$description <- ifelse(proba_walidacyjna$description == "", "Brak opisu", proba_walidacyjna$description)
proba_walidacyjna$description <- ifelse(is.na(proba_walidacyjna$description), "Brak opisu", proba_walidacyjna$description)

proba_walidacyjna <- proba_walidacyjna %>%
  select(id, createtime, amount, description, recurringaction, acquirerconnectionmethod, expirymonth, expiryyear, issuer, 
         type, level, countrycode, listtype, mccname, status) %>%
  filter(recurringaction == "AUTO") %>%
  mutate(acquirerconnectionmethod = factor(acquirerconnectionmethod),
         expirymonth = factor(expirymonth),
         description = factor(description),
         expiryyear = factor(expiryyear),
         issuer = factor(issuer),
         type = factor(type),
         level = factor(level),
         countrycode = factor(countrycode),
         listtype = factor(listtype),
         mccname = factor(mccname))

proba_walidacyjna$recurringaction <- NULL

#podstawowe statystyki
summary(proba_walidacyjna)

#utworzenie nowych zmiennych i modyfikacja istniej?cych
proba_walidacyjna$weekDay <- weekdays(as.Date(proba_walidacyjna$createtime))
proba_walidacyjna$weekDay <- as.factor(proba_walidacyjna$weekDay)
proba_walidacyjna$status <- ifelse(proba_walidacyjna$status == "completed successfully", "success", "fail")
proba_walidacyjna$status <- as.factor(proba_walidacyjna$status)

proba_walidacyjna$weekDay <- factor(proba_walidacyjna$weekDay, levels= c("poniedziałek", "wtorek", 
                                                              "środa", "czwartek", "piątek", "sobota", "niedziela"))

str(proba_walidacyjna)

#podstawowe statystyki
summary(proba_walidacyjna)


# WSTEPNA EKSPLORACJA DANYCH - proba testowa
head(proba_testowa)
tail(proba_testowa)
#typy zmiennych
str(proba_testowa)
#transformacja zmiennych

  
proba_testowa$description <- ifelse(proba_testowa$description == "", "Brak opisu", proba_testowa$description)
proba_testowa$description <- ifelse(is.na(proba_testowa$description), "Brak opisu", proba_testowa$description)

proba_testowa <- proba_testowa %>%
  select(id, createtime, amount, description, recurringaction, acquirerconnectionmethod, expirymonth, expiryyear, issuer, 
         type, level, countrycode, listtype, mccname) %>%
  filter(recurringaction == "AUTO") %>%
  mutate(acquirerconnectionmethod = factor(acquirerconnectionmethod),
         expirymonth = factor(expirymonth),
         description = factor(description),
         expiryyear = factor(expiryyear),
         issuer = factor(issuer),
         type = factor(type),
         level = factor(level),
         countrycode = factor(countrycode),
         listtype = factor(listtype),
         mccname = factor(mccname))

proba_testowa$recurringaction <- NULL


#podstawowe statystyki
summary(proba_testowa)

#utworzenie nowych zmiennych i modyfikacja istniej?cych
proba_testowa$weekDay <- weekdays(as.Date(proba_testowa$createtime))
proba_testowa$weekDay <- as.factor(proba_testowa$weekDay)
proba_testowa$weekDay <- factor(proba_testowa$weekDay, levels= c("poniedziałek", "wtorek", 
                                                               "środa", "czwartek", "piątek", "sobota", "niedziela"))

str(proba_testowa)

#podstawowe statystyki
summary(proba_testowa)

#Ujednolicenie zmiennych typu factor
proba_testowa$expiryyear <- factor(proba_testowa$expiryyear, levels = levels(nowa_uczaca$expiryyear))
proba_testowa$description <- factor(proba_testowa$description, levels = levels(nowa_uczaca$description))
proba_testowa$expirymonth  <- factor(proba_testowa$expirymonth , levels = levels(nowa_uczaca$expirymonth))
proba_testowa$issuer <- factor(proba_testowa$issuer, levels = levels(nowa_uczaca$issuer))
proba_testowa$level <- factor(proba_testowa$level, levels = levels(nowa_uczaca$level))
proba_testowa$countrycode <- factor(proba_testowa$countrycode, levels = levels(nowa_uczaca$countrycode))
proba_testowa$mccname <- factor(proba_testowa$mccname, levels = levels(nowa_uczaca$mccname))
proba_testowa$weekDay <- factor(proba_testowa$weekDay, levels = levels(nowa_uczaca$weekDay))
proba_testowa$listtype <- factor(proba_testowa$listtype, levels = levels(nowa_uczaca$listtype))
proba_testowa$acquirerconnectionmethod <- factor(proba_testowa$acquirerconnectionmethod, levels = levels(nowa_uczaca$acquirerconnectionmethod))
proba_testowa$type <- factor(proba_testowa$type, levels = levels(nowa_uczaca$type))

proba_walidacyjna$expiryyear <- factor(proba_walidacyjna$expiryyear, levels = levels(nowa_uczaca$expiryyear))
proba_walidacyjna$description <- factor(proba_walidacyjna$description, levels = levels(nowa_uczaca$description))
proba_walidacyjna$expirymonth  <- factor(proba_walidacyjna$expirymonth , levels = levels(nowa_uczaca$expirymonth))
proba_walidacyjna$issuer <- factor(proba_walidacyjna$issuer, levels = levels(nowa_uczaca$issuer))
proba_walidacyjna$level <- factor(proba_walidacyjna$level, levels = levels(nowa_uczaca$level))
proba_walidacyjna$countrycode <- factor(proba_walidacyjna$countrycode, levels = levels(nowa_uczaca$countrycode))
proba_walidacyjna$mccname <- factor(proba_walidacyjna$mccname, levels = levels(nowa_uczaca$mccname))
proba_walidacyjna$weekDay <- factor(proba_walidacyjna$weekDay, levels = levels(nowa_uczaca$weekDay))
proba_walidacyjna$listtype <- factor(proba_walidacyjna$listtype, levels = levels(nowa_uczaca$listtype))
proba_walidacyjna$acquirerconnectionmethod <- factor(proba_walidacyjna$acquirerconnectionmethod, levels = levels(nowa_uczaca$acquirerconnectionmethod))
proba_walidacyjna$type <- factor(proba_walidacyjna$type, levels = levels(nowa_uczaca$type))

proba_testowa$amount <- NULL
nowa_uczaca <- na.omit(nowa_uczaca)
proba_walidacyjna <- na.omit(proba_walidacyjna)
proba_testowa <- na.omit(proba_testowa)

summary(nowa_uczaca)
summary(proba_walidacyjna)
summary(proba_testowa)

proba_testowa_final <- proba_testowa

nowa_uczaca$amount <- NULL
proba_walidacyjna$amount <- NULL


# ZADANIE 1. Budowanie modelu w oparciu o zmienną jakościową "status"

#Funkcja do obliczania logarytmu wiarygodności

loglikelihood <- function(y, py) {
  pysmooth <- ifelse(py == 0, 1e-12,
                     ifelse(py == 1, 1 - 1e-12, py))
  
  sum(y * log(pysmooth) + (1 - y) * log(1 - pysmooth))

}

#Funkcja obliczająca i zwracająca znormalizowaną dewiancję, dokładność prefykcyjną i wynik f1

miaryDokladnosci <- function(pred, truth, name = "model") {
  dev.norm <- -2 * loglikelihood(as.numeric(truth), pred) / length(pred)
  ctable <- table(truth = truth,
                  pred = (pred > 0.5))
  accuracy <- sum(diag(ctable)) / sum(ctable)
  precision <- ctable[2,2] / sum(ctable[2,])
  recall <- ctable[2,2] / sum(ctable[2,])
  f1 <- 2 * precision * recall / (precision + recall)
  data.frame(model = name, accuracy = accuracy, f1 = f1, dev.norm)
}


#Budowa maksymalnego drzewa z cp = 0
statusTree <- rpart(formula = status ~ . - id - createtime, 
                data=nowa_uczaca,
                cp = 0, xval = 10)


#Wykres drzewa dla cp = 0
plot(statusTree)

#Błędy na bazie sprawdzania krzyżowego:
bledy <- printcp(statusTree) # sekwencja drzew do optymalnego przycinania

matplot(x = bledy[, "nsplit"],
        y = bledy[, c("rel error",  # błąd na próbie uczącej (w stosunku do błędu dla korzenia)
                      "xerror")],  # błąd w sprawdzaniu krzyżowym
        type = "l",
        xlab = "wielkość drzewa",
        ylab = "błąd")
legend(x = "topright", legend = c("błąd na próbie uczącej", 
                                  "błąd w sprawdzaniu krzyżowym"),
       col = c("black", "red"),
       lty = 1:2)

statusTree$cptable
plotcp(statusTree)


bledy <- statusTree$cptable
tmp1 <- which.min(bledy[, "xerror"])  # min błąd w sprawdzaniu krzy?owym
tmp2 <- sum(bledy[tmp1, c("xerror", "xstd")]) # min błąd + odchylenie standardowe
optymalny <- which(bledy[, "xerror"] < tmp2)[1] # nr optymalnego drzewa

statusTree.p <- prune(statusTree, cp = bledy[optymalny, "CP"]) # przycięcie drzewa, p = przyciecie

statusTree.p$cptable

#Wykres drzewa dla optymalnego parametru cp
plot(statusTree.p)

# Odczytać ważność zmiennych
statusTree.p$variable.importance
cbind(statusTree.p$variable.importance)
dotchart(rev(statusTree$variable.importance))

#Model tylko z najważniejszymi zmiennymi
impr_uczaca <- nowa_uczaca[,c(13,9,3,5,12,10,4)]

#MODEL DLA ZMIENNYCH WAŻNYCH
#Budowa maksymalnego drzewa z cp = 0
new.statusTree <- rpart(formula = status ~ ., 
                    data=impr_uczaca,
                    cp = 0, xval = 10)

#Wykres drzewa dla cp = 0 
plot(new.statusTree)

#Błędy na bazie sprawdzania krzy?owego:
bledy2 <- printcp(new.statusTree) # sekwencja drzew do optymalnego przycinania

matplot(x = bledy[, "nsplit"],
        y = bledy[, c("rel error",  # błąd na próbie uczącej (w stosunku do błędu dla korzenia)
                      "xerror")],  # błąd w sprawdzaniu krzyżowym
        type = "l",
        xlab = "wielkość drzewa",
        ylab = "błąd")
legend(x = "topright", legend = c("błąd na pr?bie ucz?cej", 
                                  "błąd w sprawdzaniu krzy?owym"),
       col = c("black", "red"),
       lty = 1:2)

new.statusTree$cptable
plotcp(new.statusTree)

bledy2 <- new.statusTree$cptable
tmp12 <- which.min(bledy2[, "xerror"])  # min b??d w sprawdzaniu krzy?owym
tmp22 <- sum(bledy2[tmp12, c("xerror", "xstd")]) # min b??d + odchylenie standardowe
optymalny2 <- which(bledy2[, "xerror"] < tmp22)[1] # nr optymalnego drzewa

new.statusTree.prune <- prune(new.statusTree, cp = bledy2[optymalny2, "CP"]) # przycięcie drzewa, p = przyciecie


#Wykres drzewa dla optymalnego parametru cp
plot(new.statusTree.prune)


#PREDYKCJE

pred.prune.stat.tree.train <- predict(object = new.statusTree.prune,
                             newdata = nowa_uczaca)[,2]

pred.prune.stat.tree.walid <- predict(object = new.statusTree.prune,
                     newdata = proba_walidacyjna)[,2]

trainOcen <- miaryDokladnosci(pred.prune.stat.tree.train,
                              nowa_uczaca$status == "success",
                              name = "drzewo, uczacy - ważne zmienne")

walidOcen <- miaryDokladnosci(pred.prune.stat.tree.walid,
                              proba_walidacyjna$status == "success",
                              name = "drzewo, walidacyjny - ważne zmienne")

predykcjaStatTreeTrain.all <- predict(object = statusTree.p,
                                  newdata = nowa_uczaca)[,2]

predykcjaStatTreeWalid.all <- predict(object = statusTree.p,
                                  newdata = proba_walidacyjna)[,2]

trainOcen.all <- miaryDokladnosci(predykcjaStatTreeTrain.all,
                              nowa_uczaca$status == "success",
                              name = "drzewo, uczacy - wszystkie zmienne")

walidOcen.all <- miaryDokladnosci(predykcjaStatTreeWalid.all,
                              proba_walidacyjna$status == "success",
                              name = "drzewo, walidacyjny - wszystkie zmienne")

perftable <- rbind(trainOcen.all, walidOcen.all, trainOcen, walidOcen)
pandoc.table(perftable, justify = perf_justify)

### BŁAD KLASYFIKACYJNY DLA PRÓBY WALIDACYJNEJ###

err.imp.var.class <- sum(predict(object = new.statusTree.prune,
            newdata = proba_walidacyjna,
            type = "class") != proba_walidacyjna$status) / nrow(proba_walidacyjna)
err.imp.var.class

err.all.var.class <- sum(predict(object = statusTree.p,
            newdata = proba_walidacyjna,
            type = "class") != proba_walidacyjna$status) / nrow(proba_walidacyjna)
err.all.var.class


### BŁAD REDYSTRYBUCJI DLA PRÓBY UCZĄCEJ###

err.res.imp.var <- mean(predict(object = new.statusTree.prune,
                     newdata = nowa_uczaca, type = 'class') != nowa_uczaca$status) 
err.res.imp.var

err.res.all.var <- mean(predict(object = statusTree.p,
             newdata = nowa_uczaca, type = 'class') != nowa_uczaca$status) 
err.res.all.var

#Macierz blednych klasyfikacji (zbior uczacy)
tabKlasWalidImp <- table(rzeczywiste = proba_walidacyjna$status, 
               prognozowane = predict(object = new.statusTree.prune,
                                      newdata = proba_walidacyjna,
                                      type = "class"))
tabKlasWalidImp


tabKlasTrainImp <- table(rzeczywiste = nowa_uczaca$status, 
                      prognozowane = predict(object = new.statusTree.prune,
                                             newdata = nowa_uczaca,
                                             type = "class"))
tabKlasTrainImp

tabKlasWalidAll <- table(rzeczywiste = proba_walidacyjna$status, 
                      prognozowane = predict(object = statusTree.p,
                                             newdata = proba_walidacyjna,
                                             type = "class"))
tabKlasWalidAll

tabKlasTrainAll <- table(rzeczywiste = nowa_uczaca$status, 
                      prognozowane = predict(object = statusTree.p,
                                             newdata = nowa_uczaca,
                                             type = "class"))
tabKlasTrainAll


### Budowa modelu metodą BAGGING i BOOSTING

#BAGGING
bagg.status.tree.train <- bagging(status ~ . - id - createtime, data = nowa_uczaca, mfinal = 30, maxdepth=5)
bagg.status.tree.walid <- bagging(status ~ . - id - createtime, data = proba_walidacyjna, mfinal = 30, maxdepth=5)

rpart.plot(bagg.status.tree.train$trees[[2]], roundint = FALSE)

plot.errorevol(
  errorevol(object = bagg.status.tree.train, newdata = proba_walidacyjna),
  errorevol(object = bagg.status.tree.walid, newdata = nowa_uczaca))

grid()

dotchart(rev(bagg.status.tree.train$importance))

bagg.status.tree.pred <- predict.bagging(bagg.status.tree.train, newdata = proba_walidacyjna)
print(bagg.status.tree.pred)
bagg.status.tree.pred$confusion
bagg.status.tree.pred$confusion <- as.table(bagg.status.tree.pred$confusion)
err.bagg.class <- bagg.status.tree.pred$error
err.bagg.class

mfinal.min <- which.min(errorevol(object = bagg.status.tree.walid, newdata = proba_walidacyjna)$error)

bagg.status.tree.train <- bagging(status ~ . - id - createtime, data = nowa_uczaca, mfinal = mfinal.min, maxdepth=5)

min.bagg.status.tree.pred <- predict.bagging(bagg.status.tree.train, newdata = proba_walidacyjna)
print(min.bagg.status.tree.pred)
min.bagg.status.tree.pred$confusion
min.bagg.status.tree.pred$error


czyMniejszy <- function(x,y){
  if (x < y){
    final.bagg.error <- x}
    
  else {
    final.bagg.error <- y }
  
  final.bagg.error
}

final.bagg.error.score <- czyMniejszy(min.bagg.status.tree.pred$error, err.bagg.class)


czyMniejszy2 <- function(x,y){
  if (min.bagg.status.tree.pred$error < err.bagg.class){
    final.bagg.conf <- x}
  
  else {
    final.bagg.conf <- y }
  
  final.bagg.conf <- as.table(final.bagg.conf)
  final.bagg.conf
}

final.bagg.conf.score <- czyMniejszy2(min.bagg.status.tree.pred$confusion, bagg.status.tree.pred$confusion)


#BOOSTING
boost.status.tree.train <- boosting(status ~ . - id - createtime, data = nowa_uczaca, mfinal = 20)
boost.status.tree.walid <- boosting(status ~ . - id - createtime, data = proba_walidacyjna, mfinal = 20)

plot.errorevol(
  errorevol(object = boost.status.tree.train, newdata = nowa_uczaca),
  errorevol(object = boost.status.tree.walid, newdata = proba_walidacyjna))

grid()

dotchart(rev(boost.status.tree.train$importance))

boost.status.tree.pred <- predict.boosting(boost.status.tree.train, newdata = proba_walidacyjna)
print(boost.status.tree.pred)
boost.status.tree.pred$confusion
boost.status.tree.pred$confusion <- as.table(boost.status.tree.pred$confusion)
err.boost.class <- boost.status.tree.pred$error
err.boost.class


### Budowa modelu metodą RANDOM FOREST

status.fr <- randomForest(status ~. - id - createtime, data=nowa_uczaca, do.trace = 10, ntree=100, keep.forest = TRUE)

print(status.fr)
status.fr$err.rate


# Odczytać wartości zmiennych
status.fr$importance

#predykcja na zbiorze walidacyjnym
predykcje.rf.walid <- predict(status.fr, newdata = proba_walidacyjna, block.size = 1)

predykcje.rf.walid

#porównanie zbioru uczacego i testowego 
x.test <- proba_walidacyjna[,c(-1,-2,-13)]
y.test <- proba_walidacyjna$status
status.fr2 <- randomForest(status ~. - id - createtime, data=nowa_uczaca, xtest= x.test, 
                           ytest=y.test, ntree = 100)

varImpPlot(status.fr2,
           sort = T,
           main = "Ważność zmiennych dla modelu Random Forest")

status.fr2$confusion <- as.table(status.fr2$confusion)
status.tf.pred.table <- as.table(confusionMatrix(predykcje.rf.walid, proba_walidacyjna$status)$table)
  

bl.oob <- status.fr2$err.rate[,1]
bl.test <- status.fr2$test$err.rate[,1]
#wykres
plot(1:100, ylim=c(min(bl.test, bl.oob), max(bl.test, bl.oob)),
     xlab="Liczba drzew", ylab="Błąd klasyfikacji", type='n')
points(1:100, bl.oob, col="orange", pch=18)
points(1:100, bl.test, col="blue", pch=16)
lines(1:100, bl.oob, col="orange")
lines(1:100, bl.test, col="blue")
leg <- c("zbiór testowy", "OOB")
legend('topright', legend=leg, col=c("blue","orange"), lty=1, pch=20)

err.rf.status.train <- tail(status.fr2$err.rate,1)[1]
err.rf.status.test <- (status.tf.pred.table[1,2] + status.tf.pred.table[2,1]) / nrow(proba_walidacyjna)

#Porównanie błędów klasyfikacyjnych
bledy.podsumowanie <- cbind(c("drzewo klasyfikacyjne ze wszystkimi zmiennymi" = err.all.var.class,
        "drzewo klasyfikacyjne tylko z najważniejszymi zmiennymi" = err.imp.var.class,
        "bagging" = final.bagg.error.score,
        "boosting" = err.boost.class,
        "random forest OOB" = err.rf.status.train,
        "random forest test" = err.rf.status.test))


bledy.podsumowanie = data.frame(
  col1 = c("drzewo klasyfikacyjne ze wszystkimi zmiennymi", "drzewo klasyfikacyjne tylko z najważniejszymi zmiennymi", 
           "bagging", "boosting","random forest train", 'random forest valid'),
  col2 = c(err.all.var.class, err.imp.var.class, final.bagg.error.score,  err.boost.class, err.rf.status.train, err.rf.status.test))

colnames(bledy.podsumowanie) <- c("Model", "Wartość błędu klasyfikacyjnego")
bledy.podsumowanie

bledy.podsumowanie %>%
  kbl(caption = "Wartosć błędów klasyfikacyjnych dla poszczególnych modeli") %>%
  kable_paper("hover", full_width = F)


#SPRAWDZENIE MIAR DLA MODELI KLASYFIKACYJNCH
podsumowanieMiar <- function(miary) {
  TP <- miary["success","success"]
  TN <- miary["fail", "fail"]
  FP <- miary["fail", "success"]
  FN <- miary["success", "fail"]
  
  c(precyzja = TP/(TP + FP),
    czułość = TP/(TP + FN),
    specyficzność = TN/(TN + FP),
    F.miara = 2*TP / (2*TP + FP + FN))
}

miary.koncowe <- cbind(
"Drzewo klasyfikacyjne dla zbioru uczącego i wszystkich zmiennych" = podsumowanieMiar(tabKlasTrainImp),
"Drzewo klasyfikacyjne dla zbioru walidacyjnego i wszystkich zmiennych" = podsumowanieMiar(tabKlasWalidImp),
"Drzewo klasyfikacyjne dla zbioru uczącego i wybranych zmiennych" = podsumowanieMiar(tabKlasTrainAll),
"Drzewo klasyfikacyjne dla zbioru walidacyjnego i wszystkich zmiennych" = podsumowanieMiar(tabKlasWalidAll),
"Model boosting" = podsumowanieMiar(boost.status.tree.pred$confusion),
"Model bagging" = podsumowanieMiar(final.bagg.conf.score),
"Model random forest dla proby uczacej" = podsumowanieMiar(status.fr2$confusion),
"Model random forest dla próby walidacyjnej" = podsumowanieMiar(status.tf.pred.table))
miary.koncowe

vec.prec <- c(podsumowanieMiar(tabKlasTrainImp)[1], podsumowanieMiar(tabKlasWalidImp)[1], podsumowanieMiar(tabKlasTrainAll)[1],
              podsumowanieMiar(tabKlasWalidAll)[1], podsumowanieMiar(boost.status.tree.pred$confusion)[1], podsumowanieMiar(final.bagg.conf.score)[1],
              podsumowanieMiar(status.fr2$confusion)[1], podsumowanieMiar(status.tf.pred.table)[1])
vec.czul <- c(podsumowanieMiar(tabKlasTrainImp)[2], podsumowanieMiar(tabKlasWalidImp)[2], podsumowanieMiar(tabKlasTrainAll)[2],
              podsumowanieMiar(tabKlasWalidAll)[2], podsumowanieMiar(boost.status.tree.pred$confusion)[2], podsumowanieMiar(final.bagg.conf.score)[2],
              podsumowanieMiar(status.fr2$confusion)[2], podsumowanieMiar(status.tf.pred.table)[2])
vec.spec <- c(podsumowanieMiar(tabKlasTrainImp)[3], podsumowanieMiar(tabKlasWalidImp)[3], podsumowanieMiar(tabKlasTrainAll)[3],
              podsumowanieMiar(tabKlasWalidAll)[3], podsumowanieMiar(boost.status.tree.pred$confusion)[3], podsumowanieMiar(final.bagg.conf.score)[3],
              podsumowanieMiar(status.fr2$confusion)[3], podsumowanieMiar(status.tf.pred.table)[3])
vec.f <- c(podsumowanieMiar(tabKlasTrainImp)[4], podsumowanieMiar(tabKlasWalidImp)[4], podsumowanieMiar(tabKlasTrainAll)[4],
           podsumowanieMiar(tabKlasWalidAll)[4], podsumowanieMiar(boost.status.tree.pred$confusion)[4], podsumowanieMiar(final.bagg.conf.score)[4],
           podsumowanieMiar(status.fr2$confusion)[4], podsumowanieMiar(status.tf.pred.table)[4])


podsumowanie.final <- data.frame("Precyzja" = vec.prec,
                                 "Czułość" = vec.czul,
                                 "Specyficzność" = vec.spec,
                                 "Miara F" = vec.f)


rownames(podsumowanie.final) <- c("Drzewo klasyfikacyjne dla zbioru uczącego i wszystkich zmiennych",
                                    "Drzewo klasyfikacyjne dla zbioru walidacyjnego i wszystkich zmiennych",
                                    "Drzewo klasyfikacyjne dla zbioru uczącego i wybranych zmiennych",
                                    "Drzewo klasyfikacyjne dla zbioru walidacyjnego i wybranych zmiennych",
                                    "Model boosting",
                                    "Model bagging",
                                    "Model random forest dla proby uczacej",
                                    "Model random forest dla próby walidacyjnej")


podsumowanie.final %>%
  kbl(caption = "Wartosć miar dla poszczególnych modeli") %>%
  kable_paper("hover", full_width = F)


#Zapisanie wartości zmiennej "status" do proby testowej na podstawie modelu random forest
proba_testowa_final <- proba_testowa
predykcje.rf.test <- predict(status.fr, newdata = proba_testowa_final, block.size = 1)
proba_testowa_final$status <- predykcje.rf.test
table(proba_testowa_final$status)


### ZADANIE 2. Budowanie modelu w oparciu o zmienną ilościową "amount"

load("dane_zaliczenie.RData")

### Przygotowanie danych do zadania 2.

proba_uczaca$description <- ifelse(proba_uczaca$description == "", "Brak opisu", proba_uczaca$description)
proba_uczaca$description <- ifelse(is.na(proba_uczaca$description), "Brak opisu", proba_uczaca$description)

nowa_uczaca <- proba_uczaca %>%
  select(id, createtime, amount, description, recurringaction, acquirerconnectionmethod, expirymonth, expiryyear, issuer, 
         type, level, countrycode, listtype, mccname) %>%
  filter(recurringaction == "AUTO") %>%
  mutate(acquirerconnectionmethod = factor(acquirerconnectionmethod),
         expirymonth = factor(expirymonth),
         description = factor(description),
         expiryyear = factor(expiryyear),
         issuer = factor(issuer),
         type = factor(type),
         level = factor(level),
         countrycode = factor(countrycode),
         listtype = factor(listtype),
         mccname = factor(mccname))

nowa_uczaca$recurringaction <- NULL

#utworzenie nowych zmiennych i modyfikacja istniejących
nowa_uczaca$weekDay <- weekdays(as.Date(nowa_uczaca$createtime))
nowa_uczaca$weekDay <- as.factor(nowa_uczaca$weekDay)

nowa_uczaca$weekDay <- factor(nowa_uczaca$weekDay, levels= c("poniedziałek", "wtorek", 
                                                             "środa", "czwartek", "piątek", "sobota", "niedziela"))

str(nowa_uczaca)


### Zbudowanie modelu drzewa regresyjnego dla zmiennej 'amount'
drzewo.reg <- rpart(amount ~. -id - createtime, data = nowa_uczaca)
rsq.rpart(drzewo.reg)

prp(drzewo.reg)

y <- nowa_uczaca$amount
sum((y-mean(y))^2)
ss <- function (x) sum((x-mean(x))^2)
ss(y)

ss(y[nowa_uczaca$listtype %in% c("MWF","ECOMMERCE")])
ss(y[!(nowa_uczaca$listtype %in% c("MWF","ECOMMERCE"))])

drzewo.reg$variable.importance

drzewo.reg.big <- rpart(amount ~.-id-createtime, data = nowa_uczaca, control = rpart.control(cp=0))
plot(drzewo.reg.big)

tail(drzewo.reg.big$cptable,10) #ostatnie 10 wierszy z tabeli, wniosek: caly czas duza zmiennosc bo rel error juz praktycznie wcale nie maleje i jest duzy
przycinanie <- function(drzewo.reg) {
  bledy <- drzewo.reg$cptable
  tmp1 <- which.min(bledy[, "xerror"])  # min błąd w sprawdzaniu krzyżowym
  tmp2 <- sum(bledy[tmp1, c("xerror", "xstd")]) # min błąd + odchylenie standardowe
  optymalny <- which(bledy[, "xerror"] < tmp2)[1] # nr optymalnego drzewa
  
  prune(drzewo.reg, cp = bledy[optymalny, "CP"]) # przycięcie drzewa
}

drzewo_przyciete.reg <- przycinanie(drzewo.reg.big)

drzewo_przyciete.reg$cptable

rsq.rpart(drzewo_przyciete.reg)

plot(drzewo_przyciete.reg)

sum((y - predict(drzewo_przyciete.reg))^2)
ss(y)
#niewielka roznica - model nieduzo wyjasnia
sum((y - predict(drzewo_przyciete.reg))^2)/ss(y) #wspolczynnik indeterminacji
1 - sum((y - predict(drzewo_przyciete.reg))^2)/ss(y) #R-kwadrat (ale nie takie samo jak przy modelu liniowym)

summary(nowa_uczaca$amount)
hist(nowa_uczaca$amount)
count(nowa_uczaca[nowa_uczaca$amount > 500,])
#338 zakwalifikowane jako outliers

#Drzewo regresyjne bez outliers (powyżej 500)
nowa_regresja <- nowa_uczaca[nowa_uczaca$amount < 500,]

summary(nowa_regresja$amount)
hist(nowa_regresja$amount)

drzewo.reg.pred <- predict(drzewo.reg, nowa_uczaca)
RMSE.reg.drzewo.first <- RMSE(pred = drzewo.reg.pred, obs = nowa_uczaca$amount)
RMSE.reg.drzewo.first


### Zbudowanie modelu drzewa regresyjnego dla zmiennej 'amount' poniżej kwoty 500 zł
new.drzewo.reg <- rpart(amount ~. -id - createtime, data = nowa_regresja)
rsq.rpart(new.drzewo.reg)

prp(new.drzewo.reg)
rpart.plot(new.drzewo.reg, branch.type = 5)

y <- nowa_regresja$amount
sum((y-mean(y))^2)
ss <- function (x) sum((x-mean(x))^2)
ss(y)

ss(y[nowa_regresja$listtype %in% c("ECOMMERCE")])
ss(y[!(nowa_regresja$listtype %in% c("ECOMMERCE"))])

drzewo.reg$variable.importance

new.drzewo.reg.big <- rpart(amount ~.-id-createtime, data = nowa_regresja, control = rpart.control(cp=0))
plot(new.drzewo.reg.big)

tail(new.drzewo.reg.big$cptable,10) #ostatnie 10 wierszy z tabeli, wniosek: caly czas duza zmiennosc bo rel error juz praktycznie wcale nie maleje i jest duzy
przycinanie <- function(new.drzewo.reg) {
  bledy <- new.drzewo.reg$cptable
  tmp1 <- which.min(bledy[, "xerror"])  # min błąd w sprawdzaniu krzyżowym
  tmp2 <- sum(bledy[tmp1, c("xerror", "xstd")]) # min błąd + odchylenie standardowe
  optymalny <- which(bledy[, "xerror"] < tmp2)[1] # nr optymalnego drzewa
  
  prune(new.drzewo.reg, cp = bledy[optymalny, "CP"]) # przycięcie drzewa
}

new.drzewo_przyciete.reg <- przycinanie(new.drzewo.reg.big)

new.drzewo_przyciete.reg$cptable

plot(new.drzewo_przyciete.reg)
rsq.rpart(new.drzewo_przyciete.reg)

sum((y - predict(new.drzewo_przyciete.reg))^2)
ss(y)
#niewielka roznica - model nieduzo wyjasnia
sum((y - predict(new.drzewo_przyciete.reg))^2)/ss(y) #wspolczynnik indeterminacji
1 - sum((y - predict(new.drzewo_przyciete.reg))^2)/ss(y)

drzewo.reg.pred.fin <- predict(new.drzewo_przyciete.reg, nowa_regresja)
RMSE.reg.drzewo.final <- RMSE(pred = drzewo.reg.pred.fin, obs = nowa_regresja$amount)
RMSE.reg.drzewo.final

#Regresja liniowa

model <- lm(amount ~.-id-createtime, data = nowa_uczaca)
summary(model)

model.lm.pred <- predict(model, nowa_uczaca)
RMSE.model <- RMSE(pred = model.lm.pred, obs = nowa_uczaca$amount)
RMSE.model

#MARS

mars1 <- earth(formula = amount ~ .-id-createtime,
               data = nowa_uczaca,
               trace = 3)

pred.mars1 <- predict(mars1, nowa_uczaca)
RMSE.mars <- RMSE(pred = pred.mars1, obs = nowa_uczaca$amount)
RMSE.mars

m <- mars1$bx

wezly <- mars1$dirs
wezly2 <- mars1$cuts

f2 <- mars1$prune.terms

summary(mars1)

mars1$gcv.per.subset

plot(mars1$gcv.per.subset)
plot(mars1$rss.per.subset)

plot(mars1, info = T)

#PORÓWNANIE RMSE DLA WYBRANYCH MODELI

RMSE.reg.drzewo.first
RMSE.reg.drzewo.final
RMSE.model
RMSE.mars

vec.names = c("Drzewo regresyjne", "Drzewo regresyjne bez obserwacji odbiegających", "Model regresji liniowej", "MARS")
vec.rmse = c(RMSE.reg.drzewo.first, RMSE.reg.drzewo.final, RMSE.model, RMSE.mars)
rmse.podsumowanie <- data.frame("Model" = vec.names,
                                "RMSE" = vec.rmse)

rmse.podsumowanie

rmse.podsumowanie %>%
  kbl(caption = "Wartosć miar RMSE dla poszczególnych modeli") %>%
  kable_paper("hover", full_width = F)



#Zapisanie wartości zmiennej "amount" do próby testowej na podstawie modelu drzewa regresyjnego bez wartości odbiegających
drzewo.reg.pred.fin <- predict(new.drzewo_przyciete.reg, proba_testowa_final)
proba_testowa_final$amount <- drzewo.reg.pred.fin

#RMSE dla drzewa regresyjnego
RMSE.reg.drzewo.final


#Sprawdzenie czy rekordy zgadzają się między zbiorami i usunięcie różnic związanych z poziomami zmiennych typu factor
which(!(predykcje_testowa$id %in% proba_testowa_final$id))
predykcje_testowa <- predykcje_testowa[-c(13079, 13080, 13081, 1991, 1992, 1993, 1994, 1995, 1996, 4167,
                                          4174,  4175,  4176,  4177,  4178,  7076,  7077,  7994,  7995,  7996,  7997,  
                                          7998,  7999,  8000,  8001,  8002, 8003,  8004, 8005,  8006,  8482,  8483,  
                                          8484, 10077, 10078, 10079, 12583, 12584, 12585, 12586, 13337),]

#OSTATECZNE WYNIKI ZAPISANE DO PREDYKCJI TESTOWEJ
predykcje_testowa$amount <- proba_testowa_final$amount
predykcje_testowa$status <- proba_testowa_final$status
summary(predykcje_testowa)[,2:3]
save(predykcje_testowa, file = "predykcje_testowe.RData")

