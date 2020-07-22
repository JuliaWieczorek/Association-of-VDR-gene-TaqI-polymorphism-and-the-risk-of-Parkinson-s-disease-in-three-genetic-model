#komenda fmsb::oddsratio(1, 446, 1, 437, conf.level=0.95, p.calc.by.independence=TRUE)

#Przygotowanie danych
Author = c("Lv Z",
           "Liu",
           "Torok",
           "Petersen",
           "Meamar",
           "Gezen-Ak")
col = c("Case_MM","Case_MW","Case_WW","Control_MM","Control_MW","Control_WW")

#Dane dla wszystkich artyku³ów, w celu oblcizenia OR wartoœci = 0 zast¹piono 1.
data=matrix(c(1,46,437,1,52,446,
              1,33,252,1,30,225,
              17,48,35,16,46,47,
              20,54,47,34,119,81,
              6,25,28,4,26,23,
              154,182,45,109,98,33),
            nrow=6, ncol=4, byrow = TRUE)


#Dodanie nazw dla wierszy i kolumn
colnames(data) <- col

data = data.frame(data)
data = cbind(Author,data)
str(data)

#Kompletne dane
data

# Forest Plot
library(meta)
Case_total = data$Case_MM + data$Case_MW + data$Case_WW
Control_total = data$Control_MM + data$Control_MW + data$Control_WW

Case_MMWW = data$Case_MM + data$Case_WW
Control_MMWW = data$Control_MM + data$Control_WW

#Metoda I - MM vs WW
m.bin <- metabin(Case_MM,
                 Case_MMWW,
                 Control_MM,
                 Control_MMWW,
                 data = data,
                 studlab = Author,
                 comb.fixed = FALSE,
                 comb.random = TRUE,
                 method.tau = "SJ",
                 hakn = TRUE,
                 prediction = TRUE,
                 incr = 0.1,
                 sm = "OR")
m.bin
data
forest(m.bin)

#Metoda II - MM + MW vs WW
a=data$Case_MM+data$Case_MW
b=data$Control_MM+data$Control_MW


m.bin <- metabin(a,
                 Case_total,
                 b,
                 Control_total,
                 data = data,
                 studlab = author,
                 comb.fixed = FALSE,
                 comb.random = TRUE,
                 method.tau = "SJ",
                 hakn = TRUE,
                 prediction = TRUE,
                 incr = 0.1,
                 sm = "OR")
m.bin
data
forest(m.bin)

#Metoda III - MM vs WW + MW
a=data$Case_WW+data$Case_MW
b=data$Control_WW+data$Control_MW


m.bin <- metabin(a,
                 Case_total,
                 b,
                 Control_total,
                 data = data,
                 studlab = author,
                 comb.fixed = FALSE,
                 comb.random = TRUE,
                 method.tau = "SJ",
                 hakn = TRUE,
                 prediction = TRUE,
                 incr = 0.1,
                 sm = "OR")
m.bin
data
forest(m.bin)
