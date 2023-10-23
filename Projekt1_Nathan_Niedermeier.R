######################################################
####### Fallstudie 1 Projekt 1 #######################
######################################################

#########################################################################
################ Projekt 1 Fallstudien 2023 #############################
#########################################################################

##### Umbenennung ######

emission_data <- daten_final3


####### Fehlende Werte: #######

# Aufgabenstellung: In einigen Bereichen ist der Datensatz nicht komplett. Wie kann das zustande kommen
# und wie könnte man bei der Deskription darauf eingehen?

# Wir begrenzen den Datensatz auf die Jahre ab 1990, für Details siehe Bericht.

emission_data <- subset(emission_data,year>1989)

# Die fehlenden Werte in den verbleibenden Daten können wir nicht schätzen,
# deshalb sollen sie bei der weiteren Auswertung ignoriert werden,
# für Details siehe Bericht.

emission_data_cleaned <- na.omit(emission_data)

####### Fuel combustion (sectoral approach): #########

# Aufgabenstellung: Welche Länder haben den höchsten/niedrigsten Ausstoß an 
# Treibhausgasen im Jahr 2021? 

# Datensatz nur für das Jahr 2021 erstellen
data2021 <- subset(emission_data_cleaned, year==2021)

# Datensatz auf die Gesamtemissionen begrenzen
data2021_gesamtemissionen <- subset(data2021, Sector=="Fuel combustion - sectoral approach")

# Die Zeilen, in denen Country == "European Union - 27 countries (from 2020)" ist, ausschließen.
# Diese Zeile Enthält die Daten für die europäische Union und ist sonst immer der Maximalwert, 
# das ist störend, wenn wir das Maximum unter den einzelnen Ländern bestimmen wollen.
data2021_gesamtemissionen <- subset(data2021_gesamtemissionen, Country!="European Union - 27 countries (from 2020)")

# Zunächst fügen wir dem Datensatz eine neue Variable hinzu,
# die die Summe der drei angegebenen Treibhausgase enthält. 
# Dabei nutzen wir jeweils die CO2 bzw. CO2 eq Werte, so dass diese Spalte 
# die emittierten Treibhausgase in CO2-Äquivalenten angibt.
data2021_gesamtemissionen$thg <- data2021_gesamtemissionen$Nitrous.oxide_CO2 + data2021_gesamtemissionen$Methane_CO2 + data2021_gesamtemissionen$Carbon.dioxide 


# Wer hat den größten und niedrigsten CO2-Ausstoß?

# Ergebnisse sortieren nach Höhe der Emissionen
data2021_co2_sort <- data2021_gesamtemissionen[order(data2021_gesamtemissionen$Carbon.dioxide),]

#Minimum und Maximum bestimmen
min_co2 <- min(data2021_gesamtemissionen$Carbon.dioxide)
Minimum_Land <- data2021_gesamtemissionen[data2021_gesamtemissionen$Carbon.dioxide == min_co2, "Country"]
max_co2 <- max(data2021_gesamtemissionen$Carbon.dioxide)
Maximum_Land <- data2021_gesamtemissionen[data2021_gesamtemissionen$Carbon.dioxide == max_co2, "Country"]

# Arithemtisches Mittel und Median bestimmen
mean(data2021_gesamtemissionen$Carbon.dioxide) #87.28218
median(data2021_gesamtemissionen$Carbon.dioxide) # 33.98714

#Barplot noch schön machen (für Anhang)
barplot(data2021_co2_sort$Carbon.dioxide)

# Wer hat den größten/niedrigsten Methan-Ausstoß?

# Ergebnisse sortieren nach Höhe der Emissionen
data2021_methan_sort <- data2021_gesamtemissionen[order(data2021_gesamtemissionen$Methane_CO2),]

#Minimum und Maximum bestimmen
min_methan <- min(data2021_gesamtemissionen$Methane_CO2)
Minimum_Land <- data2021_gesamtemissionen[data2021_gesamtemissionen$Methane_CO2 == min_methan, "Country"]
max_methan <- max(data2021_gesamtemissionen$Methane_CO2)
Maximum_Land <- data2021_gesamtemissionen[data2021_gesamtemissionen$Methane_CO2 == max_methan, "Country"]

# Arithemtisches Mittel und Median bestimmen
mean(data2021_gesamtemissionen$Methane_CO2) # 0.9046323
median(data2021_gesamtemissionen$Methane_CO2) # 0.33671

#Barplot noch schön machen (für Anhang)
barplot(data2021_methan_sort$Methane_CO2)

# Wer hat den größten/niedrigsten Lachgas(Distickstoffmonoxid)-Ausstoß?

# Ergebnisse sortieren nach Höhe der Emissionen
data2021_lachgas_sort <- data2021_gesamtemissionen[order(data2021_gesamtemissionen$Nitrous.oxide_CO2),]

#Minimum und Maximum bestimmen
min_lachgas <- min(data2021_gesamtemissionen$Nitrous.oxide_CO2)
Minimum_Land <- data2021_gesamtemissionen[data2021_gesamtemissionen$Nitrous.oxide_CO2 == min_lachgas, "Country"]
max_lachgas <- max(data2021_gesamtemissionen$Nitrous.oxide_CO2)
Maximum_Land <- data2021_gesamtemissionen[data2021_gesamtemissionen$Nitrous.oxide_CO2 == max_lachgas, "Country"]

# Arithemtisches Mittel und Median bestimmen
mean(data2021_gesamtemissionen$Nitrous.oxide_CO2) # 0.769943
median(data2021_gesamtemissionen$Nitrous.oxide_CO2) # 0.30927

#Barplot noch schön machen (für Anhang)
barplot(data2021_lachgas_sort$Nitrous.oxide_CO2)

##### Wer hat insgesamt den größten/niedrigsten Ausstoß an Treibhausgasen?  #################

# Zunächst fügen wir dem Datensatz eine neue Variable hinzu,
# die die Summe der drei angegebenen Treibhausgase enthält. 
# Dabei nutzen wir jeweils die CO2 bzw. CO2 eq Werte, so dass diese Spalte 
# die emittierten Treibhausgase in CO2-Äquivalenten angibt.
data2021_gesamtemissionen$thg <- data2021_gesamtemissionen$Nitrous.oxide_CO2 + data2021_gesamtemissionen$Methane_CO2 + data2021_gesamtemissionen$Carbon.dioxide 

# Welche Länder emittieren die meisten und wenigsten Treibhausgase?
data2021_gesamtemissionen_sort <- data2021_gesamtemissionen[order(data2021_gesamtemissionen$thg),]

# Minimum und Maximum bestimmen
min_thg <- min(data2021_gesamtemissionen$thg)
Minimum_Land <- data2021_gesamtemissionen[data2021_gesamtemissionen$thg == min_thg, "Country"]
max_thg <- max(data2021_gesamtemissionen$thg)
Maximum_Land <- data2021_gesamtemissionen[data2021_gesamtemissionen$thg == max_thg, "Country"]

# Arithemtisches Mittel und Median bestimmen
mean(data2021_gesamtemissionen$thg) # 88.95676
median(data2021_gesamtemissionen$thg) # 34.55243

# Quantile bestimmen
quantile(data2021_gesamtemissionen$thg)
# 0%       25%       50%       75%      100% 
# 1.58663  13.25164  34.55243  77.95202 638.52535 

barplot(data2021_gesamtemissionen_sort$thg)
# Hier dann noch Barplot schön machen (für Kapitel 4)


### Aufgabenstellung: Welche Sektoren tragen in den einzelnen Ländern insbesondere dazu bei? ####

# Wir betrachten nur die drei Länder mit dem höchsten Ausstoß:
# Deutschland Italien, Polen
# Und die drei Länder mit dem niedrigsten Ausstoß: Malta, Zypern, Island
# Wir berücksichtigen dabei die Sektoren A, B, C, D und E

# Datensatz auf die Sektoren begrenzen in dem Gesamtdaten und Untersektoren exkludiert werden:
data2021_Sektoren <- subset(data2021,subset = Code_Sector %in% c("A","B","C","D","E"))

# Auswahl der Daten für die sechs Länder
Germany_data2021_Sektoren <- subset(data2021_Sektoren, Country=="Germany")
Italy_data2021_Sektoren <- subset(data2021_Sektoren, Country=="Italy")
Poland_data2021_Sektoren <- subset(data2021_Sektoren, Country=="Poland")
Malta_data2021_Sektoren <- subset(data2021_Sektoren, Country=="Malta")
Iceland_data2021_Sektoren <- subset(data2021_Sektoren, Country=="Iceland")
Cyprus_data2021_Sektoren <- subset(data2021_Sektoren, Country=="Cyprus")

# Barplots für die sechs Länder für CO2-Emissionen (noch schön machen!)
barplot(Germany_data2021_Sektoren$Carbon.dioxide)
barplot(Italy_data2021_Sektoren$Carbon.dioxide)
barplot(Poland_data2021_Sektoren$Carbon.dioxide)
barplot(Malta_data2021_Sektoren$Carbon.dioxide)
barplot(Iceland_data2021_Sektoren$Carbon.dioxide)
barplot(Cyprus_data2021_Sektoren$Carbon.dioxide)

####### Welche Kennzahlen der Läder beeinflussen die Emissionen? ##############################

# Aufgabenstellung: Welche Einflussfaktoren aus der Datei Kennzahlen_Laender.csv 
# könnten für die zuvor beschriebenen Emissionen verantworlich sein? 
# Beachten sie hierbei auch die unterschiedlichen Skalenniveaus der Daten. 
# Sie können auch weitere Daten recherchieren und in die Analyse mit aufnehmen.




##### Gini Index ###############################################################################

# Aufgabenstellung: Bestimmen und interpretieren Sie für das Jahr 2021
# den Gini-Index des CO2-Ausstoßes.

install.packages("ineq")
library("ineq")

ineq(data2021$Carbon.dioxide,type="Gini")
# sollte hier aber auch gruppiert sein nach den Ländern oder?   

# Der Gini-Koeffizient liegt bei 0.8934842

# Wie interpretieren?


########################## CO2-Ausstoß unter der Lupe für bestimmte Länder #####################

# Aufgabenstellung: Vergleichen Sie den Ausstoß an CO2 in den Ländern Deutschland, 
# Frankreich, Österreich und Niederlande in den verschiedenen Sektoren. 
# Dieser soll entsprechend optisch aufbereitet werden
# und auch den zeitlichen Verlauf der Werte darstellen.









########## Weitere Fragestellungen ########################################################

# Aufgabestellung: 
# Untersuchen Sie zusätzlich zwei bis drei weitere interessante Aspekte Ihrer Wahl.

#### Welches Land hat die Treibhausgasemissionen am stärksten reduziert zwischen 1990 und 2021? #####

# Daten für das Jahr 1990 extrahieren
data1990 <- subset(emission_data_cleaned, year==1990)

# Nur die Gesamtemissionen betrachten
data1990_gesamtemissionen <- subset(data1990, Sector=="Fuel combustion - sectoral approach")

# Die Zeilen, in denen Country == "European Union - 27 countries (from 2020)" ist, ausschließen.
# Diese Zeile Enthält die Daten für die europäische Union und ist sonst immer der Maximalwert, 
# das ist störend, wenn wir das Maximum unter den einzelnen Ländern bestimmen wollen.

data1990_gesamtemissionen <- subset(data1990_gesamtemissionen, Country!="European Union - 27 countries (from 2020)")

# Treibhausgase zusammenfassen zu einer Spalte
data1990_gesamtemissionen$thg <- data1990$Nitrous.oxide_CO2 + data1990$Methane_CO2 + data1990$Carbon.dioxide 

# Daten nach Ländern aggregieren und sortieren
thg_by_country_1990 <- aggregate(data1990_gesamtemissionen$thg, by=list(Category=data1990_gesamtemissionen$Country), FUN=sum)
thg_by_country_1990 <- thg_by_country_1990[order(thg_by_country_1990$x),]

# Daten aus 2021 und 1990 mergen in einen dataframe
thg_1990_2021 <- merge(thg_by_country_1990,thg_by_country_2021, by="Category")

# Spalten korrekt bennnen
colnames(thg_1990_2021) <- c("Country","Jahr1990","Jahr2021")

# Neue Variable hinzufügen die die Differenz der Emissionen zwischen den 
# zwei Jahren angibt.
thg_1990_2021$Diff <- thg_1990_2021$Jahr1990 - thg_1990_2021$Jahr2021

# Jetzt noch die prozentuale Reduktion der Emissionen berechnen:
thg_1990_2021$Diff_rel <- thg_1990_2021$Diff*100/thg_1990_2021$Jahr1990

# Maximum und Minimum für absolute Reduktion
min_reduktion_absolut <- min(thg_1990_2021$Diff)
Minimum_absolut <- thg_1990_2021[thg_1990_2021$Diff == min_reduktion_absolut, "Country"]
max_reduktion_absolut <- max(thg_1990_2021$Diff)
Maximum_absolut <- thg_1990_2021[thg_1990_2021$Diff == max_reduktion_absolut, "Country"]

# Maximum und Minimum für relative Reduktion
min_reduktion_rel <- min(thg_1990_2021$Diff_rel)
Minimum_absolut <- thg_1990_2021[thg_2015_2021$Diff_rel == min_reduktion_rel, "Country"]
max_reduktion_rel <- max(thg_1990_2021$Diff_rel)
Maximum_absolut <- thg_1990_2021[thg_2015_2021$Diff_rel == max_reduktion_rel, "Country"]

#### Liniendiagramm zur Verdeutlichung der Enzwicklung ###############

# Daten für die Jahre 1990 bis 2021 extrahieren
data1990_2021 <- subset(emission_data_cleaned, subset = year %in% c(1990:2021))

# Treibhausgase zusammenfassen
data1990_2021$thg <- data1990_2021$Nitrous.oxide_CO2 + data1990_2021$Methane_CO2 + data1990_2021$Carbon.dioxide

# Nur die Gesamtemissionen betrachten
data1990_2021_gesamtemissionen <- subset(data1990_2021, Sector=="Fuel combustion - sectoral approach")

# Die Zeilen, in denen Country == "European Union - 27 countries (from 2020)" ist, ausschließen.
data1990_2021_gesamtemissionen <- subset(data1990_2021_gesamtemissionen, Country!="European Union - 27 countries (from 2020)")

# Liniendiagramm plotten für alle 30 Länder (Anhang)

line_plot <- ggplot(data1990_2021_gesamtemissionen, mapping = aes(x = year, y = Carbon.dioxide)) +
  geom_line(aes(colour=Country),size=0.4,linetype=1) + 
  ggtitle("Entwicklung der Treibhausgasemissionen zwischen 1990 und 2021")

line_plot

# Damit das Liniendiagramm besser lesbar wird, schauen wir uns in einem nächsten Schritt
# nur die Emissionsentwicklung der Länder an, die relativ betrachtet am stärksten reduziert haben.
# Dafür ziehen wir das obere Quartil zu Hilfe

# Bestimmung der Quantile
quantile(thg_1990_2021$Diff_rel)
# 0%       25%       50%       75%      100% 
# -56.10955  12.12789  22.37339  40.85774  71.21523 

# Nur die Länder des oberen Quartils
country_top <- subset(thg_1990_2021$Country, thg_1990_2021$Diff_rel >= 40.85774)

# Begrenzung des Datensatzes auf diese Länder für das neue Liniendiagramm

data1990_2021_gesamtemissionen_top <- subset(data1990_2021_gesamtemissionen, subset = Country %in% country_top)

# Liniendiagramm erstellen (noch verbessern?)
line_plot_top <- ggplot(data1990_2021_gesamtemissionen_top, mapping = aes(x = year, y = Carbon.dioxide)) +
  geom_line(aes(colour=Country),size=0.4,linetype=1) + 
  ggtitle("Treibhausgasemissionen zwischen 1990 und 2021 bei Ländern mit höchster Reduktion (relativ)")

line_plot_top

# In einem weiteren Schritt interessieren uns die Ländern, bei denen die Emissionen am wenigsten gesunken sind
# oder sogar gestiegen: 

# Nur die Länder des unteren Quartils
country_flop <- subset(thg_1990_2021$Country, thg_1990_2021$Diff_rel <= 12.12789)

# Begrenzung des Datensatzes auf diese Länder für das neue Liniendiagramm

data1990_2021_gesamtemissionen_flop <- subset(data1990_2021_gesamtemissionen, subset = Country %in% country_flop)

# Liniendiagramm erstellen (noch verbessern?)
line_plot_flop <- ggplot(data1990_2021_gesamtemissionen_flop, mapping = aes(x = year, y = Carbon.dioxide)) +
  geom_line(aes(colour=Country),size=0.4,linetype=1) + 
  ggtitle("Treibhausgasemissionen zwischen 1990 und 2021 bei Ländern mit geringster Reduktion (relativ)")

line_plot_flop

# Und in einem weiteren Schritt die Länder, die im Jahr 1990 die höchsten Emissionen verzeichneten
# Bei denen es also am relevantesten ist, das sie ihre Emissionen reduzieren.

# Bestimmung der Quantile
quantile(data1990_gesamtemissionen$thg)
# 0%        25%        50%        75%       100% 
# 1.77842   26.78444   51.90080  137.65928 1001.63262 

country_high <- subset(data1990_gesamtemissionen$Country, data2015_gesamtemissionen$thg >= 137.65928)

data1990_2021_gesamtemissionen_high <- subset(data1990_2021_gesamtemissionen, subset = Country %in% country_high)

# Liniendiagramm erstellen (noch verbessern?)
line_plot_high <- ggplot(data1990_2021_gesamtemissionen_high, mapping = aes(x = year, y = Carbon.dioxide)) +
  geom_line(aes(colour=Country),size=0.4,linetype=1) + 
  ggtitle("Treibhausgasemissionen zwischen 1990 und 2021 bei Ländern mit höchsten Emissionen in 1990")

line_plot_high

#### Welches Land hat nach dem Pariser Klimaabkommen seine Emissionen am meisten reduziert? #####

# Dazu nur die Werte von 2015 und 2021 pro Land miteinander vergleichen.

# Daten für das Jahr 2015 extrahieren
data2015 <- subset(emission_data_cleaned, year==2015)

# Treibhausgase zusammenfassen
data2015$thg <- data2015$Nitrous.oxide_CO2 + data2015$Methane_CO2 + data2015$Carbon.dioxide

# Nur die Gesamtemissionen betrachten
data2015_gesamtemissionen <- subset(data2015, Sector=="Fuel combustion - sectoral approach")

# Die Zeilen, in denen Country == "European Union - 27 countries (from 2020)" ist, ausschließen.
# Diese Zeile Enthält die Daten für die europäische Union und ist sonst immer der Maximalwert, 
# das ist störend, wenn wir das Maximum unter den einzelnen Ländern bestimmen wollen.

data2015_gesamtemissionen <- subset(data2015_gesamtemissionen, Country!="European Union - 27 countries (from 2020)")

# Daten nach Ländern aggregieren
thg_by_country_2015 <- aggregate(data2015_gesamtemissionen$thg, by=list(Category=data2015_gesamtemissionen$Country), FUN=sum)

# Daten aus 2015 und 2021 in einen dataframe mergen
thg_2015_2021 <- merge(thg_by_country_2015,thg_by_country_2021, by="Category")

# Spalten korrekt benennen
colnames(thg_2015_2021) <- c("Country","Jahr2015","Jahr2021")

# Differenz zwischen 2015 und 2021 absolut und relativ berechnen
thg_2015_2021$Diff <- thg_2015_2021$Jahr2015 - thg_2015_2021$Jahr2021
thg_2015_2021$Diff_rel <- thg_2015_2021$Diff*100/thg_2015_2021$Jahr2015

# Maximum und Minimum für absolute Reduktion
min_reduktion_absolut <- min(thg_2015_2021$Diff)
Minimum_absolut <- thg_2015_2021[thg_2015_2021$Diff == min_reduktion_absolut, "Country"]
max_reduktion_absolut <- max(thg_2015_2021$Diff)
Maximum_absolut <- thg_2015_2021[thg_2015_2021$Diff == max_reduktion_absolut, "Country"]

# Maximum und Minimum für relative Reduktion
min_reduktion_rel <- min(thg_2015_2021$Diff_rel)
Minimum_absolut <- thg_2015_2021[thg_2015_2021$Diff_rel == min_reduktion_rel, "Country"]
max_reduktion_rel <- max(thg_2015_2021$Diff_rel)
Maximum_absolut <- thg_2015_2021[thg_2015_2021$Diff_rel == max_reduktion_rel, "Country"]

#### Liniendiagramm zur Verdeutlichung der Enzwicklung ###############

# Daten für die Jahre 2015 bis 2021 extrahieren
data2015_2021 <- subset(emission_data_cleaned, subset = year %in% c(2015:2021))

# Treibhausgase zusammenfassen
data2015_2021$thg <- data2015_2021$Nitrous.oxide_CO2 + data2015_2021$Methane_CO2 + data2015_2021$Carbon.dioxide

# Nur die Gesamtemissionen betrachten
data2015_2021_gesamtemissionen <- subset(data2015_2021, Sector=="Fuel combustion - sectoral approach")

# Die Zeilen, in denen Country == "European Union - 27 countries (from 2020)" ist, ausschließen.
data2015_2021_gesamtemissionen <- subset(data2015_2021_gesamtemissionen, Country!="European Union - 27 countries (from 2020)")

# Liniendiagramm plotten für alle 30 Länder (Anhang)

line_plot <- ggplot(data2015_2021_gesamtemissionen, mapping = aes(x = year, y = Carbon.dioxide)) +
  geom_line(aes(colour=Country),size=0.4,linetype=1) + 
  ggtitle("Entwicklung der Treibhausgasemissionen zwischen 2015 und 2021")

line_plot

# Damit das Liniendiagramm besser lesbar wird, schauen wir uns in einem nächsten Schritt
# nur die Emissionsentwicklung der Länder an, die relativ betrachtet am stärksten reduziert haben.
# Dafür ziehen wir das obere Quartil zu Hilfe

# Bestimmung der Quantile
quantile(thg_2015_2021$Diff_rel)
# 0%       25%       50%       75%      100% 
# -9.748591  2.155756  7.426054 13.942358 33.420015

# Nur die Länder des oberen Quartils
country_top <- subset(thg_2015_2021$Country, thg_2015_2021$Diff_rel >= 13.942358)

# Begrenzung des Datensatzes auf diese Länder für das neue Liniendiagramm

data2015_2021_gesamtemissionen_top <- subset(data2015_2021_gesamtemissionen, subset = Country %in% country_top)

# Liniendiagramm erstellen (noch verbessern?)
line_plot_top <- ggplot(data2015_2021_gesamtemissionen_top, mapping = aes(x = year, y = Carbon.dioxide)) +
  geom_line(aes(colour=Country),size=0.4,linetype=1) + 
  ggtitle("Treibhausgasemissionen zwischen 2015 und 2021 bei Ländern mit höchster Reduktion (relativ)")

line_plot_top

# In einem weiteren Schritt interessieren uns die Ländern, bei denen die Emissionen am wenigsten gesunken sind
# oder sogar gestiegen: 

# Nur die Länder des unteren Quartils
country_flop <- subset(thg_2015_2021$Country, thg_2015_2021$Diff_rel <= 2.155756)

# Begrenzung des Datensatzes auf diese Länder für das neue Liniendiagramm

data2015_2021_gesamtemissionen_flop <- subset(data2015_2021_gesamtemissionen, subset = Country %in% country_flop)

# Liniendiagramm erstellen (noch verbessern?)
line_plot_flop <- ggplot(data2015_2021_gesamtemissionen_flop, mapping = aes(x = year, y = Carbon.dioxide)) +
  geom_line(aes(colour=Country),size=0.4,linetype=1) + 
  ggtitle("Treibhausgasemissionen zwischen 2015 und 2021 bei Ländern mit geringster Reduktion (relativ)")

line_plot_flop

# Und in einem weiteren Schritt die Länder, die im Jahr 2015 die höchsten Emissionen verzeichneten
# Bei denen es also am relevantesten ist, das sie ihre Emissionen reduzieren.

# Bestimmung der Quantile
quantile(data2015_gesamtemissionen$thg)
# 0%       25%       50%       75%      100% 
# 1.66425  15.75314  38.85966  82.29886 758.23791 

country_high <- subset(data2015_gesamtemissionen$Country, data2015_gesamtemissionen$thg >= 82.29886)

data2015_2021_gesamtemissionen_high <- subset(data2015_2021_gesamtemissionen, subset = Country %in% country_high)

# Liniendiagramm erstellen (noch verbessern?)
line_plot_high <- ggplot(data2015_2021_gesamtemissionen_high, mapping = aes(x = year, y = Carbon.dioxide)) +
  geom_line(aes(colour=Country),size=0.4,linetype=1) + 
  ggtitle("Treibhausgasemissionen zwischen 2015 und 2021 bei Ländern mit höchsten Emissionen in 2015")

line_plot_high


###### Wie steht es um die Pro-Kopf Emissionen in den Ländern im Jahr 2021? ##########################

# Daten von Eurostat einladen. Die ursprünglichen Daten wurden so bereinigt,
# dass sie nur noch die hier relevanten 30 Länder enthalten und die Daten
# für die Einwohneranzahl im Jahr 2021
einwohner_2021 <- read.table("C:/Users/natha/Documents/Einwohner_2021.csv", encoding = "latin1", header = TRUE, sep = ";")

# Spaltenname vergeben, so dass mergen funktioniert
colnames(einwohner_2021) <- c("Country","Einwohner")

# Beide Datensätze mergen nach Ländername
data_komplett <- merge(data2021_gesamtemissionen,einwohner_2021, by="Country")

# Punkte aus Daten rausschmeißen, um richtigen Zahlenwert zu bekommen und Datenformat anpassen
data_komplett$Einwohner <- as.numeric(gsub("\\.","",data_komplett$Einwohner))

# Emissionen mal 1000000 um Pro-Kopf Emissionen in Tonnen ausrechnen zu können
data_komplett$thg <- data_komplett$thg*1000000

# Pro Kopf Emissionen berechnen:
data_komplett$proKopf <- data_komplett$thg/data_komplett$Einwohner

# Sortieren nach der Höhe der Emissionen
data_komplett_sort <- data_komplett[order(data_komplett$proKopf),]

# Minimum und Maximum bestimmen
min_proKopf <- min(data_komplett$proKopf)
Minimum_Land <- data_komplett[data_komplett$proKopf == min_proKopf, "Country"]
max_proKopf <- max(data_komplett$proKopf)
Maximum_Land <- data_komplett[data_komplett$proKopf == max_proKopf, "Country"]

# Arithemtisches Mittel und Median bestimmen
mean(data_komplett$proKopf) # 5.647071
median(data_komplett$proKopf) # 5.249043

# Quantile bestimmen
quantile(data_komplett$proKopf)
# 0%       25%       50%       75%      100% 
# 3.117884  4.216371  5.249043  6.945867 12.612103 

barplot(data_komplett_sort$proKopf)
# Hier dann noch Barplot schön machen (für Kapitel 4)

