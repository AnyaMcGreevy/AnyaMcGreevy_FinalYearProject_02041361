rm(list=ls())
install.packages("soundecology")
library(soundecology)
install.packages("tuneR")
library("tuneR")
install.packages("ggplot2")
library(ggplot2)

#Site B

B <- "C:/Users/anyam/OneDrive/Desktop/Biology/FYP/Epping/Birds/Birds Site B"
wav_files_B <- list.files(path = B, pattern = "\\.(wav|WAV)$", full.names = TRUE)
ndsi_results_B <- data.frame(File = character(), 
                               NDSI = numeric(),
                               Biophony = numeric(),
                               Anthrophony = numeric(),
                               stringsAsFactors = FALSE)
for (file in wav_files_B) {
  sound <- readWave(file)
  ndsi_result_B <- ndsi(sound)
  
  ndsi_value_B <- ndsi_result_B$ndsi_left  # Mono files
  biophony <- ndsi_result_B$biophony_left
  anthrophony <- ndsi_result_B$anthrophony_left
  
  ndsi_results_B <- rbind(ndsi_results_B, data.frame(File = basename(file), 
                                                         NDSI = ndsi_value_B,                                                       Biophony = biophony, 
                                                         Anthrophony = anthrophony))
}
print(ndsi_results_B)

mean(ndsi_results_B$NDSI)
mean(ndsi_results_B$Biophony)
mean(ndsi_results_B$Anthrophony)
View(ndsi_results_B)  

#export to csv
write.csv(ndsi_results_B, "C:/Users/anyam/OneDrive/Desktop/Biology/FYP/Epping/Birds/Birds Site B.csv", row.names = FALSE)


#Site 6

Site6 <- "C:/Users/anyam/OneDrive/Desktop/Biology/FYP/Epping/Birds/Birds Site 6"
wav_files_Site6 <- list.files(path = Site6, pattern = "\\.(wav|WAV)$", full.names = TRUE)
ndsi_results_Site6 <- data.frame(File = character(), 
                             NDSI = numeric(),
                             Biophony = numeric(),
                             Anthrophony = numeric(),
                             stringsAsFactors = FALSE)
for (file in wav_files_Site6) {
  sound <- readWave(file)
  ndsi_result_Site6 <- ndsi(sound)
  
  ndsi_value_Site6 <- ndsi_result_Site6$ndsi_left  # Mono files
  biophony <- ndsi_result_Site6$biophony_left
  anthrophony <- ndsi_result_Site6$anthrophony_left
  
  ndsi_results_Site6 <- rbind(ndsi_results_Site6, data.frame(File = basename(file), 
                                                     NDSI = ndsi_value_Site6, 
                                                     Biophony = biophony, 
                                                     Anthrophony = anthrophony))
}
print(ndsi_results_6)

mean(ndsi_results_6$NDSI)
mean(ndsi_results_6$Biophony)
mean(ndsi_results_6$Anthrophony)
View(ndsi_results_6)  

#export to csv
write.csv(ndsi_results_Site6, "C:/Users/anyam/OneDrive/Desktop/Biology/FYP/Epping/Birds/Birds Site 6.csv", row.names = FALSE)

#Site 4

Site4 <- "C:/Users/anyam/OneDrive/Desktop/Biology/FYP/Epping/Birds/Birds Site 4"
wav_files_Site4 <- list.files(path = Site4, pattern = "\\.(wav|WAV)$", full.names = TRUE)
ndsi_results_Site4 <- data.frame(File = character(), 
                                 NDSI = numeric(),
                                 Biophony = numeric(),
                                 Anthrophony = numeric(),
                                 stringsAsFactors = FALSE)
for (file in wav_files_Site4) {
  sound <- readWave(file)
  ndsi_result_Site4 <- ndsi(sound)
  
  ndsi_value_Site4 <- ndsi_result_Site4$ndsi_left  # Mono files
  biophony <- ndsi_result_Site4$biophony_left
  anthrophony <- ndsi_result_Site4$anthrophony_left
  
  ndsi_results_Site4 <- rbind(ndsi_results_Site4, data.frame(File = basename(file), 
                                                             NDSI = ndsi_value_Site4, 
                                                             Biophony = biophony, 
                                                             Anthrophony = anthrophony))
}
print(ndsi_results_4)

mean(ndsi_results_4$NDSI)
mean(ndsi_results_4$Biophony)
mean(ndsi_results_4$Anthrophony)
View(ndsi_results_4)  # Opens in RStudio's spreadsheet-like viewer

#export to csv
write.csv(ndsi_results_Site4, "C:/Users/anyam/OneDrive/Desktop/Biology/FYP/Epping/Birds/Birds Site 4.csv", row.names = FALSE
          
          
          
////////////////////////////
  #calculating means
  data <- read.csv("C:/Users/anyam/OneDrive/Desktop/Biology/FYP/Epping/workings for NDSI.csv")
library(dplyr)
library(ggplot2)
library(tidyr)
# Means
mean1 <- mean(data$NDSI.1, na.rm = TRUE)
print(mean1)
mean2 <- mean(data$NDSI.2, na.rm = TRUE)
print(mean2)
mean3 <- mean(data$NDSI.3, na.rm = TRUE)
print(mean3)
mean4 <- mean(data$NDSI.4, na.rm = TRUE)
print(mean4)
mean5 <- mean(data$NDSI.5, na.rm = TRUE)
print(mean5)
mean6 <- mean(data$NDSI.6, na.rm = TRUE)
print(mean6)
mean7 <- mean(data$NDSI.7, na.rm = TRUE)
print(mean7)
mean8 <- mean(data$NDSI.8, na.rm = TRUE)
print(mean8)
meanA <- mean(data$NDSI.A, na.rm = TRUE)
print(meanA)
meanB <- mean(data$NDSI.B, na.rm = TRUE)
print(meanB)
meanC <- mean(data$NDSI.C, na.rm = TRUE)
print(meanC) 

# Standard Deviation
sd1 <- sd(data$NDSI.1, na.rm = TRUE)
print(sd1)
sd2 <- sd(data$NDSI.2, na.rm = TRUE)
print(sd2)
sd3 <- sd(data$NDSI.3, na.rm = TRUE)
print(sd3)
sd4 <- sd(data$NDSI.4, na.rm = TRUE)
print(sd4)
sd5 <- sd(data$NDSI.5, na.rm = TRUE)
print(sd5)
sd6 <- sd(data$NDSI.6, na.rm = TRUE)
print(sd6)
sd7 <- sd(data$NDSI.7, na.rm = TRUE)
print(sd7)
sd8 <- sd(data$NDSI.8, na.rm = TRUE)
print(sd8)
sdA <- sd(data$NDSI.A, na.rm = TRUE)
print(sdA)
sdB <- sd(data$NDSI.B, na.rm = TRUE)
print(sdB)
sdC <- sd(data$NDSI.C, na.rm = TRUE)
print(sdC)

# Calculate all standard deviations
sds <- c(
  NDSI.1 = sd(data$NDSI.1, na.rm = TRUE),
  NDSI.2 = sd(data$NDSI.2, na.rm = TRUE),
  NDSI.3 = sd(data$NDSI.3, na.rm = TRUE),
  NDSI.4 = sd(data$NDSI.4, na.rm = TRUE),
  NDSI.5 = sd(data$NDSI.5, na.rm = TRUE),
  NDSI.6 = sd(data$NDSI.6, na.rm = TRUE),
  NDSI.7 = sd(data$NDSI.7, na.rm = TRUE),
  NDSI.8 = sd(data$NDSI.8, na.rm = TRUE),
  NDSI.A = sd(data$NDSI.A, na.rm = TRUE),
  NDSI.B = sd(data$NDSI.B, na.rm = TRUE),
  NDSI.C = sd(data$NDSI.C, na.rm = TRUE)
)

# View all SD values together
print(sds)
View(sds)



#mean anthro
meananthro <-  c(
  A1 = mean(data$A1, na.rm = TRUE),
  A2 = mean(data$A2, na.rm = TRUE),
  A3 = mean(data$A3, na.rm = TRUE),
  A4 = mean(data$A4, na.rm = TRUE),
  A5 = mean(data$A5, na.rm = TRUE),
  A6 = mean(data$A6, na.rm = TRUE),
  A7 = mean(data$A7, na.rm = TRUE),
  A8 = mean(data$A8, na.rm = TRUE),
  AA = mean(data$AA, na.rm = TRUE),
  AB = mean(data$AB, na.rm = TRUE),
  AC = mean(data$AC, na.rm = TRUE)
)
View(meananthro)
print(meananthro)
#sd anthro
sdanthro <-  c(
  A1 = sd(data$A1, na.rm = TRUE),
  A2 = sd(data$A2, na.rm = TRUE),
  A3 = sd(data$A3, na.rm = TRUE),
  A4 = sd(data$A4, na.rm = TRUE),
  A5 = sd(data$A5, na.rm = TRUE),
  A6 = sd(data$A6, na.rm = TRUE),
  A7 = sd(data$A7, na.rm = TRUE),
  A8 = sd(data$A8, na.rm = TRUE),
  AA = sd(data$AA, na.rm = TRUE),
  AB = sd(data$AB, na.rm = TRUE),
  AC = sd(data$AC, na.rm = TRUE)
)
print(sdanthro)


#mean bio
meanbio <- c(
  B1 = mean(data$B1, na.rm = TRUE),
  B2 = mean(data$B2, na.rm = TRUE),
  B3 = mean(data$B3, na.rm = TRUE),
  B4 = mean(data$B4, na.rm = TRUE),
  B5 = mean(data$B5, na.rm = TRUE),
  B6 = mean(data$B6, na.rm = TRUE),
  B7 = mean(data$B7, na.rm = TRUE),
  B8 = mean(data$B8, na.rm = TRUE),
  BA = mean(data$BA, na.rm = TRUE),
  BB = mean(data$BB, na.rm = TRUE),
  BC = mean(data$BC, na.rm = TRUE)
)
View(meanbioo)
print(meanbio)
#sd anthro
sdbio <- c(
  B1 = sd(data$B1, na.rm = TRUE),
  B2 = sd(data$B2, na.rm = TRUE),
  B3 = sd(data$B3, na.rm = TRUE),
  B4 = sd(data$B4, na.rm = TRUE),
  B5 = sd(data$B5, na.rm = TRUE),
  B6 = sd(data$B6, na.rm = TRUE),
  B7 = sd(data$B7, na.rm = TRUE),
  B8 = sd(data$B8, na.rm = TRUE),
  BA = sd(data$BA, na.rm = TRUE),
  BB = sd(data$BB, na.rm = TRUE),
  BC = sd(data$BC, na.rm = TRUE)
)

print(sdbio)
# Convert NDSI columns to numeric (safely, in case of non-numeric strings)
cols <- c("NDSI 1 day", "NDSI 2 day", "NDSI 3 day", "NDSI 4 day", "NDSI 5 day",
          "NDSI 6 day", "NDSI 7 day", "NDSI 8 day", "NDSI A day", "NDSI B day", "NDSI C day")

# Use lapply to convert each to numeric
data[cols] <- lapply(data[cols], function(x) as.numeric(as.character(x)))


#mean NDSI day
meanNDSIday <- c(
  NDSId1 = mean(data$NDSI.1.day, na.rm = TRUE),
  NDSId2 = mean(data$NDSI.2.day, na.rm = TRUE),
  NDSId3 = mean(data$NDSI.3.day, na.rm = TRUE),
  NDSId4 = mean(data$NDSI.4.day, na.rm = TRUE),
  NDSId5 = mean(data$NDSI.5.day, na.rm = TRUE),
  NDSId6 = mean(data$NDSI.6.day, na.rm = TRUE),
  NDSId7 = mean(data$NDSI.7.day, na.rm = TRUE),
  NDSId8 = mean(data$NDSI.8.day, na.rm = TRUE),
  NDSIdA = mean(data$NDSI.A.day, na.rm = TRUE),
  NDSIdB = mean(data$NDSI.B.day, na.rm = TRUE),
  NDSIdC = mean(data$NDSI.C.day, na.rm = TRUE)
)
print(meanNDSIday)
warnings()
#mean bio day

meanbioDay <- c(
  B1 = mean(data$B1.day, na.rm = TRUE),
  B2 = mean(data$B2.day, na.rm = TRUE),
  B3 = mean(data$B3.day, na.rm = TRUE),
  B4 = mean(data$B4.day, na.rm = TRUE),
  B5 = mean(data$B5.day, na.rm = TRUE),
  B6 = mean(data$B6.day, na.rm = TRUE),
  B7 = mean(data$B7.day, na.rm = TRUE),
  B8 = mean(data$B8.day, na.rm = TRUE),
  BA = mean(data$BA.day, na.rm = TRUE),
  BB = mean(data$BB.day, na.rm = TRUE),
  BC = mean(data$BC.day, na.rm = TRUE)
)
print(meanbioDay)
#mean anthro day

meanAnthroDay <- c(
  anthrod1 = mean(data$A1.day, na.rm = TRUE),
  anthrod2 = mean(data$A2.day, na.rm = TRUE),
  anthrod3 = mean(data$A3.day, na.rm = TRUE),
  anthrod4 = mean(data$A4.day, na.rm = TRUE),
  anthrod5 = mean(data$A5.day, na.rm = TRUE),
  anthrod6 = mean(data$A6.day, na.rm = TRUE),
  anthrod7 = mean(data$A7.day, na.rm = TRUE),
  anthrod8 = mean(data$A8.day, na.rm = TRUE),
  anthrodA = mean(data$AA.day, na.rm = TRUE),
  anthrodB = mean(data$AB.day, na.rm = TRUE),
  anthrodC = mean(data$AC.day, na.rm = TRUE)
)
print(meanAnthroDay)

#mean NDSI night

meanNDSInight <- c(
  NDSIn1 = mean(data$NDSI.1.night, na.rm = TRUE),
  NDSIn2 = mean(data$NDSI.2.night, na.rm = TRUE),
  NDSIn3 = mean(data$NDSI.3.night, na.rm = TRUE),
  NDSIn4 = mean(data$NDSI.4.night, na.rm = TRUE),
  NDSIn5 = mean(data$NDSI.5.night, na.rm = TRUE),
  NDSIn6 = mean(data$NDSI.6.night, na.rm = TRUE),
  NDSIn7 = mean(data$NDSI.7.night, na.rm = TRUE),
  NDSIn8 = mean(data$NDSI.8.night, na.rm = TRUE),
  NDSInA = mean(data$NDSI.A.night, na.rm = TRUE),
  NDSInB = mean(data$NDSI.B.night, na.rm = TRUE),
  NDSInC = mean(data$NDSI.C.night, na.rm = TRUE)
)
print(meanNDSInight)



#mean bio night

meanBioNight <- c(
  B1 = mean(data$B1.night, na.rm = TRUE),
  B2 = mean(data$B2.night, na.rm = TRUE),
  B3 = mean(data$B3.night, na.rm = TRUE),
  B4 = mean(data$B4.night, na.rm = TRUE),
  B5 = mean(data$B5.night, na.rm = TRUE),
  B6 = mean(data$B6.night, na.rm = TRUE),
  B7 = mean(data$B7.night, na.rm = TRUE),
  B8 = mean(data$B8.night, na.rm = TRUE),
  BA = mean(data$BA.night, na.rm = TRUE),
  BB = mean(data$BB.night, na.rm = TRUE),
  BC = mean(data$BC.night, na.rm = TRUE)
)
print(meanBioNight)

#mean anthro night

meanAnthroNight <- c(
  anthron1 = mean(data$A1.night, na.rm = TRUE),
  anthron2 = mean(data$A2.night, na.rm = TRUE),
  anthron3 = mean(data$A3.night, na.rm = TRUE),
  anthron4 = mean(data$A4.night, na.rm = TRUE),
  anthron5 = mean(data$A5.night, na.rm = TRUE),
  anthron6 = mean(data$A6.night, na.rm = TRUE),
  anthron7 = mean(data$A7.night, na.rm = TRUE),
  anthron8 = mean(data$A8.night, na.rm = TRUE),
  anthronA = mean(data$AA.night, na.rm = TRUE),
  anthronB = mean(data$AB.night, na.rm = TRUE),
  anthronC = mean(data$AC.night, na.rm = TRUE)
)
print(meanAnthroNight)



#sd NDSI day

sdNDSIday <- c(
  NDSId1 = sd(data$NDSI.1.day, na.rm = TRUE),
  NDSId2 = sd(data$NDSI.2.day, na.rm = TRUE),
  NDSId3 = sd(data$NDSI.3.day, na.rm = TRUE),
  NDSId4 = sd(data$NDSI.4.day, na.rm = TRUE),
  NDSId5 = sd(data$NDSI.5.day, na.rm = TRUE),
  NDSId6 = sd(data$NDSI.6.day, na.rm = TRUE),
  NDSId7 = sd(data$NDSI.7.day, na.rm = TRUE),
  NDSId8 = sd(data$NDSI.8.day, na.rm = TRUE),
  NDSIdA = sd(data$NDSI.A.day, na.rm = TRUE),
  NDSIdB = sd(data$NDSI.B.day, na.rm = TRUE),
  NDSIdC = sd(data$NDSI.C.day, na.rm = TRUE)
)

print(sdNDSIday)
#sd bio day

sdbioDay <- c(
  B1 = sd(data$B1.day, na.rm = TRUE),
  B2 = sd(data$B2.day, na.rm = TRUE),
  B3 = sd(data$B3.day, na.rm = TRUE),
  B4 = sd(data$B4.day, na.rm = TRUE),
  B5 = sd(data$B5.day, na.rm = TRUE),
  B6 = sd(data$B6.day, na.rm = TRUE),
  B7 = sd(data$B7.day, na.rm = TRUE),
  B8 = sd(data$B8.day, na.rm = TRUE),
  BA = sd(data$BA.day, na.rm = TRUE),
  BB = sd(data$BB.day, na.rm = TRUE),
  BC = sd(data$BC.day, na.rm = TRUE)
)
print(sdbioDay)

#sd anthro day

sdAnthroDay <- c(
  anthrod1 = sd(data$A1.day, na.rm = TRUE),
  anthrod2 = sd(data$A2.day, na.rm = TRUE),
  anthrod3 = sd(data$A3.day, na.rm = TRUE),
  anthrod4 = sd(data$A4.day, na.rm = TRUE),
  anthrod5 = sd(data$A5.day, na.rm = TRUE),
  anthrod6 = sd(data$A6.day, na.rm = TRUE),
  anthrod7 = sd(data$A7.day, na.rm = TRUE),
  anthrod8 = sd(data$A8.day, na.rm = TRUE),
  anthrodA = sd(data$AA.day, na.rm = TRUE),
  anthrodB = sd(data$AB.day, na.rm = TRUE),
  anthrodC = sd(data$AC.day, na.rm = TRUE)
)

print(sdAnthroDay)

#sd NDSI night

sdNDSInight <- c(
  NDSIn1 = sd(data$NDSI.1.night, na.rm = TRUE),
  NDSIn2 = sd(data$NDSI.2.night, na.rm = TRUE),
  NDSIn3 = sd(data$NDSI.3.night, na.rm = TRUE),
  NDSIn4 = sd(data$NDSI.4.night, na.rm = TRUE),
  NDSIn5 = sd(data$NDSI.5.night, na.rm = TRUE),
  NDSIn6 = sd(data$NDSI.6.night, na.rm = TRUE),
  NDSIn7 = sd(data$NDSI.7.night, na.rm = TRUE),
  NDSIn8 = sd(data$NDSI.8.night, na.rm = TRUE),
  NDSInA = sd(data$NDSI.A.night, na.rm = TRUE),
  NDSInB = sd(data$NDSI.B.night, na.rm = TRUE),
  NDSInC = sd(data$NDSI.C.night, na.rm = TRUE)
)
print(sdNDSInight)


#sd bio night

sdBioNight <- c(
  B1 = sd(data$B1.night, na.rm = TRUE),
  B2 = sd(data$B2.night, na.rm = TRUE),
  B3 = sd(data$B3.night, na.rm = TRUE),
  B4 = sd(data$B4.night, na.rm = TRUE),
  B5 = sd(data$B5.night, na.rm = TRUE),
  B6 = sd(data$B6.night, na.rm = TRUE),
  B7 = sd(data$B7.night, na.rm = TRUE),
  B8 = sd(data$B8.night, na.rm = TRUE),
  BA = sd(data$BA.night, na.rm = TRUE),
  BB = sd(data$BB.night, na.rm = TRUE),
  BC = sd(data$BC.night, na.rm = TRUE)
)
print(sdBioNight)


#sd anthro night

sdAnthroNight <- c(
  anthron1 = sd(data$A1.night, na.rm = TRUE),
  anthron2 = sd(data$A2.night, na.rm = TRUE),
  anthron3 = sd(data$A3.night, na.rm = TRUE),
  anthron4 = sd(data$A4.night, na.rm = TRUE),
  anthron5 = sd(data$A5.night, na.rm = TRUE),
  anthron6 = sd(data$A6.night, na.rm = TRUE),
  anthron7 = sd(data$A7.night, na.rm = TRUE),
  anthron8 = sd(data$A8.night, na.rm = TRUE),
  anthronA = sd(data$AA.night, na.rm = TRUE),
  anthronB = sd(data$AB.night, na.rm = TRUE),
  anthronC = sd(data$AC.night, na.rm = TRUE)
)
print(sdAnthroNight)




rush <- read.csv("C:/Users/anyam/OneDrive/Desktop/Biology/FYP/Epping/rushhour.csv")
# Calculate mean for rush hour NDSI 1 to 8, A, B, and C
RHNDSImean <- c(
  RHNDSI1mean = mean(rush$NDSI_1, na.rm = TRUE),
  RHNDSI2mean = mean(rush$NDSI_2, na.rm = TRUE),
  RHNDSI3mean = mean(rush$NDSI_3, na.rm = TRUE),
  RHNDSI4mean = mean(rush$NDSI_4, na.rm = TRUE),
  RHNDSI5mean = mean(rush$NDSI_5, na.rm = TRUE),
  RHNDSI6mean = mean(rush$NDSI_6, na.rm = TRUE),
  RHNDSI7mean = mean(rush$NDSI_7, na.rm = TRUE),
  RHNDSI8mean = mean(rush$NDSI_8, na.rm = TRUE),
  RHNDSI_Amean = mean(rush$NDSI_A, na.rm = TRUE),
  RHNDSI_Bmean = mean(rush$NDSI_B, na.rm = TRUE),
  RHNDSI_Cmean = mean(rush$NDSI_C, na.rm = TRUE)
)
print(RHNDSImean)
RHNDSIsd <- c(
  RHNDSI1sd = sd(rush$NDSI_1, na.rm = TRUE),
  RHNDSI2sd = sd(rush$NDSI_2, na.rm = TRUE),
  RHNDSI3sd = sd(rush$NDSI_3, na.rm = TRUE),
  RHNDSI4sd = sd(rush$NDSI_4, na.rm = TRUE),
  RHNDSI5sd = sd(rush$NDSI_5, na.rm = TRUE),
  RHNDSI6sd = sd(rush$NDSI_6, na.rm = TRUE),
  RHNDSI7sd = sd(rush$NDSI_7, na.rm = TRUE),
  RHNDSI8sd = sd(rush$NDSI_8, na.rm = TRUE),
  RHNDSI_Asd = sd(rush$NDSI_A, na.rm = TRUE),
  RHNDSI_Bsd = sd(rush$NDSI_B, na.rm = TRUE),
  RHNDSI_Csd = sd(rush$NDSI_C, na.rm = TRUE)
)
print(RHNDSIsd)

RHAnthromean <- c(
  RHAnthro1mean = mean(rush$Anthro_1, na.rm = TRUE),
  RHAnthro2mean = mean(rush$Anthro_2, na.rm = TRUE),
  RHAnthro3mean = mean(rush$Anthro_3, na.rm = TRUE),
  RHAnthro4mean = mean(rush$Anthro_4, na.rm = TRUE),
  RHAnthro5mean = mean(rush$Anthro_5, na.rm = TRUE),
  RHAnthro6mean = mean(rush$Anthro_6, na.rm = TRUE),
  RHAnthro7mean = mean(rush$Anthro_7, na.rm = TRUE),
  RHAnthro8mean = mean(rush$Anthro_8, na.rm = TRUE),
  RHAnthro_Amean = mean(rush$Anthro_A, na.rm = TRUE),
  RHAnthro_Bmean = mean(rush$Anthro_B, na.rm = TRUE),
  RHAnthro_Cmean = mean(rush$Anthro_C, na.rm = TRUE)
)
print(RHAnthromean)

RHAnthrosd <- c(
  RHAnthro1sd = sd(rush$Anthro_1, na.rm = TRUE),
  RHAnthro2sd = sd(rush$Anthro_2, na.rm = TRUE),
  RHAnthro3sd = sd(rush$Anthro_3, na.rm = TRUE),
  RHAnthro4sd = sd(rush$Anthro_4, na.rm = TRUE),
  RHAnthro5sd = sd(rush$Anthro_5, na.rm = TRUE),
  RHAnthro6sd = sd(rush$Anthro_6, na.rm = TRUE),
  RHAnthro7sd = sd(rush$Anthro_7, na.rm = TRUE),
  RHAnthro8sd = sd(rush$Anthro_8, na.rm = TRUE),
  RHAnthro_Asd = sd(rush$Anthro_A, na.rm = TRUE),
  RHAnthro_Bsd = sd(rush$Anthro_B, na.rm = TRUE),
  RHAnthro_Csd = sd(rush$Anthro_C, na.rm = TRUE)
)
print(RHAnthrosd)


