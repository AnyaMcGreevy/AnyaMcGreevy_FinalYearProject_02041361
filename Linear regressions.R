overall <- read.csv("C:/Users/anyam/OneDrive/Desktop/Biology/FYP/Epping/Comparison.csv")
library(ggplot2)
library(tidyverse)

#FOREST COVER

fifty <- glm(SpeciesRichness ~ log50m, data = overall, family = quasipoisson)
summary(fifty)

hundred <- glm(SpeciesRichness ~ log100m, data = overall, family = quasipoisson)
summary(hundred)

onefifty <- glm(SpeciesRichness ~ log150m, data = overall, family = quasipoisson)
summary(onefifty)

twohundred <- glm(SpeciesRichness ~ log200m, data = overall, family = quasipoisson)
summary(twohundred)

# Base plot
ggplot(overall, aes(x = X200m.1, y = SpeciesRichness)) +
  geom_point(color = "#36343F", size = 3) +  # scatter plot
  stat_smooth(method = "glm", method.args = list(family = "quasipoisson"),
              se = TRUE, color = "#36343F", fill = "darkgrey", alpha = 0.2) +  # model fit with CI
  labs(
    x = "log(%Cover + 1) at 200m buffer radius",
    y = "Species Richness"
  ) +
  theme_minimal(base_size = 14)


twofifty  <- glm(SpeciesRichness ~ log250m, data = overall, family = quasipoisson)
summary(twofifty)

threehundred  <- glm(SpeciesRichness ~ log300m, data = overall, family = quasipoisson)
summary(threehundred)

#R^2

df <- data.frame(
  Percentage = c(50, 100, 150, 200, 250, 300),
  R2 = c(0.296772009,
         0.150180587,
         0.408374718,
         0.516726862,
         0.366591422,
         0.216817156
  )
)
ggplot(df, aes(x = Percentage, y = R2)) +
  geom_point(color = "black", size = 3) +
  geom_smooth(method = "loess", se = FALSE, color = "black", linetype = "solid") +
  labs(x = "Buffer Radius (m)", y = expression(R^2))



#//////////////////////////////////
#NDSI overall

NDSImean <- glm(SpeciesRichness ~ NDSI_mean, data = overall, family = quasipoisson)
summary(NDSImean)

NDSIsd <- glm(SpeciesRichness ~ NDSI_sd, data = overall, family = quasipoisson)
summary(NDSIsd)

NDSIcv <- glm(SpeciesRichness ~ NDSI_cv, data = overall, family = quasipoisson)
summary(NDSIcv)


#daytime NDSI
NDSIdaymean <- glm(SpeciesRichness ~ NDSIday_mean, data = overall, family = quasipoisson)
summary(NDSIdaymean)

NDSIdaysd <- glm(SpeciesRichness ~ NDSIday_sd, data = overall, family = quasipoisson)
summary(NDSIdaysd)

NDSIdaycv <- glm(SpeciesRichness ~ NDSIday_cv, data = overall, family = quasipoisson)
summary(NDSIdaycv)

ggplot(overall, aes(x = NDSIday_cv, y = SpeciesRichness)) +
  geom_point(color = "#36343F", size = 3) +  # scatter plot
  stat_smooth(method = "glm", method.args = list(family = "quasipoisson"),
              se = TRUE, color = "#36343F", fill = "darkgrey", alpha = 0.2) +  # model fit with CI
  labs(
    x = "Coefficient of Variation of day time NDSI values",
    y = "Species Richness"
  ) +
  theme_minimal(base_size = 14)
library(ggplot2)


#nighttime NDSI
NDSInightmean <- glm(SpeciesRichness ~ NDSInight_mean, data = overall, family = quasipoisson)
summary(NDSInightmean)

NDSInightsd <- glm(SpeciesRichness ~ NDSInight_sd, data = overall, family = quasipoisson)
summary(NDSInightsd)

NDSInightcv <- glm(SpeciesRichness ~ NDSInight_cv, data = overall, family = quasipoisson)
summary(NDSInightcv)


#overall anthro

anthromean <- glm(SpeciesRichness ~ Anthro_mean, data = overall, family = quasipoisson)
summary(anthromean)

anthrosd <- glm(SpeciesRichness ~ Anthro_sd, data = overall, family = quasipoisson)
summary(anthrosd)

anthrocv <- glm(SpeciesRichness ~ Anthro_cv, data = overall, family = quasipoisson)
summary(anthrocv)

#daytime anthro

anthrodaymean <- glm(SpeciesRichness ~ anthroday_mean, data = overall, family = quasipoisson)
summary(anthrodaymean)

anthrodaysd <- glm(SpeciesRichness ~ anthroday_sd, data = overall, family = quasipoisson)
summary(anthrodaysd)

anthrocv <- glm(SpeciesRichness ~ anthroday_cv, data = overall, family = quasipoisson)
summary(anthrocv)

#night time

anthronightmean <- glm(SpeciesRichness ~ anthronight_mean, data = overall, family = quasipoisson)
summary(anthronightmean)

anthronightsd <- glm(SpeciesRichness ~ anthronight_sd, data = overall, family = quasipoisson)
summary(anthronightsd)

anthronightcv <- glm(SpeciesRichness ~ anthronight_cv, data = overall, family = quasipoisson)
summary(anthronightcv)

##ALL ARE UNDER DISPERSED!!!!!
pois_mean <- glm(SpeciesRichness ~ NDSInight_mean, family = poisson, data = overall)
pois_sd <- glm(SpeciesRichness ~ NDSInight_sd, family = poisson, data = overall)
pois_cv <- glm(SpeciesRichness ~ NDSInight_cv, family = poisson, data = overall)
disp_mean <- sum(residuals(pois_mean, type = "pearson")^2) / pois_mean$df.residual
disp_sd <- sum(residuals(pois_sd, type = "pearson")^2) / pois_sd$df.residual
disp_cv <- sum(residuals(pois_cv, type = "pearson")^2) / pois_cv$df.residual

disp_mean
disp_sd
disp_cv

##NDSI RUshhour

NDSIrush <- glm(SpeciesRichness ~ RHNDSI_mean, data = overall, family = quasipoisson)
summary(NDSIrush)

NDSIsd <- glm(SpeciesRichness ~RHNDSI_sd, data = overall, family = quasipoisson)
summary(NDSIsd)

NDSIcv <- glm(SpeciesRichness ~ RHNDSI_cv, data = overall, family = quasipoisson)
summary(NDSIcv)
summary(model)
model_pois <- glm(SpeciesRichness ~ RHNDSI_mean, data = overall, family = poisson)
# Calculate dispersion statistic manually
dispersion <- sum(residuals(model_pois, type = "pearson")^2) / df.residual(model_pois)
dispersion #underdispersed
# Plot to visualize the relationship
ggplot(overall, aes(x = RHNDSI_mean, y = SpeciesRichness)) +
  geom_point(color = "#36343F", size = 3) +  # scatter plot
  stat_smooth(method = "glm", method.args = list(family = "quasipoisson"),
              se = TRUE, color = "#36343F", fill = "darkgrey", alpha = 0.2) +  # model fit with CI
  labs(
    title = "",
    x = "Coefficient of Variation of rush hour NDSI values",
    y = "Species Richness"
  ) +
  theme_minimal(base_size = 14)

Anthrorush <- glm(SpeciesRichness ~ RHAnthro_mean, data = overall, family = quasipoisson)
summary(NDSIrush)

Anthrosd <- glm(SpeciesRichness ~RHAnthro_sd, data = overall, family = quasipoisson)
summary(Anthrosd)

Antrhocv <- glm(SpeciesRichness ~ RHAnthro_cv, data = overall, family = quasipoisson)
summary(Antrhocv)

##///////////////////////////
#DISTANCE FROM M25
# Fit a GLM with Poisson distribution (for count data like species richness)
model <- glm(SpeciesRichness ~ LogDistance, data = overall, family = quasipoisson)

# Check the model summary
summary(model)
model_pois <- glm(SpeciesRichness ~ LogDistance, data = overall, family = poisson)
# Calculate dispersion statistic manually
dispersion <- sum(residuals(model_pois, type = "pearson")^2) / df.residual(model_pois)
dispersion


# Plot to visualize the relationship
plot(overall$LogDistance, overall$SpeciesRichness,
     xlab = "Distance from M25 (m)", ylab = "Species Richness",
     main = "Species Richness vs Distance from M25", pch = 19)
curve(predict(model, newdata = data.frame(DistanceM25 = x), type = "response"),
      add = TRUE, col = "blue", lwd = 2)


#wilcoxon
wilcoxon <- read.csv("C:/Users/anyam/OneDrive/Desktop/Biology/FYP/Epping/motorwaysite.csv")
#overall NDSI
wilcox.test(wilcoxon$NDSI_A, wilcoxon$NDSI_C, paired = TRUE)
wilcox.test(wilcoxon$NDSI_C, wilcoxon$NDSI_A, paired = TRUE)

boxplot(wilcoxon$NDSI_A, wilcoxon$NDSI_C,
        names = c("A- 33.5%", "C- 0.444%"),
        main = "Boxplot of overall  values for site A and C",
        ylab = "NDSI",
        col = c("gray", "gray"))




###
#violin plot to show ndsi distribution
species_richness <- data.frame(
  Site = c("1","2","3","4","5","6","7","8","A","B","C"),
  SpeciesRichness = c(10,6,10,10,10,9,11,10,10,5,8)
)

# Select relevant NDSI columns from your imported NDSI dataset
ndsi_selected <- dplyr::select(NDSI, 
                               NDSI.1.day, NDSI.2.day, NDSI.3.day, NDSI.4.day,
                               NDSI.5.day, NDSI.6.day, NDSI.7.day, NDSI.8.day,
                               NDSI.A.day, NDSI.B.day, NDSI.C.day)

# Rename columns to match site names only (remove "NDSI." and ".day")
colnames(ndsi_selected) <- species_richness$Site

# Convert wide to long format
ndsi_long <- ndsi_selected %>%
  pivot_longer(
    cols = everything(),
    names_to = "Site",
    values_to = "NDSI_value"
  )

# Join with species richness data to add richness info
ndsi_long <- ndsi_long %>%
  left_join(species_richness, by = "Site")

# Set Site factor levels in CHRONOLOGICAL order (1,2,...,A,B,C)
ndsi_long$Site <- factor(ndsi_long$Site, levels = species_richness$Site)

# Plot horizontal violin plot with boxplot overlay
ggplot(ndsi_long, aes(x = Site, y = NDSI_value)) +
  geom_violin(fill = "lightgray", color = "darkgray", width=1.6) +
  geom_boxplot(width = 0.09, fill = "white", outlier.shape = NA) +
  labs(
    x = "Site",
    y = "daytime NDSI value"
  ) +
  theme_minimal() +
  coord_flip()
citation('tidyr')

