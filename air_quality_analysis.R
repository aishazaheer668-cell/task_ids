# --- Step 1: Load Libraries ---
library(tidyverse)
library(ggplot2)

# --- Step 2: Import Data ---
air_quality <- read.csv("air_quality_health_impact_data.csv")

# --- Step 3: Preview ---
head(air_quality)
colnames(air_quality)
summary(air_quality)

# --- Step 4: Data Cleaning ---
## Rename columns to consistent names (remove underscores)
air_quality <- air_quality %>%
  rename(
    PM2.5 = PM2_5,
    DiseaseRate = RespiratoryCases
  )

## Remove missing pollution data
air_quality <- air_quality %>%
  filter(!is.na(PM2.5), !is.na(PM10))

## Replace missing readings using dataset averages
air_quality <- air_quality %>%
  mutate(
    PM2.5 = ifelse(is.na(PM2.5), mean(PM2.5, na.rm = TRUE), PM2.5),
    PM10  = ifelse(is.na(PM10), mean(PM10, na.rm = TRUE), PM10)
  )

# --- Step 5: Data Transformation ---
## Compute deviation from WHO limits
air_quality <- air_quality %>%
  mutate(
    PM2.5_Deviation = PM2.5 - 5,
    PM10_Deviation = PM10 - 15
  )

# --- Step 6: Analysis ---
## Correlation between pollution and respiratory disease
cor_PM25 <- cor(air_quality$PM2.5, air_quality$DiseaseRate, use = "complete.obs")
cor_PM10 <- cor(air_quality$PM10,  air_quality$DiseaseRate, use = "complete.obs")

cat("Correlation (PM2.5 vs Disease Rate):", cor_PM25, "\n")
cat("Correlation (PM10 vs Disease Rate):", cor_PM10, "\n")

# --- Step 7: Identify High-Risk Zones ---
high_risk_zones <- air_quality %>%
  filter(PM2.5_Deviation > 10 & DiseaseRate > mean(DiseaseRate, na.rm = TRUE))

# Display high-risk zones
print(high_risk_zones[, c("RecordID", "PM2.5", "PM10", "DiseaseRate")])

# --- Step 8: Visualization ---
## 1️⃣ Scatter plot: Pollution vs Respiratory Cases
ggplot(air_quality, aes(x = PM2.5, y = DiseaseRate)) +
  geom_point(color = "steelblue") +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(
    title = "PM2.5 vs Respiratory Disease Incidence",
    x = "PM2.5 Concentration (µg/m³)",
    y = "Respiratory Cases"
  ) +
  theme_minimal()
## 2️⃣ High-Risk Zones Bar Plot
ggplot(high_risk_zones, aes(x = reorder(RecordID, DiseaseRate), y = DiseaseRate, fill = PM2.5)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(
    title = "High-Risk Zones (High Pollution + High Respiratory Cases)",
    x = "Record ID",
    y = "Respiratory Cases"
  ) +
  theme_minimal()

# --- Step 9: Save Results ---
write.csv(high_risk_zones, "High_Risk_Zones.csv", row.names = FALSE)
cat("\n✅ Analysis Completed! High-risk zones saved as 'High_Risk_Zones.csv'\n")
