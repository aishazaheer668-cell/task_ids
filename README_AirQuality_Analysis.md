# 🌍 Air Quality and Respiratory Disease Analysis

## 📝 Project Description

This project analyzes the relationship between **air pollution** (PM2.5 and PM10 levels) and **respiratory disease incidence** across 30 global cities.  
It uses datasets from the **World Health Organization (WHO)** and **Global Health Observatory (GHO)** to explore how air quality impacts public health.

The workflow includes:
- **Data Cleaning:** Handling missing values and filtering incomplete records.  
- **Data Transformation:** Standardizing disease rates per 100,000 population and calculating deviations from WHO pollution limits.  
- **Analysis:** Measuring correlations between pollution and disease incidence.  
- **Visualization:** Creating scatter plots and bar charts to identify high-risk cities.  
- **Output:** Exporting a CSV file containing cities with the highest pollution and disease rates.

This study provides insights into which urban areas face the greatest public health risks from air pollution, helping guide policy and awareness efforts.

---

## ⚙️ Project Workflow (R Script)

### Step 1: Load Libraries
```r
library(tidyverse)
library(ggplot2)
```

### Step 2: Import Data
```r
air_quality <- read.csv("WHO_AirPollution_30Cities.csv")
health_data <- read.csv("GHO_Respiratory_30Cities.csv")
View(air_quality)
View(health_data)
```

### Step 3: Data Cleaning
```r
air_quality <- air_quality %>%
  filter(!is.na(PM2.5), !is.na(PM10)) %>%
  group_by(City) %>%
  mutate(
    PM2.5 = ifelse(is.na(PM2.5), mean(PM2.5, na.rm = TRUE), PM2.5),
    PM10 = ifelse(is.na(PM10), mean(PM10, na.rm = TRUE), PM10)
  )

if ("Cases" %in% names(health_data) & "Population" %in% names(health_data)) {
  health_data <- health_data %>%
    mutate(DiseaseRate = (Cases / Population) * 100000)
}
```

### Step 4: Data Transformation
```r
air_quality <- air_quality %>%
  mutate(
    PM2.5_Deviation = PM2.5 - 5,
    PM10_Deviation = PM10 - 15
  )
merged_data <- merge(air_quality, health_data, by = c("City", "Year"))
```

### Step 5: Analysis
```r
cor_PM25 <- cor(merged_data$PM2.5, merged_data$DiseaseRate, use = "complete.obs")
cor_PM10 <- cor(merged_data$PM10, merged_data$DiseaseRate, use = "complete.obs")
cat("Correlation (PM2.5 vs Disease Rate):", cor_PM25, "\n")
cat("Correlation (PM10 vs Disease Rate):", cor_PM10, "\n")
```

### Step 6: Identify High-Risk Zones
```r
high_risk_zones <- merged_data %>%
  filter(PM2.5_Deviation > 10 & DiseaseRate > 150)
print(high_risk_zones[, c("City", "Country.x", "PM2.5", "DiseaseRate")])
```

### Step 7: Visualization
```r
# Scatter Plot
ggplot(merged_data, aes(x = PM2.5, y = DiseaseRate)) +
  geom_point(color = "steelblue") +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "PM2.5 vs Respiratory Disease Incidence",
       x = "PM2.5 Concentration (µg/m³)",
       y = "Disease Incidence per 100,000") +
  theme_minimal()

# High-Risk Cities Bar Plot
ggplot(high_risk_zones, aes(x = reorder(City, DiseaseRate), y = DiseaseRate, fill = PM2.5)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "High-Risk Cities (High Pollution + High Disease Rate)",
       x = "City",
       y = "Respiratory Disease Incidence") +
  theme_minimal()
```

### Step 8: Save Results
```r
write.csv(high_risk_zones, "High_Risk_Zones.csv", row.names = FALSE)
cat("\n✅ Analysis Completed! High-risk zones saved as 'High_Risk_Zones.csv'\n")
```
