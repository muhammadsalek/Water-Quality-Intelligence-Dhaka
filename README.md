# AquaLens — Water Quality Dashboard

## Interactive Water Quality Monitoring and Analysis Dashboard

<div align="center">

[![License](https://img.shields.io/badge/License-MIT-10b981?style=for-the-badge&labelColor=0d1117&logo=opensourceinitiative&logoColor=10b981)](LICENSE)
[![Status](https://img.shields.io/badge/Status-Live%20App-7c3aed?style=for-the-badge&labelColor=0d1117)](https://salek.shinyapps.io/aqualens/)
[![Platform](https://img.shields.io/badge/Platform-shinyapps.io-00d4ff?style=for-the-badge&labelColor=0d1117)](https://salek.shinyapps.io/aqualens/)

</div>

<div align="center">

![R](https://img.shields.io/badge/R_4.2+-276DC3?style=flat-square&logo=r&logoColor=white)
![Shiny](https://img.shields.io/badge/Shiny-interactive%20app-ef4444?style=flat-square&logo=r&logoColor=white)
![rsconnect](https://img.shields.io/badge/rsconnect-deployment-1a73e8?style=flat-square&logoColor=white)
![ggplot2](https://img.shields.io/badge/ggplot2-ef4444?style=flat-square&logoColor=white)
![tidyverse](https://img.shields.io/badge/tidyverse-1a73e8?style=flat-square&logo=r&logoColor=white)

</div>

---

## 🌊 Overview

**AquaLens** is an interactive R Shiny web application for water quality monitoring and analysis. It provides visual dashboards and analytical tools for exploring water quality parameters, enabling researchers, policymakers, and public health professionals to identify patterns and make data-driven decisions.

🔗 **Live App:** [https://salek.shinyapps.io/aqualens/](https://salek.shinyapps.io/aqualens/)

**Pipeline:**

```
Water Quality Data
        │
        ▼
  Data Preprocessing     cleaning · recoding · formatting
        │
        ▼
  Analysis & Modeling    statistical summaries · trend analysis
      (R / Shiny)        parameter monitoring
        │
        ▼
  Interactive Dashboard  reactive UI · dynamic charts
      (R Shiny)          user-controlled filtering
        │
        ▼
  Deployment             shinyapps.io · rsconnect
```

---

## 🚀 Live Deployment

The app is deployed on **shinyapps.io** and accessible at:

> **[https://salek.shinyapps.io/aqualens/](https://salek.shinyapps.io/aqualens/)**

---

## Authors

**Developer & Analyst**

**Md Salek Miah**
Research Assistant, Department of Statistics
Shahjalal University of Science and Technology (SUST), Sylhet-3114, Bangladesh
📧 [saleksta@gmail.com](mailto:saleksta@gmail.com)
[![ORCID](https://img.shields.io/badge/ORCID-0009--0005--5973--461X-A6CE39?style=flat-square&logo=orcid&logoColor=white)](https://orcid.org/0009-0005-5973-461X)
[![LinkedIn](https://img.shields.io/badge/LinkedIn-Md_Salek_Miah-0077B5?style=flat-square&logo=linkedin&logoColor=white)](https://www.linkedin.com/in/md-salek-miah-b34309329/)

---

**Supervisor**

**Md Jamal Uddin, Ph.D.**
Professor, Department of Statistics
Shahjalal University of Science and Technology (SUST), Sylhet-3114, Bangladesh
📧 [jamal-sta@sust.edu](mailto:jamal-sta@sust.edu)
[![ORCID](https://img.shields.io/badge/ORCID-0000--0002--8360--3274-A6CE39?style=flat-square&logo=orcid&logoColor=white)](https://orcid.org/0000-0002-8360-3274)

---

**Affiliation**

Biostatistics, Epidemiology, and Public Health Research Team
Department of Statistics, Shahjalal University of Science & Technology (SUST), Sylhet-3114, Bangladesh

---

## Repository Structure

```
Water Quality/
│
├── README.md
├── app.R              ← Main Shiny application (UI + Server)
├── deploy.R           ← Deployment script for shinyapps.io
├── data/              ← Water quality datasets
└── www/               ← Static assets (CSS, images)
```

---

## Quick Start

### Requirements

- R `>= 4.2`
- `rsconnect` package
- A [shinyapps.io](https://www.shinyapps.io/) account

### Step 1 — Run Locally

```r
# Install required packages
packages <- c("shiny", "ggplot2", "tidyverse", "rsconnect")
installed <- packages %in% rownames(installed.packages())
if (any(!installed)) install.packages(packages[!installed])

# Launch the app
shiny::runApp("path/to/Water Quality")
```

### Step 2 — Deploy to shinyapps.io

```r
# Set working directory
setwd("path/to/Water Quality")

# Load rsconnect
library(rsconnect)

# Configure account credentials
rsconnect::setAccountInfo(
  name   = 'your_account_name',
  token  = 'YOUR_TOKEN',
  secret = 'YOUR_SECRET'
)

# Deploy
rsconnect::deployApp(
  appDir         = getwd(),
  appName        = "aqualens",
  account        = "your_account_name",
  forceUpdate    = TRUE,
  launch.browser = TRUE
)
```

> **Note:** Get your token and secret from your [shinyapps.io dashboard](https://www.shinyapps.io/) under Account → Tokens.

---

## Key Highlights

| Feature | Details |
|:--------|:--------|
| **App Type** | R Shiny interactive dashboard |
| **Hosting** | shinyapps.io |
| **Deployment Tool** | `rsconnect` R package |
| **App Name** | `aqualens` |
| **Primary Language** | R |

---

## Deployment Notes

- Old `rsconnect/` cache folder is removed before each fresh deployment to avoid stale configuration conflicts.
- `forceUpdate = TRUE` ensures the latest version always overwrites the previous deployment.
- The browser launches automatically after a successful deployment.

---

## Citation

```bibtex
@software{miah_aqualens_2025,
  title  = {AquaLens: Interactive Water Quality Monitoring Dashboard},
  author = {Miah, Md Salek},
  year   = {2025},
  url    = {https://salek.shinyapps.io/aqualens/}
}
```

---

## License

MIT License — Copyright (c) 2025 Md Salek Miah
Open for academic and research use. Citation appreciated.

---

<div align="center">

**Biostatistics, Epidemiology, and Public Health Research Team**
Department of Statistics · Shahjalal University of Science and Technology · Sylhet-3114, Bangladesh

[![Made with R](https://img.shields.io/badge/Made%20with-R-276DC3?style=flat-square&logo=r&logoColor=white)](https://r-project.org)
[![Shiny](https://img.shields.io/badge/Powered%20by-Shiny-ef4444?style=flat-square&logo=r&logoColor=white)](https://shiny.posit.co)
[![Hosted on shinyapps.io](https://img.shields.io/badge/Hosted%20on-shinyapps.io-00d4ff?style=flat-square)](https://salek.shinyapps.io/aqualens/)
[![SUST](https://img.shields.io/badge/University-SUST%20Bangladesh-f59e0b?style=flat-square)](https://www.sust.edu)

*⭐ Star this repo if it helped your research!*

</div>
