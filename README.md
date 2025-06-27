

```markdown
# 📊 ENA Agadir Admission Selection Analysis

This project analyzes the selection and admission process for candidates applying to the École Nationale d’Architecture d’Agadir. It showcases an end-to-end data analysis workflow using **R** and applies fair, logical, and data-driven decision-making throughout the selection pipeline.

---

## 🔍 Project Context

The dataset combines information from:
- ✅ **ENA Rabat**: pre-registration list of students
- ✅ **Massar (Ministry of Education)**: national academic database
- ✅ **Entrance Exam Results**: QCM and DEG scores

Our goal: filter valid candidates, detect anomalies (redoublants, duplicate Bac, low scores), and select top students for admission based on well-defined criteria.

---

## 👤 Author Profile

**IT Developer | Data Analyst | Math Teacher | Marketing-Informed Professional**

This project combines my experience in:
- 🔧 Programming and data structuring
- 📊 Analytical thinking and data visualization
- 📚 Mathematical filtering and statistical validation
- 📣 Communicating data insights for institutional decisions

---

## 🧰 Technologies & Tools

- **Language**: R
- **Libraries**: `dplyr`, `ggplot2`, `readr`, `readxl`, `ggrepel`, `forcats`, `here`, `stringr`
- **Tools**: RStudio, Git, GitHub

---

## 📁 Folder Structure

```

ena-selection-project/
│
├── data/
│   ├── pre\_examen/
│   │   ├── raw/
│   │   └── prepped/
│   ├── post\_examen/
│   │   ├── raw/
│   │   └── prepped/
├── scripts/
│   └── 01\_selection\_process.R
├── output/
│   └── visuals/
├── README.md
└── .gitignore

````

---

## ⚙️ Main Features

- 🔄 **Data Cleaning**: standardize, join, and validate student lists
- 🔍 **Filtering Pipeline**:
  - Redoublants (students repeating the year)
  - Duplicate Bac entries
  - Students with low national/regional scores
  - Candidates above age threshold
  - Invalid Bac streams
- 🧮 **Score Calculation**: combining QCM (60%) and DEG (40%) results
- 🏅 **Final Ranking**: generate list of admitted and waiting list students
- 🧾 **Documentation**: export step-by-step filtered datasets for traceability
- 📊 **Visual Insights**: explore results by region, score, and Bac stream

---

## 📸 Sample Visuals

*(Replace with actual image links)*

![Score Distribution](output/visuals/score_distribution.png)  
*Distribution of Final Scores (QCM + DEG)*

![Region vs Admission](output/visuals/region_admission.png)  
*Admissions by Region Code*

---

## 🧪 How to Run

1. Clone the repository
2. Open the project in RStudio
3. Make sure required packages are installed:
```r
install.packages(c("dplyr", "ggplot2", "readr", "readxl", "ggrepel", "forcats", "here", "stringr"))
````

4. Run the main script:

```r
source("scripts/01_selection_process.R")
```

---

## 📈 Insights From The Project

* Strong correlation between academic performance and entrance scores
* Certain Bac streams outperform others consistently
* Gender or region distribution can be explored for equity analysis

---

## ✅ Use Cases

* For **schools/universities**: create transparent selection pipelines
* For **data analysts**: explore joining, wrangling, and filtering techniques
* For **education researchers**: analyze factors of student success


---

## 💼 Contact

📧 [taha.anouar.wiifm@gmail.com](mailto:taha.anouar.wiifm@gmail.com)
🔗 [LinkedIn](https://www.linkedin.com/in/your-profile)
📍 Morocco / Canada

