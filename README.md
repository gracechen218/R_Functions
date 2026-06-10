# 📦 R\_Functions

A curated collection of reusable R functions designed to improve efficiency in **ADaM dataset preparation** and **TLF (Tables, Listings, and Figures) programming** for clinical and regulatory reporting.

This repository will be **continuously updated** with new utilities, improvements, and optimizations to streamline common workflows in statistical programming.

***

## 🚀 Purpose

The goal of this repository is to:

* Reduce repetitive coding in ADaM/TLF workflows
* Standardize outputs for regulatory submission and reporting
* Provide ready-to-use, extensible functions for clinical data analysis
* Improve programming efficiency and reproducibility

***

## 📂 Current Functions

### 1. `RTF_Functions.R`

**Description:**  
A set of ready-to-use functions that convert R data frames into **submission-ready RTF outputs**.

**Key Features:**

* Direct export from R to formatted RTF tables
* Suitable for regulatory submissions
* Supports customization of layout and formatting
* Reduces manual formatting work in Word

**Use Case:**  
Quick generation of TLF tables formatted for submission without additional post-processing.

***

### 2. `Summary_Stats_Function.R`

**Description:**  
Functions to generate **summary statistics tables** optimized for TLF generation in regulatory reporting.

**Includes:**

* Separate functions for:
  * **Numeric variables** (e.g., mean, SD, median, range)
  * **Categorical variables** (e.g., counts and percentages)

**Key Features:**

* Output formatted for downstream TLF generation
* Standardized summary structures
* Flexible input handling

**Use Case:**  
Efficiently create summary statistics tables for clinical study reports (CSR) and regulatory submissions.

***

### 3. `Univariate_Function.R`

**Description:**  
Generates **publication- and presentation-ready summary tables** for univariate analyses.

**Supported Data Types:**

* Numeric predictors
* Categorical predictors

**Key Features:**

* Clean, interpretable output formats
* Suitable for publications and slide decks
* Combines statistical summaries into a single table

**Use Case:**  
Exploratory analysis, manuscript tables, and presentation-ready outputs.

***

## 🛠️ Installation & Usage

Clone the repository:

```bash
git clone https://github.com/<your-username>/R_Functions.git
```

Then source the desired function in R:

```r
source("RTF_Functions.R")
source("Summary_Stats_Function.R")
source("Univariate_Function.R")
```

***

## 🤝 Contributions

Contributions, suggestions, and improvements are welcome!

If you have ideas to improve efficiency in ADaM/TLF programming, feel free to:

* Open an issue
* Submit a pull request

***



