## Folder and File Overview

This repository follows a structured workflow. Below is a brief description of each folder and how the files are used.

---

## 📂 Data
- Contains **raw data files** used for analysis.
- Raw data are referenced in `script/cleaning.R`.
- After cleaning, the finalized dataset (`Fullerton_clean_VW.xlsx`) is created here.
- All `.qmd` files reference the cleaned dataset for reporting and visualization.

---

## 📂 Figure
- Stores static figures and visual assets.
- These files are referenced by templates to ensure consistent styling and branding across outputs.

---

## 📂 Info
- Contains the **codebook and reference documentation**.
- Not required for rendering or analysis workflows.

---

## 📂 Script
- `cleaning.R`  
  - This script should be run **first**.
  - It cleans raw data and outputs the finalized dataset used by all reports.
- All `.qmd` files reference the shared template file:  
  `../template/fullerton.R`
  - This file defines libraries, helper functions, themes, and formatting.
- Each `.qmd` file renders as its **own site**.
  - `report.qmd` is the primary landing page.
  - It links to additional pages such as `demographics.html` and `addloutcomes.html`.

---

## 📂 Template (IMPORTANT)
This folder defines the look, feel, and functionality of all outputs.
- `fullerton.css`  
  Controls visual styling and layout.
- `fullerton.html`  
  Sets default HTML structure and page behavior.
- `fullerton.R`  
  **Critical file.**  
  Contains shared libraries, global assignments, and functions used across all `.qmd` files.

---

## 📂 Main Folder / Rendering Workflow
- Files are **not** rendered or moved manually.
- The `render_and_copy.R` script:
  1. Renders all `.qmd` files in the project
  2. Copies the generated `.html` files into the `public` directory
- The `public` folder is pushed to **GitHub Pages** and contains all viewable outputs.

---

This structure ensures a reproducible, automated workflow with consistent formatting across reports.
