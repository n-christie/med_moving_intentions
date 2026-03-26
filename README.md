# Medical Moving Intentions

> Brief one-line description of the paper.

**Status:** In progress  
**Authors:** [Your Name], [Co-author Names]  
**Target journal:** [Journal]

---

## Project structure

```
med_moving_intentions/
├── data/
│   ├── raw/          # Original, unmodified survey data (never edit)
│   ├── processed/    # Cleaned/transformed datasets
│   └── codebooks/    # Variable labels, survey instruments
├── scripts/
│   ├── 01_clean/     # Data cleaning and preparation
│   ├── 02_analyze/   # Statistical analyses
│   └── 03_visualize/ # Figures and tables
├── output/
│   ├── figures/      # Saved plots (.png, .pdf)
│   └── tables/       # Saved tables (.csv, .html, .docx)
├── paper/            # Manuscript drafts and submission files
├── docs/             # About page (shared with co-authors)
├── AGENTS.md         # Instructions for Claude Code
└── README.md
```

## Reproducibility

Run scripts in numbered order within each stage:

```r
# 1. Clean
source("scripts/01_clean/01_import.R")

# 2. Analyze
source("scripts/02_analyze/01_descriptives.R")

# 3. Visualize
source("scripts/03_visualize/01_figures.R")
```

## Data

Raw data lives in `data/raw/` and is excluded from version control (see `.gitignore`).  
A synthetic or anonymized sample may be provided in `data/processed/` for reproducibility.

## Contact

[Your name and email]
