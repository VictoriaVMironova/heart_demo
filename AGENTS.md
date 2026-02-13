# AGENTS.md

Guidance for coding agents working in this repository.

## Project Overview

- Type: R Shiny dashboard (`app.R`) for heart-attack outcomes and charges analysis.
- Data source for runtime app: `data/heart.rds`.
- Core app pattern: one-file app (`app.R`) plus helper/module scripts under `R/`.

## Repository Layout

- `app.R`: Main UI + server logic.
- `R/helpers.R`: Small utility helpers (for example mortality text computation).
- `R/mod_download_plot.R`: Reusable Shiny module that exports plots (PDF/PNG/SVG).
- `R/prepare_data.R`: Script that converts raw text data into `data/heart.rds`.
- `data/heartatk4R.txt`: Raw input data.
- `data/heart.rds`: Prepared data used by app.
- `www/logo.png`: Static asset used in app title.
- `site/`: Built/deployed site artifacts (Shinylive/GitHub Pages output).

## Local Development

- Run app locally from repo root:

```r
shiny::runApp()
```

or

```bash
R -q -e "shiny::runApp()"
```

- If raw data changes, regenerate `data/heart.rds` by running:

```bash
Rscript R/prepare_data.R
```

## Coding Conventions For This Repo

- Keep changes minimal and localized; avoid broad refactors unless requested.
- Preserve existing style in each file (this codebase mixes base R subsetting and tidyverse verbs).
- Prefer creating reactive plot objects once, then reusing for both rendering and downloading.
- For new plots that need export support, use the existing download module (`mod_download_plot_ui` + `mod_download_plot_server`) rather than duplicating download handlers.
- Use `req()` guards for reactive code that depends on non-empty filtered data.

## Module Integration Pattern

When adding a downloadable plot:

1. Define a reactive plot object (ggplot object).
2. Render it (`renderPlot` or `renderPlotly(ggplotly(...))`).
3. Add `mod_download_plot_ui("<id>", label = "Download")` in matching UI card.
4. Wire `mod_download_plot_server("<id>", filename = "<name>", figure = <reactive_plot>)` in server.

## Deployment Notes

- GitHub Actions deployment is configured in `.github/workflows/deploy-app.yaml` and targets GitHub Pages.
- `site/` contains build artifacts; treat it as generated output unless task explicitly requires editing built files.

## Safety Checks Before Finishing

- Confirm app still starts (`shiny::runApp()` without immediate errors).
- Confirm changed outputs still respect active filters (`outcome`, `diagnosis`, `drg`, `age_range`).
- If modifying plots, verify both on-screen rendering and download behavior.
