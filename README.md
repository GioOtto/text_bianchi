# Text Mining and Sentiment Analysis

Course assignment (Text Mining and Sentiment Analysis – 17711-ENG).
Authors: Bombardieri, Ottoboni, Zubani.

Sentiment analysis of Amazon UK reviews for *Atomic Habits* by James Clear,
comparing five approaches against star-rating ground truth:

- Bing lexicon (tidy), with and without stopwords removal
- UDPipe, baseline (UDbing) and extended with negators/amplifiers (UDpipe)
- Naive Bayes classifier trained on the star labels

## Repository structure

```
Script_updated.R     Full R script: scraping (RSelenium/rvest) + analysis
data.rds             Cached scraped reviews (input to the analysis)
plots/               The six figures used in the report (fig1a … fig4)
report_finale/       Final report
  report.tex         LaTeX source of the report
  figs/              Figures referenced by report.tex
Assignment.md        Original assignment brief
```

## Reproducing the analysis

The script requires R with: `tidyverse`, `rvest`, `RSelenium`, `netstat`,
`cld2`, `tidytext`, `wordcloud`, `caret`, `udpipe`, `quanteda`,
`quanteda.textmodels`.

The scraping/login section needs Amazon credentials and a Selenium browser;
to reproduce only the analysis, run the script from the cached `data.rds`
(the `readRDS` line onward).

The UDPipe English GUM model (`english-gum-ud-2.5-191206.udpipe`) is not
committed (see `.gitignore`); `udpipe('english-gum')` downloads it on first use.

## Building the report

`report_finale/report.tex` compiles with the figures in `report_finale/figs/`
(or upload the `.tex` + `figs/` to Overleaf).
