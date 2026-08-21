# Optiver 3

Project:
- Optiver_Analysis.Rmd includes code of our models including pre-processing, feature engineering, model construction and evaluation.
- Optiver_Analysis.html is the output html of Optiver_Analysis.Rmd.

Report: 
- Under Report folder including,
- ReportOptiver3.Rmd which is the code of generating our report.
- ReportOptiver3.pdf which is the output pdf of ReportOptiver3.Rmd.
- main.bib which is the bibliography of our report.
- figures folder which includes our useful figures in the report.
  - NOTE: All figures except rolling_window.png are reproducible as it is the schematic for rolling window.

Shiny App:
- Under ShinyApp folder including,
- app.r which is the code of app.
- rds objects storing the data the app needs.

RData:
- includes all data needed in the report and 
- an additional stock_file.rds which stores all stocks' dataframe.
  - NOTE: We won't include original datasets in our submission.
  - The dataframes stored in stock_file.rds are exactly the same as the original ones on Canvas.