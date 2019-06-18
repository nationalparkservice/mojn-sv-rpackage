# springvegetation
Data quality control, analysis, and visualization for the Mojave Desert Network (MOJN) spring vegetation monitoring protocol

## Overview
This package is under active development! This readme will be updated as more features are added. Check out Issues and Projects for more information on what's planned.
When it's done, this package will be a one stop shop for working with spring vegetation data, including line point intercept, tree count, and species inventory data. Although much of the code is specific to MOJN's protocol, it should provide a good structure and starting point for anyone who is collecting similar data.

## Functions in this package

### Quality control
These functions check for common errors and omissions, e.g. points missing from line point intercept data. Each one returns a data frame of potential problems, listed by park, spring, field season, and transect.

### Data wrangling and analysis
These functions reshape, summarize, and/or analyze data. They return a data frame of the processed data.

### Data visualization
These functions produce plots and tables for quick and easy data visualization. Plot functions return a ggplot object; table functions return an HTML widget. These functions are useful for producing dashboards, Shiny apps, and Markdown reports with minimal code.
