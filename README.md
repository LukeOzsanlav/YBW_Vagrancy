# Understanding Vagrancy in Yellow-browed Warblers *Phylloscopus inornatus*

Analysis to examine origins of vagrant Yellow-browed Warblers in western Europe using hydorgen stable isotope data. All code and data required to complete the analysis will be hosted on this repository.

## *Authors*
- Luke Ozsanlav-Harris
- Liam Langley

## Models
Current models run in `Chiff Explore and prelim analysis.R` and to be included in MS
```{r]
H2 ~ species grouping
H2 ~ wing length + Capture day + condition
Capture day ~ wing length + condition
```

## Repo Structure
- `Data`: Data from Jake and Robbie, only 2 files used both read into my code at the start. 
- `Code`: Code to run my analysis and also the code used by Jake and Robbie. My code is called `Chiff Explore and prelim analysis.R`
- `Outputs`: Prelim plots that will go in first draft of MS
