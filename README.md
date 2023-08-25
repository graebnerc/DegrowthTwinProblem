# Degrowth and the global south: The twin problem of global dependencies
**[Claudius Gräbner-Radkowitsch](https://claudius-graebner.com/)**
**and**
**[Birte Strunk](https://birtestrunk.github.io/)**

In this repository we provide all data and code that is required to 
replicate the figures and results for the following paper:

> Gräbner-Radkowitsch, C. and Strunk, B. (2023): 
Degrowth and the Global South: The twin problem of global dependencies,
*Ecological Economics*, Vol. 213. DOI: [10.1016/j.ecolecon.2023.107946](https://doi.org/10.1016/j.ecolecon.2023.107946) (open access).

We also provide spreadsheet versions of the paper overview tables from the 
paper:

- [Appendix B: List of papers in the core sample](output/Appendix-CorePapers.xlsx)
- [Appendix C: List of papers in the relevant sample](output/Appendix-RelevantPapers.xlsx)
- [Appendix D: Distribution of the sub-arguments made in the core sample](data/manual/Argument-Distribution.xlsx)

## Functioning of the code

- `setup_WoSraw_data.R`: Reads in the raw data sets as 
downloaded directly from the Web of Science, merges them into one coherent 
data set, removes duplicates, and saves the result as `WoS-RawSampleUnique.csv`. 
It also creates the template sheets that we used for our qualitative 
assessment (`Qual-review.xlsx`).

- `setup_data.R`: Takes the WoS sample, merges it with the other data
sources (informal sample, degrowth sample, reference sample) as well as our
own qualitative assessment, and produces the core and relevant sample. The
paper selection process gets summarized via the variables in `sample_summary.Rdata`.

- `setup_cited_refs.R`: This conducts the analysis of the reference list of
the papers from the WoS sample to come up with the reference sample used in
the main paper. It saves the resulting sample as 

- `build_cited_refs.R`: This takes the previously created file 
`cited_articles_full.Rds` and merges it with tue qualitative assessment
data to come up with the final reference sample (i.e. the articles with
sufficient citations).

- `screening_figure.R`: Creates all the numbers used in Figure 1 of the main 
paper. To this end, it reads the files `sample_summary.Rdata` 
(created by `setup_rev_data.R`)
and `sample_summary_WoSraw.Rdata` (created by `setup_rev_data.R`).

- `Figures-Descriptives.R`: Creates most of the descriptive figures used
in the main paper. When executed, it also calls `setup_data.R` automatically
to create the core and relevant sample. It also uses the sheet 
`Argument-Distribution.xlsx`, which has been prepared manually.
The functions used in this script are defined separately in 
`analysis_functions.R`.

- `Figures-MethodologyNarratives.R`: Creates the figures on which narratives the 
core papers follow, and which methodologies they use. It calls `setup_data.R`, 
uses the functions defined in `analysis_functions.R` and 
creates the corresponding plots.

- `Appendix-SampleLists.R`: Creates the list of papers reported in the appendix.
To this end, it calls `setup_data.R` to define the core and relevant sample,
and writes `Appendix-RelevantPapers.xlsx` and `Appendix-CorePapers.xlsx`. 

- `FullText-PostDevelopment-Search.py`: A python script searching the paper
full texts for references 

- `PostDevelopment-Links.R`: Analyzes paper titles and abstracts regarding their
mentioning of relevant post development keywords. Uses the file 
`PostDevelopmentSearch.csv`, which has been produced by 
`python/FullText-PostDevelopment-Search.py`. Note that the latter would require
the PDF files of all core papers, which we cannot provide here for copyright
issues.

Regarding the data, the raw data is contained in `data/raw` and it is the
unmodified data downloaded from the Web of Science. 
The data in `data/manual` has been prepared manually during the
qualitative assessment of the papers. All files in `data/tidy` were prepared
by the scripts from `R/`.

## Contact
If you have comments or requests, please do not hesitate to reach out to
[Birte](mailto:birte@strunk-mg.de) or
[Claudius](https://claudius-graebner.com/contact-1.html).
