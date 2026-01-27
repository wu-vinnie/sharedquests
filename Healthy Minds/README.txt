About each folder and files in them

Data:
1. Raw data files go in "data," which is referenced in script/cleaning.R.
2. After cleaning, the clean data file, which I called "Fullerton_clean_VW.xlsx" is referenced in the .qmd files.

Figure:
These are the figures I reference for the templates to make things pretty and on brand.

Info:
Just the codebook. Ignore.

Script:
1. First run "cleaning.R."
2. All .qmd files reference a template file: "../template/fullerton.R". This is a super important file for libraries, 
functions, formatting, etc.
3. Each .qmd is its own site. Report.qmd is the primary page, which links to demographics.html and addloutcomes.html. 

Template [IMPORTANT]:
1. Fullerton.css: makes things pretty for formatting
2. Fullerton.html: same but sets the defaults for each webpage.
3. Fullerton.R: most important. It has assignments and functions that are referenced in the other pages.

Main folder:
I don't manually run each of the files or transfer them over. I run the "render_and_copy.R" script, which
runs each .qmd I have in these folders, then copies each of the generated .html files into a different folder,
which is the "public" one that I push into GitHub Pages.


