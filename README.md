# jobhunting

###To run:
Change search terms in config.R and then run this in your terminal:
Multiple words must separated by '%2B', as in data%2Bscience. I'll fix this with next commit. 

```
cd jobhunting
Rscript scrape_linkedjobs.R
```
Prerequisites:
You will need R: Here are the mirrors for many locations. Click on one near you and then download R
[https://cran.r-project.org/mirrors.html]

### Description

This tool searches LinkedIn for job listings, based on keywords and location (for now), and creates a csv file with the data listed on the search page:
- job title
- company
- link to main listing
- link to company
- other stuff I've added. The names in the file are terrible in this version. 

It then scrapes the data from each listing, and adds more data. For now, that's just the number of applicants, but the tool saves the scraped data in an rds file (using a list column), for later analysis. 

### More config details. 
start: 0 by default as LinkedIn is zero-indexed. Should the scraper begin at the 0th job, or at, say, the 50th? 
steps: LinkedIn lists 25 jobs per search page, so 'steps' is set to 25 by default, in the config. 
end: A multiple of steps. Set to 100 for now, so the tool will scrape 100 + 25 listings. 



#To do
- Config 
  this is currently an R list, but will be changed to a .yml file with the config package
  see https://appsilon.com/r-config/
  
- speed up:
  #insert section to get rid of unwanted urls (i.e. likely no jobs there )
  
- clean
 'allow for spaces in the config, not just "%2B"
 
- Argparse
- allow for parsed arguments too. 
 