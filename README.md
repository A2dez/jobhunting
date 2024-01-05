# jobhunting

To run:
Change search terms in config.R and thenrun:

```
cd jobhunting
Rscript scrape_linkedjobs.R
```


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
 