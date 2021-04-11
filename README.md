# This runs things

We really don't know anything about life, the universe, and everything, do we?

But this does my stuff for me because laziness is pretty much the next best thing to knowing the answer to the question.

## It goes and gets data from a few websites on a weekly basis and turns it into a nice csv

How you _might_ ask? 

~~Magic of course.~~ Github Actions! 

Why you _might_ ask?

Weekly data for my work [here](https://github.com/mrpotatocode/COFFEE_COFFEE_COFFEE)

## Do it yourself

Scripts are [here](https://github.com/mrpotatocode/Automatic_Drip/tree/main/R). 

Actions are [here](https://github.com/mrpotatocode/Automatic_Drip/tree/main/.github/workflows).

Then bulk load:
```
library(tidyverse)
library(data.table)

### add filename with a mutate (or add your own!)
read_plus <- function(flnm) {
    read_csv(flnm) %>% 
        mutate(filename = flnm)
}

datafolder <-  paste0(here(),'/data') ###set your directory here
raw_data <-  
    list.files(path = datafolder,
      pattern = "*.csv",
      full.names = T) %>% 
    map_df(~read_plus(.))
```
