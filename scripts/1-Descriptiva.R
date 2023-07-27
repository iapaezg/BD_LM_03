rm(list=ls())
if(!require(pacman)) install.packages("pacman") ; require(pacman)
require(pacman)
p_load(rio, # import/export data
       tidyverse, # tidy-data
       skimr, # summary data
       caret) # Classification And REgression Training

#Se cargan los datos disponibles en dropbox (dl=0 a dl=1)
tr_p <- read.csv("https://www.dropbox.com/scl/fi/vwfhvj05zbjh88ym0ywrf/train_personas.csv?dl=1&rlkey=zl6jxjvbzji2aqeaxsuqhrc2i")
tr_h <- read.csv("https://www.dropbox.com/scl/fi/mujk9xw7rerfg8efq22b5/train_hogares.csv?rlkey=2lp8la11mvsfz3jufn9fe21jk&dl=1")
skim(tr_p)
