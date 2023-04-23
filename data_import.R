library(tidyverse)

read_csv("a,b,c
         1,2,3
         4,5,6")

read_csv("One line of metadata
         Another line of metadata
         x,y,z
         1,2,3", skip = 2)

read_csv("# A comment
         x,y,z
         1,2,3", comment = "#")

read_csv("1,2,3\n4,5,6", col_names = FALSE)

