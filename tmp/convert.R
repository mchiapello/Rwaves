# Load libraries
library(fs)

# Set wd to folder with files to convert
setwd("../inst/extdata")

# read data to convert
f <- fs::dir_ls(".", recurse = FALSE, regex = "ANA$")

# Conver the data
for (i in seq_along(f)){
    system(paste0('iconv -f "UTF-16LE" -t "UTF-8" ', f[i], ' -o ', gsub("ANA", "converted.ANA", f[i])))
}

# Change comma to dot
f <- fs::dir_ls(".", recurse = FALSE, regex = "converted.ANA$")
for (i in seq_along(f)){
    system(paste0("sed -i 's/,/./g' ", f[i]))
}

