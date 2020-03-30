# Load libraries
library(fs)

# Set wd to folder with files to convert
setwd("~/Downloads/daconvertire/")

# read data to convert
f <- fs::dir_ls(".", recurse = FALSE, regex = "ANA$")

# Conver the data
for (i in seq_along(f)){
    system(paste0('iconv -f "UTF-16LE" -t "UTF-8" ', f[i], ' > ', gsub("ANA", "con.ANA", f[i])))
}

# Change comma to dot
f <- fs::dir_ls(".", recurse = FALSE, regex = "con.ANA$")
for (i in seq_along(f)){
    system(paste0("sed 's/,/./g' ", f[i], " > ", gsub("con.ANA", "converted.ANA", f[i])))
}

