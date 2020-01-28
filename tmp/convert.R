# Load libraries
library(fs)

# read data to convert
f <- dir_ls(".", recurse = FALSE, regex = "ANA$")

iconv -f "UTF-16LE" -t "UTF-8" data/St19_3-ch1_BRA20fem.ANA -o data/St19_3-ch1_BRA20fem_converted.ANA 

for (i in seq_along(f)){
    system(paste0('iconv -f "UTF-16LE" -t "UTF-8" ', f[i], ' -o ', gsub("ANA", "converted.ANA", f[i])))
}
