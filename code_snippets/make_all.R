#rmarkdown::render_site(output_format = 'bookdown::pdf_book', encoding = 'UTF-8')

# usun wdowy o sieroty

ema_tt <- readLines("ema.tex")
ema_tt <- gsub(ema_tt, pattern = " a ", replacement = " a~")
ema_tt <- gsub(ema_tt, pattern = " an ", replacement = " an~")
ema_tt <- gsub(ema_tt, pattern = " A ", replacement = " A~")
ema_tt <- gsub(ema_tt, pattern = " An ", replacement = " An~")
indy <- which(ema_tt == "\\listoftables")
ema_tt <- ema_tt[ - (indy + (0:7))]
writeLines(ema_tt, con = "ema.tex")

# wkopiuj grafiki
system("mv _bookdown_files/ema_files/ ema_files/")
system("cp ./tmp/2020_11_09_recznie_przerobione_strony/* ./ema_files/figure-latex/")
#system("pdflatex -interaction nonstopmode -halt-on-error -file-line-error ema.tex")
























allFiles <- list.files(".", pattern = "[0-9].*Rmd")
allContent <- lapply(allFiles, function(fl) {
  tmp <- readLines(fl)
  tmp <- c("","//// ----------------------",
           fl, "",
           tmp)
  tmp
})

allContentFlat <- unlist(allContent)

writeLines(allContentFlat, "_allFiles.combined")




# ---------------- split into files

if (FALSE) {

compact <- readLines("_allFiles.Rmd")
compact <- c(compact, "")
indexec <- c(grep("//// ----------------------", compact), length(compact))

for (i in 2:length(indexec)) {
  indexec[i-1]
  fname <- compact[indexec[i-1] + 1]
  conte <- compact[(indexec[i-1] + 3):(indexec[i]-1)]
  writeLines(conte, fname)
}



}



# get all aread calls

compact <- readLines("_allFiles.Rmd")
tmp <- grep(compact, pattern = "aread", value = TRUE)
tmp <- gsub(tmp, pattern = ".*aread..", replacement = "")
tmp <- gsub(tmp, pattern = ".\\).*", replacement = "")
tmp_final <- unique(tmp[-1])
tmp_final
sort(substr(tmp_final, 16, 50))
nchar(sort(substr(tmp_final, 16, 50)))

all_files <- list.files("~/GitHub/models/gallery/")
to_files <- paste0(substr(all_files, 1, 5), ".rda")

for (ff in seq_along(all_files)) {
  file.copy(all_files[ff], to_files[ff])
}

