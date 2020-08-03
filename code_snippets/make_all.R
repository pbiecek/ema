#rmarkdown::render_site(output_format = 'bookdown::pdf_book', encoding = 'UTF-8')

ema_tt <- readLines("ema.tex")
ema_tt <- gsub(ema_tt, pattern = " a ", replacement = " a~")
ema_tt <- gsub(ema_tt, pattern = " an ", replacement = " an~")
ema_tt <- gsub(ema_tt, pattern = " A ", replacement = " A~")
ema_tt <- gsub(ema_tt, pattern = " An ", replacement = " An~")
indy <- which(ema_tt == "\\listoftables")
ema_tt <- ema_tt[ - (indy + (0:7))]
writeLines(ema_tt, con = "ema.tex")

system("mv _bookdown_files/ema_files/ ema_files/")

#system("pdflatex -interaction nonstopmode -halt-on-error -file-line-error ema.tex")

