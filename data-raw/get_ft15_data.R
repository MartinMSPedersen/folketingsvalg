fintal <- "http://www.dst.dk/valg/Valg1664255/xml/fintal.xml"

f <- readLines(fintal)

files_to_get <- vector('character',0)

for (idx in seq_along(f)) {
  g <- regexpr("http[^\"]*xml", f[[idx]])
  if (g[1] == -1) next
  start_idx <- g[[1]]
  len <- attr(g, "match.length")

  fname <- substr(f[[idx]], start_idx, start_idx + len - 1)
  files_to_get <- append(files_to_get,fname)
}

files_to_get <- grep("fintal",files_to_get, value = TRUE)

for (idx in seq_along(files_to_get)) {
  afile <- files_to_get[[idx]]
  destfile <- sub("^.*/","", afile)
  download.file(afile, destfile)
}
