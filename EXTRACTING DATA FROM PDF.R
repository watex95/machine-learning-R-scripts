install.packages("pdftools")

library(pdftools)
download.file("https://github.com/Huitziii/crispy-pdf/raw/master/71_PV.62.pdf",
              "./71_PV.62.pdf")
text <- pdf_text("./71_PV.62.pdf")


#will help you separate lines from each other:
text2 <- strsplit(text, "\n")
head(text2[[1]])

library(tm)
read <- readPDF(control = list(text = "-layout"))
#The control argument enables you to set up parameters as you would write them in the command line.
#Think of the above function as writing xpdf -layout in the shell.
#Then, you're ready to import the PDF document:
document <- Corpus(URISource("./71_PV.62.pdf"), readerControl = list(reader = read))
doc <- content(document[[1]])
head(doc)





