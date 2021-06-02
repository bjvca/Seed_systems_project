#This R program prepares the reports that will be distributed to the agro-input dealers as part of the feedback from farmer ratings
#the program loads that data and essentially writes latex markup into a file in /output called "output_AD_xxx.tex". These texfiles are then compiled 
# by running these commands in bash/terminal:

#cd output; for i in *.tex; do pdflatex $i;done
#rm *.aux
#rm *.log
#rm *.tex

rm(list=ls())

path <- getwd()

path <- strsplit(path, "/Study design/treatments/info_clearing/agro_input")[[1]]


dta <- read.csv(paste(path,"baseline/data/agro_input/public/reviews_seed.csv", sep="/"), stringsAsFactors = FALSE)
### here we can merge in data
dta_identifier <- read.csv(paste(path,"baseline/data/agro_input/raw/ODK_imp.csv", sep="/"), stringsAsFactors = FALSE)
dta <- merge(dta, dta_identifier[c("shop_ID","maize.owner.agree.biz_name")], by="shop_ID")

for (i in 1:dim(dta)[1]) {

sink(paste(paste("output/output",dta$shop_ID[i],sep="_"), "tex",sep="."))
cat("\\batchmode")
cat("\n")
cat("\\documentclass[english]{article}\\usepackage[]{graphicx}\\usepackage[]{color}")
cat("\n")

cat("\\usepackage{tikz}")
cat("\n")
cat("\\usetikzlibrary{shapes.geometric}")
cat("\n")
cat("\\newcommand{\\Stars}[2][fill=yellow,draw=orange]{\\begin{tikzpicture}[baseline=-0.35em,#1]")
cat("\n")
cat("\\foreach \\X in {1,...,5}")
cat("\n")
cat("{\\pgfmathsetmacro{\\xfill}{min(1,max(1+#2-\\X,0))}")
cat("\n")
cat("\\path (\\X*1.1em,0)")
cat("\n")
cat("node[star,draw,star point height=0.25em,minimum size=1em,inner sep=0pt,")
cat("\n")
cat("path picture={\\fill (path picture bounding box.south west)")
cat("\n") 
cat("rectangle  ([xshift=\\xfill*1em]path picture bounding box.north west);}]{};")
cat("\n")
cat("}")
cat("\n")
cat("\\end{tikzpicture}}")
cat("\n")

cat("\\makeatother")
cat("\n")

cat("\\begin{document}")
cat("\n")

cat("\\begin{center}")
cat("\n")

cat(paste(paste("Congratulations", dta$maize.owner.agree.biz_name[i], sep=" "),"! You score:", sep=""))
cat("\n")
cat(paste(paste("\\center{\\Huge",format(round(dta$score_corrected[i],digits=1),nsmall=1),sep="{"),"}}",sep=""))
cat("\n")
cat("out of 5")
cat("\n")


cat("\\par\\end{center}")
cat("\n")

cat(paste(paste("\\center{\\Huge{\\Stars",dta$score_corrected[i],sep="{"),"}}}",sep=""))
cat("\n")


cat(paste(paste("\\center{This was based on", dta$nr_reviews[i], sep=" "),"reviews.}", sep=" "))
cat("\n")
cat("\\center{This score means that the average farmer is of the opinion that}")
cat("\\center{the quality of the seed you sell is:}") 
cat("\n")
cat(paste(paste("\\center{\\Huge",ifelse(dta$score_corrected[i]>3.649582,"Excellent",ifelse(dta$score_corrected[i]>3.468970,"Very Good",ifelse(dta$score_corrected[i]>3.287588,"Good",ifelse(dta$score_corrected[i]>3.116589 ,"OK","Poor")))),sep="{"),"}}",sep=""))
cat("\\newpage{}")


cat("How do you compare to others in your area?") 
cat("\n")

cat("\\begin{tabular}{|c|c|c|}") 
cat("\n")
cat("\\hline")
cat("\n") 
cat(" & you & average") 
cat("\n")
cat("\\tabularnewline") 
cat("\n")
cat("\\hline")
cat("\n")  
cat("\\hline")
cat("\n")  
cat(paste(paste(paste(paste("general & \\Huge{\\Stars", dta$general_corrected[i],sep = "{"),"}}  & \\Huge{\\Stars", sep =""), dta$general_corrected_av[i],sep = "{"),"}}", sep=""))
cat("\n")  

cat("\\tabularnewline")
cat("\n")  

cat("\\hline")
cat("\n")  
cat(paste(paste(paste(paste("quality & \\Huge{\\Stars", dta$quality_rating_corrected[i],sep = "{"),"}}  & \\Huge{\\Stars", sep =""), dta$quality_rating_corrected_av[i],sep = "{"),"}}", sep=""))
cat("\n")  

cat("\\tabularnewline")
cat("\n")  

cat("\\hline")
cat("\n")  

cat(paste(paste(paste(paste("yield & \\Huge{\\Stars", dta$yield_corrected[i],sep = "{"),"}}  & \\Huge{\\Stars", sep =""), dta$yield_corrected_av[i],sep = "{"),"}}", sep=""))
cat("\n")  

cat("\\tabularnewline")
cat("\n")  

cat("\\hline")
cat("\n") 

cat(paste(paste(paste(paste("drought resistant & \\Huge{\\Stars", dta$drought_resistent_corrected[i],sep = "{"),"}}  & \\Huge{\\Stars", sep =""), dta$drought_resistent_corrected_av[i],sep = "{"),"}}", sep=""))
cat("\n")  

cat("\\tabularnewline")
cat("\n")  

cat("\\hline")
cat("\n")   

cat(paste(paste(paste(paste("disease resistant & \\Huge{\\Stars", dta$disease_resistent_corrected[i],sep = "{"),"}}  & \\Huge{\\Stars", sep =""), dta$disease_resistent_corrected_av[i],sep = "{"),"}}", sep=""))
cat("\n")  

cat("\\tabularnewline")
cat("\n")  

cat("\\hline")
cat("\n")   

cat(paste(paste(paste(paste("early maturing & \\Huge{\\Stars", dta$early_maturing_corrected[i],sep = "{"),"}}  & \\Huge{\\Stars", sep =""), dta$early_maturing_corrected_av[i],sep = "{"),"}}", sep=""))
cat("\n")  

cat("\\tabularnewline")
cat("\n")  

cat("\\hline")
cat("\n")   

cat(paste(paste(paste(paste("germination & \\Huge{\\Stars", dta$germination_corrected[i],sep = "{"),"}}  & \\Huge{\\Stars", sep =""), dta$germination_corrected_av[i],sep = "{"),"}}", sep=""))
cat("\n")  

cat("\\tabularnewline")
cat("\n")  

cat("\\hline")
cat("\n")  

cat("\\hline") 
cat("\\end{tabular}") 

cat("\\end{document}")
cat("\n")
sink()
}








