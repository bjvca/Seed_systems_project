#run in: Study design/treatments/info_clearing/agro_input
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


dta <- read.csv(paste(path,"midline/data/agro_input/public/reviews_seed.csv", sep="/"), stringsAsFactors = FALSE)
### here we can merge in data
dta_identifier <- read.csv(paste(path,"baseline/data/agro_input/raw/ODK_imp.csv", sep="/"), stringsAsFactors = FALSE)
dta <- merge(dta, dta_identifier[c("shop_ID","maize.owner.agree.biz_name")], by="shop_ID")


dta$rank <- rank(dta$score_corrected)
dta$rank[is.na(dta$score_corrected)] <- NA
dta$stars <- min(dta$score_corrected, na.rm=T)+ dta$rank*(max(dta$score_corrected, na.rm=T)-min(dta$score_corrected, na.rm=T))/max(dta$rank, na.rm=T)

dta$rank <- rank(dta$quality_rating_corrected)
dta$rank[is.na(dta$quality_rating_corrected)] <- NA
dta$stars_quality_rating <- min(dta$quality_rating_corrected, na.rm=T)+ dta$rank*(max(dta$quality_rating_corrected, na.rm=T)-min(dta$quality_rating_corrected, na.rm=T))/max(dta$rank, na.rm=T)

dta$rank <- rank(dta$quality_rating_corrected_av)
dta$rank[is.na(dta$quality_rating_corrected_av)] <- NA
dta$stars_quality_rating_av <- min(dta$quality_rating_corrected_av, na.rm=T)+ dta$rank*(max(dta$quality_rating_corrected_av, na.rm=T)-min(dta$quality_rating_corrected_av, na.rm=T))/max(dta$rank, na.rm=T)

dta$rank <- rank(dta$yield_corrected)
dta$rank[is.na(dta$yield_corrected)] <- NA
dta$stars_yield <- min(dta$yield_corrected, na.rm=T)+ dta$rank*(max(dta$yield_corrected, na.rm=T)-min(dta$yield_corrected, na.rm=T))/max(dta$rank, na.rm=T)
 
dta$rank <- rank(dta$yield_corrected_av)
dta$rank[is.na(dta$yield_corrected_av)] <- NA
dta$stars_yield_av <- min(dta$yield_corrected_av, na.rm=T)+ dta$rank*(max(dta$yield_corrected_av, na.rm=T)-min(dta$yield_corrected_av, na.rm=T))/max(dta$rank, na.rm=T)

dta$rank <- rank(dta$drought_resistent_corrected)
dta$rank[is.na(dta$drought_resistent_corrected)] <- NA
dta$stars_drought_resistent <- min(dta$drought_resistent_corrected, na.rm=T)+ dta$rank*(max(dta$drought_resistent_corrected, na.rm=T)-min(dta$drought_resistent_corrected, na.rm=T))/max(dta$rank, na.rm=T)
 
dta$rank <- rank(dta$drought_resistent_corrected_av)
dta$rank[is.na(dta$drought_resistent_corrected_av)] <- NA
dta$stars_drought_resistent_av <- min(dta$drought_resistent_corrected_av, na.rm=T)+ dta$rank*(max(dta$drought_resistent_corrected_av, na.rm=T)-min(dta$drought_resistent_corrected_av, na.rm=T))/max(dta$rank, na.rm=T)

dta$rank <- rank(dta$disease_resistent_corrected)
dta$rank[is.na(dta$disease_resistent_corrected)] <- NA
dta$stars_disease_resistent <- min(dta$disease_resistent_corrected, na.rm=T)+ dta$rank*(max(dta$disease_resistent_corrected, na.rm=T)-min(dta$disease_resistent_corrected, na.rm=T))/max(dta$rank, na.rm=T)
 
dta$rank <- rank(dta$disease_resistent_corrected_av)
dta$rank[is.na(dta$disease_resistent_corrected_av)] <- NA
dta$stars_disease_resistent_av <- min(dta$disease_resistent_corrected_av, na.rm=T)+ dta$rank*(max(dta$disease_resistent_corrected_av, na.rm=T)-min(dta$disease_resistent_corrected_av, na.rm=T))/max(dta$rank, na.rm=T)

dta$rank <- rank(dta$early_maturing_corrected)
dta$rank[is.na(dta$early_maturing_corrected)] <- NA
dta$stars_early_maturing <- min(dta$early_maturing_corrected, na.rm=T)+ dta$rank*(max(dta$early_maturing_corrected, na.rm=T)-min(dta$early_maturing_corrected, na.rm=T))/max(dta$rank, na.rm=T)
 
dta$rank <- rank(dta$early_maturing_corrected_av)
dta$rank[is.na(dta$early_maturing_corrected_av)] <- NA
dta$stars_early_maturing_av <- min(dta$early_maturing_corrected_av, na.rm=T)+ dta$rank*(max(dta$early_maturing_corrected_av, na.rm=T)-min(dta$early_maturing_corrected_av, na.rm=T))/max(dta$rank, na.rm=T)

dta$rank <- rank(dta$germination_corrected)
dta$rank[is.na(dta$germination_corrected)] <- NA
dta$stars_germination <- min(dta$germination_corrected, na.rm=T)+ dta$rank*(max(dta$germination_corrected, na.rm=T)-min(dta$germination_corrected, na.rm=T))/max(dta$rank, na.rm=T)
 
dta$rank <- rank(dta$germination_corrected_av)
dta$rank[is.na(dta$germination_corrected_av)] <- NA
dta$stars_germination_av <- min(dta$germination_corrected_av, na.rm=T)+ dta$rank*(max(dta$germination_corrected_av, na.rm=T)-min(dta$germination_corrected_av, na.rm=T))/max(dta$rank, na.rm=T)

for (i in 1:dim(dta)[1]) {

sink(paste(paste("output_midline/output",dta$shop_ID[i],sep="_"), "tex",sep="."))
cat("\\batchmode")
cat("\n")
cat("\\documentclass[english]{article}")
cat("\n")
cat("\\usepackage[]{graphicx}")
cat("\n")
cat("\\usepackage[]{color}")
cat("\n")
cat("\\usepackage[dvipsnames]{xcolor}")
cat("\n")
cat("\\usepackage{graphicx}")
cat("\n")
cat("\\usepackage{geometry}")
cat("\n")
cat("\\usepackage{array}")
cat("\n")
cat("\\usepackage{changepage}")
cat("\n")
cat("\\usepackage{makecell}")
cat("\n")

cat("\\usepackage{tikz}")
cat("\n")
cat("\\usetikzlibrary{shapes.geometric}")
cat("\n")
cat("\\usetikzlibrary{calc}")
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

cat("\\pagenumbering{gobble}")
cat("\n")
cat("\\pagestyle{empty}")
cat("\n")

cat("\\begin{tikzpicture}[remember picture, overlay]")
cat("\n")
  cat("\\draw[Green, line width = 4pt] ($(current page.north west) + (1in,-1in)$) rectangle ($(current page.south east) + (-1in,1in)$);")
  cat("\n")
cat("\\end{tikzpicture}")
cat("\n")


cat("\\begin{LARGE}")
cat("\n")


cat("\\begin{center}")
cat("\n")


cat("\\includegraphics[scale=0.1]{../logo.jpeg}")
cat("\n")
cat("\\par")
cat("\n")


cat(dta$maize.owner.agree.biz_name[i])
cat("\n")
cat("\\par")
cat("\n")


cat("scores")
cat("\n")
cat("\\par")
cat("\n")
cat(paste(paste("\\center{\\Huge{\\textbf",format(round(dta$stars[i],digits=1),nsmall=1),sep="{"),"}}}",sep=""))
cat("\n")
cat("\\textbf{out of 5}")
cat("\n")


cat("\\par\\end{center}")
cat("\n")

cat(paste(paste("\\center{\\Huge{\\Stars",dta$stars[i],sep="{"),"}}}",sep=""))
cat("\n")


cat(paste(paste("\\center{This was based on", dta$nr_reviews[i], sep=" "),"reviews.}", sep=" "))
cat("\n")
cat("\\center{This score means that farmers in the area think that the quality of the maize seed this shop sells is:}")
cat("\n")
cat(paste(paste("\\center{\\Huge{\\textbf",ifelse(dta$score_corrected[i]>3.468970,"Excellent!",ifelse(dta$score_corrected[i]>3.287588,"Very Good!",ifelse(dta$score_corrected[i]>3.116589,"Good!","OK!"))),sep="{"),"}}}",sep=""))
cat("\n")
cat("\\par")
cat("\n")
cat("\\bigskip{}")
cat("\n")
cat("\\begin{center}")
cat("\n")
cat("\\textit{SeedAdvisor certificate 2021}") 
cat("\n")
cat("\\end{center}")
cat("\n")
cat("\\newpage{}")
cat("\n")
cat("\\end{LARGE}")
cat("\n")
cat("\\newgeometry{left=20mm, right=20mm, top=20mm, bottom=20mm}")
cat("\n")

cat("\\begin{large}")
cat("\n")
cat("\\begin{center}")
cat("\n")
cat("\\includegraphics[scale=0.05]{../logo.jpeg}")
cat("\n")
cat("\\end{center}")
cat("\n")
cat("\\par")
cat("\n")
cat("Hi there!")
cat("\\par")
cat("\n")
cat("\\medskip{}")
cat("\n")
cat("As a business, you know that your customers' opinion is very important! Your sales and profits depend on what customers think about the quality of the products you sell.")
cat("\n") 
cat("\\par")
cat("\n")
cat("\\medskip{}")
cat("\n")
cat("We visited farmers in your area and asked them about their experience with maize seed that they bought in shops such as yours. We have used this information to compute a score for you. In this way, you get an idea of what customers think about the maize seed you sell, and how it compares to other agro-input shops in your area. This gives you the opportunity to improve your business even more!")
cat("\n") 
cat("\\par")
cat("\n")
cat("\\medskip{}")
cat("\n")
cat("The overall score indicated on the certificate combines different components. You can find separate ratings in the table below. For instance, we asked what customers think of maize seed quality in general. But we also asked customers about their experiences with respect to yield, drought and disease resistance, time to mature, and germination, keeping in mind what was advertised when they bought the seed.")
cat("\n") 
cat("\\par")
cat("\n")
cat("\\medskip{}")
cat("\n")
cat("Check your scores carefully and see where you can do better! You can also compare your own results to what customers think of the quality of maize seed sold by other agro-input dealers in your area.")
cat("\n") 
cat("\\par")
cat("\n")
cat("\\medskip{}")
cat("\n")
cat("\\medskip{}")
cat("\n")

cat("\\begin{tabular}{| >{\\raggedleft}m{9cm} |  >{\\centering}m{3cm} |  >{\\centering}m{3cm} |}") 
cat("\n")
cat("\\hline")
cat("\n") 
cat(" & you & \\makecell{other \\\\ input dealers}") 
cat("\n")
cat("\\tabularnewline") 
cat("\n")
cat("\\hline")
cat("\n")  
cat("\\hline")
cat("\n")  


cat(paste(paste(paste(paste("What do customers say about overall seed quality? & \\large{\\Stars", dta$stars_quality_rating[i],sep = "{"),"}}  & \\large{\\Stars", sep =""), dta$stars_quality_rating_av[i],sep = "{"),"}}", sep=""))
cat("\n")  

cat("\\tabularnewline")
cat("\n")  
cat("\\hline")
cat("\n") 



cat(paste(paste(paste(paste("Do customers think yield of maize seed is as advertised? & \\large{\\Stars", dta$stars_yield[i],sep = "{"),"}}  & \\large{\\Stars", sep =""), dta$stars_yield_av[i],sep = "{"),"}}", sep=""))
cat("\n")  

cat("\\tabularnewline")
cat("\n")  
cat("\\hline")
cat("\n") 

cat(paste(paste(paste(paste("Do customers think maize seed is as drought resistant as advertised? & \\large{\\Stars", dta$stars_drought_resistent[i],sep = "{"),"}}  & \\large{\\Stars", sep =""), dta$stars_drought_resistent_av[i],sep = "{"),"}}", sep=""))
cat("\n")  

cat("\\tabularnewline")
cat("\n")  
cat("\\hline")
cat("\n") 

cat(paste(paste(paste(paste("Do customers think maize seed is as disease resistant as advertised? & \\large{\\Stars", dta$stars_disease_resistent[i],sep = "{"),"}}  & \\large{\\Stars", sep =""), dta$stars_disease_resistent_av[i],sep = "{"),"}}", sep=""))
cat("\n")  

cat("\\tabularnewline")
cat("\n")  
cat("\\hline")
cat("\n") 

cat(paste(paste(paste(paste("Do customers think maize seed matures as advertised? & \\large{\\Stars", dta$stars_early_maturing[i],sep = "{"),"}}  & \\large{\\Stars", sep =""), dta$stars_early_maturing_av[i],sep = "{"),"}}", sep=""))
cat("\n")  

cat("\\tabularnewline")
cat("\n")  
cat("\\hline")
cat("\n") 

cat(paste(paste(paste(paste("What do customers think about maize seed germination? & \\large{\\Stars", dta$stars_germination[i],sep = "{"),"}}  & \\large{\\Stars", sep =""), dta$stars_germination_av[i],sep = "{"),"}}", sep=""))
cat("\n")  

cat("\\tabularnewline")
cat("\n")  

cat("\\hline")
cat("\n")  

cat("\\hline") 
cat("\n")
cat("\\end{tabular}") 
cat("\n")
cat("\\par")
cat("\n")
cat("\\bigskip{}")
cat("\n")

cat("\\textbf{Tip: Display this certificate in your shop to inform customers about your score. You will be scored again in January 2021, so keep standards up!}") 
cat("\n")
cat("\\par")
cat("\n")
cat("\\bigskip{}")
cat("\n")
cat("\\end{large}")
cat("\n")
cat("\\begin{footnotesize}")
cat("\n")
cat("SeedAdvisor is a service of Digital Natives LLC in collarboration with the International Food Policy Research Institute. Ratings are based on crowdsourced data that is aggregated to protect the privacy of the raters. The service and data is provided as is, and makes no warranties, either expressed or implied, concerning the accuracy, completeness, reliability, or suitability of the information.")
cat("\n")
cat("\\end{footnotesize}")
cat("\n")




cat("\\end{document}")
cat("\n")
sink()
}








