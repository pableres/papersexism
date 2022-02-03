#Sintaxis ASI-AMI final

#package needed.

ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

# usage
packages <- c("parameters","apa","haven","ggplot2","ggpubr","gridExtra","apaTables", "reshape", "GPArotation", "mvtnorm", "psych", "psychometric", "lavaan", "nFactors", "semPlot", "lavaan", "MVN", "semTools")
ipak(packages)

#citar paketes
citation(package = "parameters")
citation(package = "apa")
citation(package = "haven")
citation(package = "ggplot2")
citation(package = "ggpubr")
citation(package= "gridExtra")
citation(package= "apaTables")
citation(package= "reshape")
citation(package= "GPArotation")
citation (package= "mvtnorm")
citation (package = "psych")
citation (package = "psychometric")
citation (package = "lavaan")
citation (package= "nFactors")
citation (package = "semPlot")
citation (package = "MVN")
citation (package= "semTools")

ipak(packages)

#load dataframe
DAFE2 <- read_sav("DAFE2.sav")
DFCFA <- read_sav("DAFC2.sav")
ASIAMI <- read_sav("Completa2.sav")



ASI<-data.frame(cbind(DAFE2$ASI1, DAFE2$ASI2, DAFE2$ASI3 , DAFE2$ASI4, DAFE2$ASI5, DAFE2$ASI6, DAFE2$ASI7 , DAFE2$ASI8, DAFE2$ASI9, DAFE2$ASI10, DAFE2$ASI11, DAFE2$ASI12))
names(ASI)<- c( "1", "2","3","4","5", "6","7","8","9", "10","11","12")

#n factors ASI
results_nfactorASI<-n_factors(ASI, rotate = "oblimin", fm = "mle", n = NULL)
plot(results_nfactorASI)
results_nfactorASI
as.data.frame(results_nfactorASI)
summary(results_nfactorASI)

#Exploratory Factorial ANalysis ASI
ASIfactor<-fa(ASI,nfactors = 2,fm = "ml",rotate ="oblimin",cor = "poly")
print(ASIfactor,digits = 2,cut = .00,sort=TRUE)

#COnfirmatory Factor Analysis ASI

ASIconf<-data.frame(cbind(DFCFA$ASI1, DFCFA$ASI2, DFCFA$ASI3 , DFCFA$ASI4, DFCFA$ASI5, DFCFA$ASI6, DFCFA$ASI7 , DFCFA$ASI8, DFCFA$ASI9, DFCFA$ASI10, DFCFA$ASI11, DFCFA$ASI12))
ASIconf

Onefactor<-'Sexs =~ X1 + X2 + X3 + X4 + X5 + X6 + X7 + X8 + X9 + X10 + X11 + X12
'
Twofactor<-'Host =~ X3 + X6 + X7 + X8 + X9 + X11 
Benev =~ X1 + X2 + X4 + X5 + X10 + X12
'
CFAone <- cfa(Onefactor,orthogonal=TRUE, data=ASIconf, estimator="WLSMV",ordered =names(ASIconf))
summary(CFAone, fit.measures=TRUE)

CFAtwoindep <- cfa(Twofactor,orthogonal=TRUE, data=ASIconf, estimator="WLSMV",ordered =names(ASIconf))
summary(CFAtwoindep, fit.measures=TRUE)

CFAtworele <- cfa(Twofactor,orthogonal=FALSE, data=ASIconf, estimator="WLSMV",ordered =names(ASIconf))
summary(CFAtworele, fit.measures=TRUE)
semPaths(CFAtworele, intercepts = FALSE,edge.label.cex=1.5, optimizeLatRes = TRUE, groups = "lat",pastel = TRUE, exoVar = FALSE, sizeInt=5,edge.color ="black",esize = 6, label.prop=2,sizeLat = 6,"std", layout="circle2")

#Some item properties ASI

Host<-data.frame(cbind(ASIAMI$ASI3, ASIAMI$ASI6, ASIAMI$ASI7 , ASIAMI$ASI8 , ASIAMI$ASI9 , ASIAMI$ASI11))
Host_tetra<-polychoric(Host)
psych::alpha(Host_tetra$rho)
describe(Host)

Benev<-data.frame(cbind(ASIAMI$ASI1, ASIAMI$ASI2, ASIAMI$ASI4 , ASIAMI$ASI5 , ASIAMI$ASI12 , ASIAMI$ASI10))
Benev_tetra<-polychoric(Benev)
psych::alpha(Benev_tetra$rho)
describe(Benev)

#data frame AMI
AMI<-data.frame(cbind(DAFE2$AMI1, DAFE2$AMI2, DAFE2$AMI3 , DAFE2$AMI4, DAFE2$AMI5, DAFE2$AMI6, DAFE2$AMI7 , DAFE2$AMI8, DAFE2$AMI9, DAFE2$AMI10, DAFE2$AMI11, DAFE2$AMI12))
names(AMI)<- c( "1", "2","3","4","5", "6","7","8","9","10","11","12")

#n factors AMI
results_nfactorAMI<-n_factors(AMI, rotate = "oblimin", fm = "mle", n = NULL)
plot(results_nfactorAMI)
results_nfactorAMI
as.data.frame(results_nfactorAMI)
summary(results_nfactorAMI)

#Exploratory Factorial ANalysis AMI
AMIdos<-fa(AMI,nfactors = 2,fm = "ml",rotate ="oblimin",cor = "poly")
print(AMIdos,digits = 2,cut = .00,sort=TRUE)


#confirmatorio AMI
AMIconf<-data.frame(cbind(DFCFA$AMI1, DFCFA$AMI2, DFCFA$AMI3 , DFCFA$AMI4, DFCFA$AMI5, DFCFA$AMI6, DFCFA$AMI7 , DFCFA$AMI8, DFCFA$AMI9, DFCFA$AMI10, DFCFA$AMI11, DFCFA$AMI12))
AMIconf

Onefactorami<-'Sexs =~ X1 + X2 + X3 + X4 + X5 + X6 + X7 + X8 + X9 + X10 + X11 + X12
'
Twofactorami<-'Host =~ X7 + X2 + X10 + X4 + X5 + X12 
Benev =~ X1 + X8 + X3 + X6 + X11 + X9
'

Twofactoramicov<-'Host =~ X1 + X2 + X3 + X5 + X6 
Benev =~ X8 + X9 + X10 + X11 + X12
X11 ~~ X12
X1 ~~ X5
'
CFAoneami <- cfa(Onefactorami,orthogonal=TRUE, data=AMIconf, estimator="WLSMV",ordered =names(AMIconf))
summary(CFAoneami, fit.measures=TRUE)

CFAtwoindepami <- cfa(Twofactorami,orthogonal=TRUE, data=AMIconf, estimator="WLSMV",ordered =names(AMIconf))
summary(CFAtwoindepami, fit.measures=TRUE)

CFAtworeleami <- cfa(Twofactorami,orthogonal=FALSE, data=AMIconf, estimator="WLSMV",ordered =names(AMIconf))
summary(CFAtworeleami, fit.measures=TRUE)
semPaths(CFAtworeleami, intercepts = FALSE,edge.label.cex=1.5, optimizeLatRes = TRUE, groups = "lat",pastel = TRUE, exoVar = FALSE, sizeInt=5,edge.color ="black",esize = 6, label.prop=2,sizeLat = 6,"std", layout="circle")

modindices(CFAtworeleami)



#propiedades del AMI
#Some item properties AMI

Hostami<-data.frame(cbind(ASIAMI$AMI7, ASIAMI$AMI2, ASIAMI$AMI4, ASIAMI$AMI5 , ASIAMI$AMI10 , ASIAMI$AMI12))
names(Hostami)<- c("7","2","4","5","10","12")
Host_tetra_ami<-polychoric(Hostami)
psych::alpha(Host_tetra_ami$rho)
describe(Hostami)

Benevami<-data.frame(cbind(ASIAMI$AMI1,ASIAMI$AMI8, ASIAMI$AMI9 , ASIAMI$AMI6 , ASIAMI$AMI11 , ASIAMI$AMI3))
names(Benevami)<- c("1","8", "9","6","11","3")
Benev_tetra_ami<-polychoric(Benevami)
psych::alpha(Benev_tetra_ami$rho)
describe(Benevami)

#Criterium validity
ASI_host<-(ASIAMI$ASI3+ASIAMI$ASI6+ASIAMI$ASI7+ASIAMI$ASI8+ASIAMI$ASI9+ASIAMI$ASI11)
ASI_Benev<-(ASIAMI$ASI1+ASIAMI$ASI2+ASIAMI$ASI4+ASIAMI$ASI5+ASIAMI$ASI10+ASIAMI$ASI12)
AMI_host<-(ASIAMI$AMI2+ASIAMI$AMI4+ASIAMI$AMI5+ASIAMI$AMI7+ASIAMI$AMI10+ASIAMI$AMI12)
AMI_Benev<-(ASIAMI$AMI1+ASIAMI$AMI3+ASIAMI$AMI6+ASIAMI$AMI8+ASIAMI$AMI9+ASIAMI$AMI11)
Dfcor<-data.frame(cbind(ASI_host , ASI_Benev , AMI_host , AMI_Benev, ASIAMI$IGtotal))
names(Dfcor)<- c( "ASIhos", "ASIben","AMIhos","AMIben","IGtotal")
apa.cor.table(Dfcor, filename ="C:/Users/pable/Desktop/Validación ASI y AMI/Validacion 2/correlaciones.doc",show.conf.interval = FALSE, landscape = TRUE)

#Sex differences densiogram

#Transform agrupation ariable as a factor variable

ASIAMI$ASIh<-(ASIAMI$ASI3+ASIAMI$ASI6+ASIAMI$ASI7+ASIAMI$ASI8+ASIAMI$ASI9+ASIAMI$ASI11)/6
ASIAMI$ASIb<-(ASIAMI$ASI1+ASIAMI$ASI2+ASIAMI$ASI4+ASIAMI$ASI5+ASIAMI$ASI10+ASIAMI$ASI12)/6
ASIAMI$AMIh<-(ASIAMI$AMI2+ASIAMI$AMI4+ASIAMI$AMI5+ASIAMI$AMI7+ASIAMI$AMI10+ASIAMI$AMI12)/6
ASIAMI$AMIb<-(ASIAMI$AMI1+ASIAMI$AMI3+ASIAMI$AMI6+ASIAMI$AMI8+ASIAMI$AMI9+ASIAMI$AMI11)/6
myplot<-data.frame(cbind(ASIAMI$Sexo, ASIAMI$Escolaridad, ASIAMI$ASIh , ASIAMI$ASIb , ASIAMI$AMIh , ASIAMI$AMIb))
names(myplot)<- c( "Sex", "Studies","ASIh","ASIb","AMIh" , "AMIb")


myplot$Sex <- as.factor(myplot$Sex)
myplot$Studies <- as.factor(myplot$Studies)

#it gives levels to Grouping variable
levels(myplot$Sex) <- c("Male", "Female")
levels(myplot$Studies)<- c("0 years", "5 years", "11 years","12 years","13 years","14 years","16 years","17 years","18 years")
myplot<- data.frame(myplot[complete.cases(myplot), ])
myplot
#Figura 1
#density plot

ASI1<-ggstatsplot::ggbetweenstats(
  data = myplot,
  x = Sex,
  y = ASIh,
  notch = TRUE, # show notched box plot
  mean.ci = TRUE, # whether to display confidence interval for means
  effsize.type = "d",
  k = 2, # number of decimal places for statistical results
  outlier.tagging = FALSE, # whether outliers need to be tagged
  outlier.label = , # variable to be used for the outlier tag
  xlab = "Sex", # label for the x-axis variable
  ylab = "Hostile Sexism", # label for the y-axis variable
  title = "Gender differences in hostile sexism (ASI)", # title text for the plot
  ggtheme = ggthemes::theme_tufte(), # choosing a different theme
  ggstatsplot.layer = FALSE, # turn off `ggstatsplot` theme layer
  package = "wesanderson", # package from which color palette is to be taken
  palette = "Darjeeling1", # choosing a different color palette
  messages = FALSE
)

ASI2<-ggstatsplot::ggbetweenstats(
  data = myplot,
  x = Sex,
  y = ASIb,
  notch = TRUE, # show notched box plot
  mean.ci = TRUE, # whether to display confidence interval for means
  effsize.type = "d",
  k = 2, # number of decimal places for statistical results
  outlier.tagging = FALSE, # whether outliers need to be tagged
  outlier.label = , # variable to be used for the outlier tag
  xlab = "Sex", # label for the x-axis variable
  ylab = "Benevolent Sexism", # label for the y-axis variable
  title = "Gender differences in benevolent sexism (ASI)", # title text for the plot
  ggtheme = ggthemes::theme_tufte(), # choosing a different theme
  ggstatsplot.layer = FALSE, # turn off `ggstatsplot` theme layer
  package = "wesanderson", # package from which color palette is to be taken
  palette = "Darjeeling1", # choosing a different color palette
  messages = FALSE
)

ASI3<-ggstatsplot::ggbetweenstats(
  data = myplot,
  x = Studies,
  y = ASIh,
  notch = TRUE, # show notched box plot
  mean.ci = TRUE, # whether to display confidence interval for means
  pairwise.comparisons = FALSE, # display results from pairwise comparisons
  pairwise.display = "significant", # display only significant pairwise comparisons
  p.adjust.method = "fdr", # adjust p-values for multiple tests using this method
  k = 2, # number of decimal places for statistical results
  outlier.tagging = FALSE, # whether outliers need to be tagged
  outlier.label = , # variable to be used for the outlier tag
  xlab = "Schooling", # label for the x-axis variable
  ylab = "Hostile Sexism", # label for the y-axis variable
  title = "Schooling differences in hostile sexism (ASI)", # title text for the plot
  ggtheme = ggthemes::theme_tufte(), # choosing a different theme
  ggstatsplot.layer = FALSE
)


ASI4<-ggstatsplot::ggbetweenstats(
  data = myplot,
  x = Studies,
  y = ASIb,
  notch = TRUE, # show notched box plot
  mean.ci = TRUE, # whether to display confidence interval for means
  pairwise.comparisons = FALSE, # display results from pairwise comparisons
  pairwise.display = "significant", # display only significant pairwise comparisons
  p.adjust.method = "fdr", # adjust p-values for multiple tests using this method
  k = 2, # number of decimal places for statistical results
  outlier.tagging = FALSE, # whether outliers need to be tagged
  outlier.label = , # variable to be used for the outlier tag
  xlab = "Schooling", # label for the x-axis variable
  ylab = "Benevolent Sexism", # label for the y-axis variable
  title = "Schooling differences in benevolent sexism (ASI)", # title text for the plot
  ggtheme = ggthemes::theme_tufte(), # choosing a different theme
  ggstatsplot.layer = FALSE, # turn off `ggstatsplot` theme layer
  messages = FALSE
)


#imagenes AMI

AMI1<-ggstatsplot::ggbetweenstats(
  data = myplot,
  x = Sex,
  y = AMIh,
  notch = TRUE, # show notched box plot
  mean.ci = TRUE, # whether to display confidence interval for means
  effsize.type = "d",
  k = 2, # number of decimal places for statistical results
  outlier.tagging = FALSE, # whether outliers need to be tagged
  outlier.label = , # variable to be used for the outlier tag
  xlab = "Sex", # label for the x-axis variable
  ylab = "Hostile Sexism", # label for the y-axis variable
  title = "Gender differences in hostile sexism (AMI)", # title text for the plot
  ggtheme = ggthemes::theme_tufte(), # choosing a different theme
  ggstatsplot.layer = FALSE, # turn off `ggstatsplot` theme layer
  package = "wesanderson", # package from which color palette is to be taken
  palette = "Darjeeling1", # choosing a different color palette
  messages = FALSE
)

AMI2<-ggstatsplot::ggbetweenstats(
  data = myplot,
  x = Sex,
  y = AMIb,
  notch = TRUE, # show notched box plot
  mean.ci = TRUE, # whether to display confidence interval for means
  effsize.type = "d",
  k = 2, # number of decimal places for statistical results
  outlier.tagging = FALSE, # whether outliers need to be tagged
  outlier.label = , # variable to be used for the outlier tag
  xlab = "Sex", # label for the x-axis variable
  ylab = "Benevolent Sexism", # label for the y-axis variable
  title = "Gender differences in benevolent sexism (AMI)", # title text for the plot
  ggtheme = ggthemes::theme_tufte(), # choosing a different theme
  ggstatsplot.layer = FALSE, # turn off `ggstatsplot` theme layer
  package = "wesanderson", # package from which color palette is to be taken
  palette = "Darjeeling1", # choosing a different color palette
  messages = FALSE
)

AMI3<-ggstatsplot::ggbetweenstats(
  data = myplot,
  x = Studies,
  y = AMIh,
  notch = TRUE, # show notched box plot
  mean.ci = TRUE, # whether to display confidence interval for means
  pairwise.comparisons = FALSE, # display results from pairwise comparisons
  pairwise.display = "significant", # display only significant pairwise comparisons
  p.adjust.method = "fdr", # adjust p-values for multiple tests using this method
  k = 2, # number of decimal places for statistical results
  outlier.tagging = FALSE, # whether outliers need to be tagged
  outlier.label = , # variable to be used for the outlier tag
  xlab = "Schooling", # label for the x-axis variable
  ylab = "Hostile Sexism", # label for the y-axis variable
  title = "Schooling differences in hostile sexism (AMI)", # title text for the plot
  ggtheme = ggthemes::theme_tufte(), # choosing a different theme
  ggstatsplot.layer = FALSE
)


AMI4<-ggstatsplot::ggbetweenstats(
  data = myplot,
  x = Studies,
  y = AMIb,
  notch = TRUE, # show notched box plot
  mean.ci = TRUE, # whether to display confidence interval for means
  pairwise.comparisons = FALSE, # display results from pairwise comparisons
  pairwise.display = "significant", # display only significant pairwise comparisons
  p.adjust.method = "fdr", # adjust p-values for multiple tests using this method
  k = 2, # number of decimal places for statistical results
  outlier.tagging = FALSE, # whether outliers need to be tagged
  outlier.label = , # variable to be used for the outlier tag
  xlab = "Schooling", # label for the x-axis variable
  ylab = "Benevolent Sexism", # label for the y-axis variable
  title = "Schooling differences in benevolent sexism (AMI)", # title text for the plot
  ggtheme = ggthemes::theme_tufte(), # choosing a different theme
  ggstatsplot.layer = FALSE, # turn off `ggstatsplot` theme layer
  messages = FALSE
)

#tres opciones de unificar los plots

ggpubr::ggarrange(ASI1,ASI2,AMI1,AMI2, labels = c("A", "B", "C" ,"D"))
ggpubr::ggarrange(ASI3,ASI4,AMI3,AMI4, labels = c("A", "B", "C" ,"D"))

combine_plots(
  ASI1,
  ASI2,
  ASI3,
  ASI4,
  labels = c("(a)", "(b)"),
  title.text = "Dataset: Iris Flower dataset",
  caption.text = "",
  title.color = "black",
  caption.color = "blue"
)


