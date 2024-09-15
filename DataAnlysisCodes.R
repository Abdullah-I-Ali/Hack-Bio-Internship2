## Hackbio-Cancer-Internship

## programming in biology & Data visualization

## Authors(@slack):

#Abdullah Ibrahim Ali(@Abdullah108)  
#Mahmoud Hassanen (@Mahmoud203)  
#Sarah Shebl (@Sarah50)  
#Kareem Elsayed Saad Abdell Gawad(@Kareem\_101)  
#Nada Esmael Sleim (@Eddy27)

# The code(s) for infograph:

\#used library  
library(dplyr)  
library(ggplot2)  
library(plotly)  
library(ggsci)

\# to Download data  
AMR\_Products\<- read.delim("https://raw.githubusercontent.com/HackBio-Internship/public\_datasets/main/R/WHO\_AMR\_PRODUCTS\_DATA.tsv", sep \= "\\t", header \= TRUE)  
\#preprocessing  
AMR\_Products\[is.na(AMR\_Products)\]\<-'unknown'

write.table(AMR\_Products,"AMR\_Product.tsv")  
\#Identify Key Trends:

\#1- distribution of Product Type  
Product\_type\_summary\<-AMR\_Products%\>%  
  filter(\!duplicated(AMR\_Products$Product.name))%\>%  
  group\_by(Product.type)%\>%  
  summarise(count=n())  
    
ggplot(Product\_type\_summary, aes(x \= Product.type, y \= count, fill \=Product.type)) \+  
  geom\_bar(stat \= 'identity',width \= 0.3,position \= 'dodge') \+  
  scale\_fill\_manual(values \= c("\#4F81BD", "\#C0504D"))  \+  
  theme\_minimal()+xlab('')+  
  theme(text \= element\_text(size \= 9),  
        plot.title \= element\_text(hjust \=0.5,face \= 'bold' ))

\#

\#-----------------  
\#Create the  bar chart..  
\#..that shows distribution of Product Type with activity status  
product\_activity \<- AMR\_Products %\>%  
  filter(Active.against.priority.pathogens. \!='N/A')%\>%  
  group\_by(Product.type, Active.against.priority.pathogens.) %\>%  
  summarise(Count \= n(), .groups \= 'drop')

\# Create faceted bar plot  
ggplot(product\_activity, aes(x \= Product.type, y \= Count, fill \= Active.against.priority.pathogens.)) \+  
  geom\_bar(stat \= "identity",width \= 0.4) \+  
  facet\_wrap(\~ Active.against.priority.pathogens.) \+  
  scale\_fill\_manual(values \= c("Yes" \= "\#BC0CFF", "No" \= "\#008080","Possibly"="\#8B0000")) \+  
  labs(  
       x \= "Product Type",  
       y \= "Count",  
       fill \= "Activity Status") \+  
  xlab('')  
  theme\_minimal() \+  
  theme(axis.text.x \= element\_text(angle \= 45, hjust \= 1),  
        plot.title \= element\_text(size \= 4),  
        axis.title.x  \= element\_text(size \= 2),  
        axis.text \= element\_text(size \= 4))

  \#....................................  
    
    
  \#The relation between Rout of addministraition and drug activity  
  \# Summarize the data  
  Route\_Activity\_relation \<- AMR\_Products %\>%  
    filter(Active.against.priority.pathogens. \!='N/A')%\>%  
    filter(Route.of.administration \!='N/A')%\>%  
    group\_by(Route.of.administration, Active.against.priority.pathogens.) %\>%  
    summarise(Count \= n(), .groups \= 'drop')  
    
  \# Create the dot plot with ggplot  
  ggplot\_plot \<- ggplot(Route\_Activity\_relation, aes(x \= Route.of.administration, y \= Count, fill \= Active.against.priority.pathogens.)) \+  
    geom\_point(size \= 3\) \+  
    scale\_fill\_manual(values \= c("Yes" \= "green", "No" \= "red", "Possibly" \= "orange")) \+  
    labs(title \= "Relationship Between Route of Administration and Drug Effectiveness",  
         x \= "Route of Administration",  
         y \= "Number of Products",  
         color \= "Effectiveness Against Priority Pathogens") \+  
    theme\_minimal() \+  
    theme(axis.text.x \= element\_text(angle \= 45, hjust \= 1),  
          plot.title \= element\_text(size \= 8),  
          axis.title \= element\_text(size \= 8),  
          axis.text \= element\_text(size \= 8))  
    
  \#   
  interactive\_plot \<- ggplotly(ggplot\_plot)  
  \# Display the interactive plot  
  interactive\_plot  
  \#uploud the plot online  
  htmlwidgets::saveWidget(interactive\_plot,'interactive\_polt.html')  
  Sys.setenv("plotly\_username"="Abdullah988")  
  Sys.setenv("plotly\_api\_key"="r4cNVAaAC8iReKbMxLDS")  
  api\_create(interactive\_plot, filename \= "plotly")  
\#\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_  
  \# Summarize the data to count occurrences of each combination  
data \<- AMR\_Products %\>%  
    group\_by(Product.name, Antibacterial.class, R.D.phase,Pathogen.name,Active.against.priority.pathogens.) %\>%  
  filter(Active.against.priority.pathogens.\!='N/A')%\>%  
  filter(Active.against.priority.pathogens.\!='Possibly')%\>%  
    
    summarise(Count \= n(), .groups \= 'drop')  
    
 

\# Create the bubble chart  
bubble\_plot \<- plot\_ly(  
  data \= data,  
  x \= \~R.D.phase,  
  y \= \~Antibacterial.class,  
  size \= I(10),  \# Constant size for all bubbles  
  color \= \~Active.against.priority.pathogens.,  \# Color by activity against priority pathogens  
    \# Color by activity against priority pathogens  
  colors \= c("Yes" \= "blue", "No" \= "red"),  
   text \= \~paste("Product Name:", Product.name,   
                "\<br\>Pathogen Name:", Pathogen.name,   
                "\<br\>Active Against Priority Pathogens:", Active.against.priority.pathogens.),  
  hoverinfo \= 'text',  
  type \= 'scatter',  
  mode \= 'markers'  
) %\>%  
  layout(  
    xaxis \= list(title \= "R\&D Phase"),  
    yaxis \= list(title \= "Antibacterial Class"),  
    title \= "Bubble Plot of Antibacterial Products",  
    showlegend \= TRUE  
  )

\# Display the interactive bubble plot  
bubble\_plot  
\#upload the plot online  
Sys.setenv("plotly\_username"="Abdullah988")  
Sys.setenv("plotly\_api\_key"="r4cNVAaAC8iReKbMxLDS")  
api\_create(bubble\_plot, filename \= "bubbplot")  
\#\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_  
\#non traditional data categories  
Non.traditional=AMR\_Products%\>%filter(Product.type=="Non-traditional")  
Non.traditional  
colnames(Non.traditional)

ggplot(Non.traditional, aes(x \= "", y \= Non.traditionals.categories,   
                            fill \= factor(Non.traditionals.categories))) \+  
  geom\_bar(stat \= "identity", width \= 1\) \+  
  coord\_polar("y", start \= 0\) \+  
  scale\_fill\_discrete(name \= "Non.traditionals.categories") \+  
  labs(title \= "Pie Chart of Non.traditionals.categories") \+  
  theme\_void() \+  
  theme(plot.title \= element\_text(hjust \= 0.5),  
        legend.position \= "bottom",legend.title.position \= "top")  
\#Which category has effect on which pathigens

\# Create a simple dot plot   
ggplot(Non.traditional, aes(x \= Non.traditionals.categories, y \= Pathogen.name)) \+  
  geom\_point(aes(color \= Pathogen.name), size \= 3, position \= position\_jitter(width \= 0.2, height \= 0.2)) \+  
  labs(title \= "Effect of Products on Pathogens",  
       x \= "Non.traditionals.categories",  
       y \= "Pathogen Name",  
       color \= "Pathogen Name") \+  
  theme(legend.position \= "bottom")

\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#  
\#========================== create Bacteriophages\_and\_phage\_derived\_enzymes\_data  
Bacteriophages\_and\_phage\_derived\_enzymes\_data \<- AMR\_Products %\>%  
  filter(Non.traditionals.categories \== "Bacteriophages and phage-derived enzymes")

Bacteriophages\_and\_phage\_derived\_enzymes\_data

\#what is the route of adminstrtion  
ggplot(Bacteriophages\_and\_phage\_derived\_enzymes\_data,mapping \= aes(Route.of.administration,fill=Product.name))+  
  geom\_bar()+theme\_light()+scale\_fill\_manual(values \= pal\_igv()(20))

\#what is R.D phase of this proucts  
ggplot(Bacteriophages\_and\_phage\_derived\_enzymes\_data,mapping \= aes(R.D.phase,fill=Product.name))+  
  geom\_bar()+theme\_light()+scale\_fill\_manual(values \= pal\_igv()(20))

\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#  
\#========================== create Immunomodulating\_agents\_activity\_data  
Immunomodulating\_agents\_data \<- AMR\_Products %\>% filter(Non.traditionals.categories \== "Immunomodulating agents")

\# what products used in Immunomodulating\_agents\_activity  
ggplot(Immunomodulating\_agents\_data,mapping \= aes(Antibacterial.class,fill=Product.name))+  
  geom\_bar()+theme\_light()+scale\_fill\_manual(values \= pal\_igv()(20))

\#what is R.D phase of this proucts  
ggplot(Immunomodulating\_agents\_data,mapping \= aes(R.D.phase,fill=Product.name))+  
  geom\_bar()+theme\_light()+scale\_fill\_manual(values \= pal\_igv()(20))

\#what is the route of adminstrtion  
ggplot(Immunomodulating\_agents\_data,mapping \= aes(Route.of.administration,fill=Product.name))+  
  geom\_bar()+theme\_light()+scale\_fill\_manual(values \= pal\_igv()(20))

\#Application on different pathogens  
Immunomodulating\_agents \<- Non.traditional %\>%  
  filter(Product.name %in% c("Rhu-pGSN", "AB103"))  
Immunomodulating\_agents

\# Create a dot plot  
ggplot(Immunomodulating\_agents, aes(x \= Product.name, y \= Pathogen.name)) \+  
  geom\_point(aes(color \= Pathogen.name), size \= 3, position \= position\_jitter(width \= 0.2, height \= 0.2)) \+  
  labs(title \= "Effect of Rhu-pGSN and AB103 on Pathogens",  
       x \= "Product Name",  
       y \= "Pathogen Name",  
       color \= "Pathogen Name") \+  
  theme(  
    text \= element\_text(size \= 16),  \# Set base font size for all text  
    plot.title \= element\_text(size \= 20, face \= "bold"),    
    axis.title.x \= element\_text(size \= 18),                 
    axis.title.y \= element\_text(size \= 18),                 
    axis.text.x \= element\_text(size \= 14, angle \= 25, hjust \= 1),    
    axis.text.y \= element\_text(size \= 14),                   
    legend.text \= element\_text(size \= 14),                   
    legend.title \= element\_text(size \= 16),                  
    legend.position \= "bottom"                               
  )

\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#  
Microbiome\_modulating\_agents \<- AMR\_Products %\>% filter(Non.traditionals.categories \== "Microbiome-modulating agents")  
\# what is the anti bacterial class that have been used in Microbiome\_modulating\_agents  
ggplot(Microbiome\_modulating\_agents,mapping \= aes(Antibacterial.class,fill=Product.name))+  
  geom\_bar()+theme\_light()+scale\_fill\_manual(values \= pal\_igv()(20))

\#what is R.D phase of this proucts

\#what pathogens had been tried aganist products  
ggplot(Microbiome\_modulating\_agents,mapping \= aes(Pathogen.name,fill=Product.name))+  
  geom\_bar()+theme\_light()+scale\_fill\_manual(values \= pal\_igv()(20))

\#what is the route of adminstrtion  
ggplot(Microbiome\_modulating\_agents,mapping \= aes(Route.of.administration,fill=Product.name))+  
  geom\_bar()+theme\_light()+scale\_fill\_manual(values \= pal\_igv()(20))

\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#  
\#========================== create Miscellaneous\_data  
Miscellaneous\_data \<- AMR\_Products %\>% filter(Non.traditionals.categories \== "Miscellaneous")  
\# what is the anti bacterial class that have been used in Miscellaneous  
ggplot(Miscellaneous\_data,mapping \= aes(Antibacterial.class,fill=Product.name))+geom\_bar()+theme\_light()+scale\_fill\_manual(values \= pal\_igv()(20))+  
  theme(axis.title.x \= element\_text(size \= 12, face \= "bold"),  
        axis.title.y \= element\_text(size \= 12, face \= "bold"),  
        plot.title \= element\_text(size \= 14, face \= "bold"),  
        axis.text.x \= element\_text(size \= 10,angle \= 45, hjust \= 1),  
        axis.text.y \= element\_text(size \= 10))+coord\_flip()

\#what is R.D phase of this proucts  
ggplot(Miscellaneous\_data,mapping \= aes(R.D.phase,fill=Product.name))+geom\_bar()+theme\_light()+scale\_fill\_manual(values \= pal\_igv()(20))+  
  theme(axis.title.x \= element\_text(size \= 12, face \= "bold"),  
        axis.title.y \= element\_text(size \= 12, face \= "bold"),  
        plot.title \= element\_text(size \= 14, face \= "bold"),  
        axis.text.x \= element\_text(size \= 10,angle \= 45, hjust \= 1),  
        axis.text.y \= element\_text(size \= 10\)  
  )  
\#what is the route of adminstrtion  
ggplot(Miscellaneous\_data,mapping \= aes(Route.of.administration,fill=Product.name))+geom\_bar()+theme\_light()+scale\_fill\_manual(values \= pal\_igv()(20))+  
  theme(axis.title.x \= element\_text(size \= 12, face \= "bold"),  
        axis.title.y \= element\_text(size \= 12, face \= "bold"),  
        plot.title \= element\_text(size \= 14, face \= "bold"),  
        axis.text.x \= element\_text(size \= 10,angle \= 45, hjust \= 1),  
        axis.text.y \= element\_text(size \= 10\)  
  )

\#--------------------------------------------------------------  
\#========================== create antibodies\_data  
antibodies\_data \<- AMR\_Products %\>% filter(Non.traditionals.categories \== "Antibodies")  
\# what is the anti bacterial class that have been used in Antibodies  
ggplot(antibodies\_data,mapping \= aes(Antibacterial.class,fill=Product.name))+geom\_bar()+theme\_light()+scale\_fill\_manual(values \= pal\_igv()(20))+  
  theme(axis.title.x \= element\_text(size \= 12, face \= "bold"),  
        axis.title.y \= element\_text(size \= 12, face \= "bold"),  
        plot.title \= element\_text(size \= 14, face \= "bold"),  
        axis.text.x \= element\_text(size \= 10,angle \= 45, hjust \= 1),  
        axis.text.y \= element\_text(size \= 10))+coord\_flip()

\#what is R.D phase of this products  
ggplot(antibodies\_data,mapping \= aes(R.D.phase,fill=Product.name))+  
  geom\_bar()+theme\_light()+scale\_fill\_manual(values \= pal\_igv()(20))

\#what pathogens had been tried aganist products  
ggplot(antibodies\_data,mapping \= aes(Pathogen.name,fill=Product.name))+geom\_bar()+theme\_light()+scale\_fill\_manual(values \= pal\_igv()(20))+  
  theme(axis.title.x \= element\_text(size \= 12, face \= "bold"),  
        axis.title.y \= element\_text(size \= 12, face \= "bold"),  
        plot.title \= element\_text(size \= 14, face \= "bold"),  
        axis.text.x \= element\_text(size \= 10,angle \= 45, hjust \= 1),  
        axis.text.y \= element\_text(size \= 10\)  
  )  
\#what is the route of adminstrtion  
ggplot(antibodies\_data,mapping \= aes(Route.of.administration,fill=Product.name))+  
  geom\_bar()+theme\_light()+scale\_fill\_manual(values \= pal\_igv()(20))

\#products of antibodies aganist pathogens.  
\#--------------------------------------------------------------  
\#loading\_data  
antibodies\_activity\_data \<- AMR\_Products %\>% filter(Non.traditionals.categories \== "Antibodies")  
\#visualize data  
ggplot(antibodies\_activity\_data,mapping \= aes(Active.against.priority.pathogens.,fill=Product.name))+  
  geom\_bar()+theme\_light()+scale\_fill\_manual(values \= pal\_igv()(20))

## The code(s) for deep analysis:

\# Distribution of Product Type  
Product\_type\_summary\<-AMR\_Products%\>%  
  group\_by(Product.type)%\>%  
  summarise(count=n())  
Product\_type\_summary

ggplot(Product\_type\_summary, aes(x \= Product.type, y \= count, fill \=Product.type)) \+  
  geom\_bar(stat \= 'identity',width \= 0.3,position \= 'dodge') \+  
  scale\_fill\_manual(values \= c("\#4F81BD", "\#C0504D"))  \+  
  theme\_minimal()+xlab('')+  
  theme(text \= element\_text(size \= 9),  
        plot.title \= element\_text(hjust \=0.5,face \= 'bold' ),legend.position \= "bottom")

\#Test the product activity  
product\_activity\<-AMR\_Products%\>%  
  group\_by(Product.type, Active.against.priority.pathogens.)%\>%  
  summarise(Count=n())  
product\_activity

\# Create the stacked bar chart that shows distribution of Product Type with activity status  
ggplot(product\_activity, aes(x \= Product.type, y \= Count, fill \= Active.against.priority.pathogens.)) \+  
  geom\_bar(stat \= "identity", width \= 0.8) \+    
  scale\_fill\_manual(values \= c("Yes" \= "\#FF5733", "No" \= "grey", "Possibly" \= "\#3357FF")) \+  
  labs(  
    x \= "Product Type",  
    y \= "Count",  
    fill \= "Activity Status"  
  ) \+  
  theme\_minimal() \+  
  theme(legend.position \= "bottom")

\# Summarize the data  
Route\_Activity\_relation \<- AMR\_Products %\>%  
  group\_by(Route.of.administration, Active.against.priority.pathogens.) %\>%  
  summarise(Count \= n(), .groups \= 'drop')  
Route\_Activity\_relation

\# relation between Route od administration and activity status  
ggplot\_plot \<- ggplot(Route\_Activity\_relation, aes(x \= Route.of.administration, y \= Count, color \= Active.against.priority.pathogens.)) \+  
  geom\_point(size \= 3\) \+  
  scale\_color\_manual(values \= c("Yes" \= "\#FF5733", "No" \= "grey", "Possibly" \= "\#3357FF")) \+  
  labs(title \= "Relationship Between Route of Administration and Drug Effectiveness",  
       x \= "Route of Administration",  
       y \= "Number of Products",  
       color \= "Effectiveness Against Priority Pathogens") \+  
  theme\_minimal() \+  
  theme(legend.position \= "bottom", legend.title.position \= "bottom")

ggplot\_plot

\#================================Antibiotics data  
Antibiotics \<- AMR\_Products %\>% filter(Product.type \== "Antibiotics")

\#what is the R.D phase of antibiotic products  
ggplot(Antibiotics,mapping  \= aes(R.D.phase,fill=Product.name))+geom\_bar()+theme\_light()+scale\_fill\_manual(values \= pal\_igv()(50))+  
  theme(axis.title.x \= element\_text(size \= 12, face \= "bold"),  
        axis.title.y \= element\_text(size \= 12, face \= "bold"),  
        plot.title \= element\_text(size \= 14, face \= "bold"),  
        axis.text.x \= element\_text(size \= 10,angle \= 45, hjust \= 1),  
        axis.ticks.y \= element\_blank())

\#A:The vast majority of new antibiotic products present in phase 1

\#what is the route of adminstrtion of the products  
ggplot(Antibiotics,mapping \= aes(Route.of.administration,fill=Product.name))+geom\_bar()+theme\_light()+scale\_fill\_manual(values \= pal\_igv()(46))+  
  theme(axis.title.x \= element\_text(size \= 12, face \= "bold"),  
        axis.title.y \= element\_text(size \= 12, face \= "bold"),  
        plot.title \= element\_text(size \= 14, face \= "bold"),  
        axis.text.x \= element\_text(size \= 10,angle \= 45, hjust \= 1),  
        axis.text.y \= element\_text(size \= 10\)  
  )

\#what pathogens have been tried aganist Antibiotic products   
ggplot(Antibiotics,mapping \= aes(Pathogen.name,fill=Product.name))+geom\_bar()+theme\_light()+scale\_fill\_manual(values \= pal\_igv()(46))+  
  theme(axis.title.x \= element\_text(size \= 12, face \= "bold"),  
        axis.title.y \= element\_text(size \= 12, face \= "bold"),  
        plot.title \= element\_text(size \= 14, face \= "bold"),  
        axis.text.x \= element\_text(size \= 10,angle \= 45, hjust \= 1),  
        axis.text.y \= element\_text(size \= 10)+coord\_flip()  
  )

\# Activity status of the antibotics aganist pathogens  
ggplot(Antibiotics,mapping \= aes(Active.against.priority.pathogens.,fill=Product.name))+  
  geom\_bar()+theme\_light()+scale\_fill\_manual(values \= pal\_igv()(50))

\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#  
\#non traditional data categories  
Non.traditional=AMR\_Products%\>%filter(Product.type=="Non-traditional")  
Non.traditional  
colnames(Non.traditional)

ggplot(Non.traditional, aes(x \= "", y \= Non.traditionals.categories, fill \= factor(Non.traditionals.categories))) \+  
  geom\_bar(stat \= "identity", width \= 1\) \+  
  coord\_polar("y", start \= 0\) \+  
  scale\_fill\_discrete(name \= "Non.traditionals.categories") \+  
  labs(title \= "Pie Chart of Non.traditionals.categories") \+  
  theme\_void() \+  
  theme(plot.title \= element\_text(hjust \= 0.5), legend.position \= "bottom",legend.title.position \= "top")

\#Which category has effect on which pathigens

\# Create a simple dot plot   
ggplot(Non.traditional, aes(x \= Non.traditionals.categories, y \= Pathogen.name)) \+  
  geom\_point(aes(color \= Pathogen.name), size \= 3, position \= position\_jitter(width \= 0.2, height \= 0.2)) \+  
  labs(title \= "Effect of Products on Pathogens",  
       x \= "Non.traditionals.categories",  
       y \= "Pathogen Name",  
       color \= "Pathogen Name") \+  
  theme(legend.position \= "bottom")

\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#  
\#========================== create Bacteriophages\_and\_phage\_derived\_enzymes\_data  
Bacteriophages\_and\_phage\_derived\_enzymes\_data \<- AMR\_Products %\>%  
  filter(Non.traditionals.categories \== "Bacteriophages and phage-derived enzymes")

Bacteriophages\_and\_phage\_derived\_enzymes\_data

\#what is the route of adminstrtion  
ggplot(Bacteriophages\_and\_phage\_derived\_enzymes\_data,mapping \= aes(Route.of.administration,fill=Product.name))+  
  geom\_bar()+theme\_light()+scale\_fill\_manual(values \= pal\_igv()(20))

\#what is R.D phase of this proucts  
ggplot(Bacteriophages\_and\_phage\_derived\_enzymes\_data,mapping \= aes(R.D.phase,fill=Product.name))+  
  geom\_bar()+theme\_light()+scale\_fill\_manual(values \= pal\_igv()(20))

\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#  
\#========================== create Immunomodulating\_agents\_activity\_data  
Immunomodulating\_agents\_data \<- AMR\_Products %\>% filter(Non.traditionals.categories \== "Immunomodulating agents")

\# what products used in Immunomodulating\_agents\_activity  
ggplot(Immunomodulating\_agents\_data,mapping \= aes(Antibacterial.class,fill=Product.name))+  
  geom\_bar()+theme\_light()+scale\_fill\_manual(values \= pal\_igv()(20))

\#what is R.D phase of this proucts  
ggplot(Immunomodulating\_agents\_data,mapping \= aes(R.D.phase,fill=Product.name))+  
  geom\_bar()+theme\_light()+scale\_fill\_manual(values \= pal\_igv()(20))

\#what is the route of adminstrtion  
ggplot(Immunomodulating\_agents\_data,mapping \= aes(Route.of.administration,fill=Product.name))+  
  geom\_bar()+theme\_light()+scale\_fill\_manual(values \= pal\_igv()(20))

\#Application on different pathogens  
Immunomodulating\_agents \<- Non.traditional %\>%  
  filter(Product.name %in% c("Rhu-pGSN", "AB103"))  
Immunomodulating\_agents

\# Create a dot plot  
ggplot(Immunomodulating\_agents, aes(x \= Product.name, y \= Pathogen.name)) \+  
  geom\_point(aes(color \= Pathogen.name), size \= 3, position \= position\_jitter(width \= 0.2, height \= 0.2)) \+  
  labs(title \= "Effect of Rhu-pGSN and AB103 on Pathogens",  
       x \= "Product Name",  
       y \= "Pathogen Name",  
       color \= "Pathogen Name") \+  
  theme(  
    text \= element\_text(size \= 16),  \# Set base font size for all text  
    plot.title \= element\_text(size \= 20, face \= "bold"),    
    axis.title.x \= element\_text(size \= 18),                 
    axis.title.y \= element\_text(size \= 18),                 
    axis.text.x \= element\_text(size \= 14, angle \= 25, hjust \= 1),    
    axis.text.y \= element\_text(size \= 14),                   
    legend.text \= element\_text(size \= 14),                   
    legend.title \= element\_text(size \= 16),                  
    legend.position \= "bottom"                               
  )

\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#  
Microbiome\_modulating\_agents \<- AMR\_Products %\>% filter(Non.traditionals.categories \== "Microbiome-modulating agents")  
\# what is the anti bacterial class that have been used in Microbiome\_modulating\_agents  
ggplot(Microbiome\_modulating\_agents,mapping \= aes(Antibacterial.class,fill=Product.name))+  
  geom\_bar()+theme\_light()+scale\_fill\_manual(values \= pal\_igv()(20))

\#what is R.D phase of this proucts

\#what pathogens had been tried aganist products  
ggplot(Microbiome\_modulating\_agents,mapping \= aes(Pathogen.name,fill=Product.name))+  
  geom\_bar()+theme\_light()+scale\_fill\_manual(values \= pal\_igv()(20))

\#what is the route of adminstrtion  
ggplot(Microbiome\_modulating\_agents,mapping \= aes(Route.of.administration,fill=Product.name))+  
  geom\_bar()+theme\_light()+scale\_fill\_manual(values \= pal\_igv()(20))

\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#  
\#========================== create Miscellaneous\_data  
Miscellaneous\_data \<- AMR\_Products %\>% filter(Non.traditionals.categories \== "Miscellaneous")  
\# what is the anti bacterial class that have been used in Miscellaneous  
ggplot(Miscellaneous\_data,mapping \= aes(Antibacterial.class,fill=Product.name))+geom\_bar()+theme\_light()+scale\_fill\_manual(values \= pal\_igv()(20))+  
  theme(axis.title.x \= element\_text(size \= 12, face \= "bold"),  
        axis.title.y \= element\_text(size \= 12, face \= "bold"),  
        plot.title \= element\_text(size \= 14, face \= "bold"),  
        axis.text.x \= element\_text(size \= 10,angle \= 45, hjust \= 1),  
        axis.text.y \= element\_text(size \= 10))+coord\_flip()

\#what is R.D phase of this proucts  
ggplot(Miscellaneous\_data,mapping \= aes(R.D.phase,fill=Product.name))+geom\_bar()+theme\_light()+scale\_fill\_manual(values \= pal\_igv()(20))+  
  theme(axis.title.x \= element\_text(size \= 12, face \= "bold"),  
        axis.title.y \= element\_text(size \= 12, face \= "bold"),  
        plot.title \= element\_text(size \= 14, face \= "bold"),  
        axis.text.x \= element\_text(size \= 10,angle \= 45, hjust \= 1),  
        axis.text.y \= element\_text(size \= 10\)  
  )  
\#what is the route of adminstrtion  
ggplot(Miscellaneous\_data,mapping \= aes(Route.of.administration,fill=Product.name))+geom\_bar()+theme\_light()+scale\_fill\_manual(values \= pal\_igv()(20))+  
  theme(axis.title.x \= element\_text(size \= 12, face \= "bold"),  
        axis.title.y \= element\_text(size \= 12, face \= "bold"),  
        plot.title \= element\_text(size \= 14, face \= "bold"),  
        axis.text.x \= element\_text(size \= 10,angle \= 45, hjust \= 1),  
        axis.text.y \= element\_text(size \= 10\)  
  )

\#--------------------------------------------------------------  
\#========================== create antibodies\_data  
antibodies\_data \<- AMR\_Products %\>% filter(Non.traditionals.categories \== "Antibodies")  
\# what is the anti bacterial class that have been used in Antibodies  
ggplot(antibodies\_data,mapping \= aes(Antibacterial.class,fill=Product.name))+geom\_bar()+theme\_light()+scale\_fill\_manual(values \= pal\_igv()(20))+  
  theme(axis.title.x \= element\_text(size \= 12, face \= "bold"),  
        axis.title.y \= element\_text(size \= 12, face \= "bold"),  
        plot.title \= element\_text(size \= 14, face \= "bold"),  
        axis.text.x \= element\_text(size \= 10,angle \= 45, hjust \= 1),  
        axis.text.y \= element\_text(size \= 10))+coord\_flip()

\#what is R.D phase of this products  
ggplot(antibodies\_data,mapping \= aes(R.D.phase,fill=Product.name))+  
  geom\_bar()+theme\_light()+scale\_fill\_manual(values \= pal\_igv()(20))

\#what pathogens had been tried aganist products  
ggplot(antibodies\_data,mapping \= aes(Pathogen.name,fill=Product.name))+geom\_bar()+theme\_light()+scale\_fill\_manual(values \= pal\_igv()(20))+  
  theme(axis.title.x \= element\_text(size \= 12, face \= "bold"),  
        axis.title.y \= element\_text(size \= 12, face \= "bold"),  
        plot.title \= element\_text(size \= 14, face \= "bold"),  
        axis.text.x \= element\_text(size \= 10,angle \= 45, hjust \= 1),  
        axis.text.y \= element\_text(size \= 10\)  
  )  
\#what is the route of adminstrtion  
ggplot(antibodies\_data,mapping \= aes(Route.of.administration,fill=Product.name))+  
  geom\_bar()+theme\_light()+scale\_fill\_manual(values \= pal\_igv()(20))

\#products of antibodies aganist pathogens.  
\#--------------------------------------------------------------  
\#loading\_data  
antibodies\_activity\_data \<- AMR\_Products %\>% filter(Non.traditionals.categories \== "Antibodies")  
\#visualize data  
ggplot(antibodies\_activity\_data,mapping \= aes(Active.against.priority.pathogens.,fill=Product.name))+  
  geom\_bar()+theme\_light()+scale\_fill\_manual(values \= pal\_igv()(20))

