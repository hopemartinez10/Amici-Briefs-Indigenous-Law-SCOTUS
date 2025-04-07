#Hope Martinez
#CHT 2 STM K Search Native briefs
#2/12/2025

library(ggplot2)
library(readtext)
library(stm)
library(quanteda)
library(stringr)
library(tm)
library(dplyr)


Nativebriefs <- readtext("/Users/hmartinez10/OneDrive - Georgia State University (1)/Data 2024/ICWA/Native_1997_2023/*.pdf")

Nativebriefs <- Nativebriefs %>%
  mutate(Group = 0)
Statebriefs <- readtext("/Users/hmartinez10/OneDrive - Georgia State University (1)/Data 2024/ICWA/State_1997_2023/*.pdf")

Statebriefs <- Statebriefs %>% 
  mutate(Group = 1)

combined_data <- rbind(Nativebriefs, Statebriefs)

# simulate words to remove #THESE WORDS ARE NOT BEING REMOVED
remove.words <- readLines(textConnection(c("and", "or", "but", "of", "indian", "court",
                                           "tribe", "tribal", "author", "amici", "navajo",
                                           "curia", "Oklahoma", "choctaw", "creek",
                                           "petit", "www", "https", "said", "code", "chickasaw", "suit", "general",
                                           "tort", "igra", "attorney", "org", "app")))

temp<-textProcessor(documents = combined_data$text, metadata = combined_data, customstopwords=remove.words, ucp = TRUE, onlycharacter = TRUE, stem = TRUE, removenumbers = TRUE, removepunctuation = TRUE)
meta<-temp$meta
vocab<-temp$vocab
docs<-temp$documents
out<-prepDocuments(docs, vocab,meta, lower.thresh = 8, upper.thresh = 130)
docs<-out$documents
vocab<-out$vocab
meta<-out$meta
set.seed(02138)
K <- c(8, 10, 12)
df1 <- searchK(docs, vocab, K, data = meta)
plot(df1)



#model and plots

Briefs <- stm(documents=out$documents, vocab=out$vocab, K=8, content = ~Group, max.em.its = 75, 
              data = out$meta, seed=9876)

#plot topics
plot(Briefs, type="summary", n=4, xlim=c(0, .5),  main="")

#plot per topic
plot(Briefs, type="perspective", topics=1, plabels=c("Native", "State"))
plot(Briefs, type="perspective", topics=2, plabels=c("Native", "State"))
plot(Briefs, type="perspective", topics=3, plabels=c("Native", "State"))
plot(Briefs, type="perspective", topics=4, plabels=c("Native", "State"))
plot(Briefs, type="perspective", topics=5, plabels=c("Native", "State"))
plot(Briefs, type="perspective", topics=6, plabels=c("Native", "State"))
plot(Briefs, type="perspective", topics=7, plabels=c("Native", "State"))
plot(Briefs, type="perspective", topics=8, plabels=c("Native", "State"))

#word clouds
cloud(Briefs, topics = 3)
cloud(Briefs, 1)
