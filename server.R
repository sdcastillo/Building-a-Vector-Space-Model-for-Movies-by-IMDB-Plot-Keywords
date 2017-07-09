
library(shiny)
library(stringr)
library(lsa)
library(parallel)
library(sna)
library(network)
library(SnowballC)
library(statnet.common)
library(datasets)
library(graphics)
library(grDevices)
library(methods)
library(parallel)
library(stats)
library(utils)
library(tidyr)
library(dplyr)

imdb = read.csv("imdb.csv", header = T, na.strings=c("","NA"))
imdb = imdb %>% select(gross, genres, movie_title, country, movie_imdb_link, budget, title_year, imdb_score, content_rating, plot_keywords, actor_1_name, actor_2_name, actor_3_name )

names(imdb) = c("gross", "genres", "title", "country","links", "budget", "year", "score", "rating", "keywords", "actor1", "actor2", "actor3")

#cleaning up plot keywords
imdb$title = as.character(imdb$title)
imdb$keywords = as.character(imdb$keywords)
imdb$keywords = strsplit(imdb$keywords, split = "|", fixed = TRUE) %>%as.list()

#simplify the genres by taking the first entry
imdb$genres = as.character(imdb$genres)
imdb$genres_simple = strsplit(imdb$genres, split = "|", fixed = TRUE) 
imdb$genres_simple = as.character(imdb$genres_simple)
imdb$genres_simple = str_extract(imdb$genres, pattern = "^[A-Za-z]{1,20}")
imdb$genres_simple = as.factor(imdb$genres_simple)
imdb$title = gsub(imdb$title, pattern = "?", replacement ="")
imdb$links = as.character(imdb$links)

#Convert data types
imdb$genres = as.factor(imdb$genres)
imdb$budget = as.numeric(imdb$budget)
imdb$gross = as.numeric(imdb$gross)
imdb$score = as.numeric(imdb$score)
imdb$rating = as.factor(imdb$rating)

unique_keywords = imdb$keywords %>% unlist() %>% unique()

n = length(unique_keywords)

#Need an index number for each link to match
vectorize = function(keywords_list){
  #create a vector from the list
  cur_keywords = keywords_list %>% unlist()
  #initialize an empty vector
  out = c(rep(0, n))
  for(i in cur_keywords){
    index = match(i, unique_keywords)
    out[index] = 1
  }
  return(out)
} 

imdb$keyword_vectors = lapply( FUN = vectorize, X = imdb$keywords)

#keywords for actors
by_actor = imdb %>%
  select( actor1, actor2, actor3, keyword_vectors)%>% 
  gather(key = actor, keywords, actor1:actor3 )%>%
  select( keyword_vectors, actor = keywords) %>%
  ungroup()%>%
  group_by(actor) %>%
  summarise( keywords = list(Reduce("+", keyword_vectors)))

names(by_actor) = c("actor", "keyword_vectors")

#Which actors are most popular?
df = imdb %>%
  select( actor1, actor2, actor3, keyword_vectors)%>% 
  gather(key = actor, keywords, actor1:actor3 )%>%
  select( keyword_vectors, actor = keywords) %>%
  group_by(actor) %>%
  summarise(count = n()) %>%
  na.omit() %>%
  arrange(desc(count))

df$count = as.numeric(df$count)
#Filter to only those actors wit h>10 appearances
popular_actors = subset( df, count > 10)

by_actor = filter( by_actor, (actor %in% popular_actors$actor))

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
   output$addition_text_movie <- renderText({
    
    closest_numeric = function ( cur_vector, titleA, titleB){
      
      temp = imdb %>% select( title, keyword_vectors) %>%
        filter(title != titleA, title!= titleB)
      
      my_cos = function( list_vectors){
        x = unlist( cur_vector)
        y = unlist( list_vectors)
        cosine(x,y)
      }
      
      temp$cosines = sapply( FUN = my_cos, temp$keyword_vectors)
      
      out = temp %>%
        group_by(title) %>%
        arrange( desc( cosines))
      
      return( c(out[1,]$title, out[1,]$cosines))
    }
    
    addition = function( titleA, titleB){
      movieA =  filter( imdb, title == titleA)
      movieB = filter( imdb, title == titleB)
      x = movieA$keyword_vectors %>% unlist()
      y = movieB$keyword_vectors %>% unlist()
      AplusB = x + y
      result = closest_numeric(AplusB, titleA, titleB)
      paste( titleA, " + ", titleB, " = ", result[1], "", 
             paste("Cosine of Angle: ", result[2]), "", 
             paste("Angle in Pi Radians: ", acos(as.numeric(result[2]))/pi), "",
             paste("Angle in Degrees: ", acos(as.numeric(result[2]))*360/(2*pi)),sep = "\n")
    }
  
    paste( addition( input$MovieA, input$MovieB) )
   
  })
   
   output$addition_text_actor <- renderText({
     
     #Now the "titles" are the actors names
     closest_numeric_actor = function ( cur_vector, titleA, titleB, dist = F){
       
       temp = by_actor%>% filter(actor != titleA, actor!= titleB)
       
       my_cos = function( list_vectors){
         x = unlist( cur_vector)
         y = unlist( list_vectors)
         cosine(x,y)
       }
       
       euc.dist <- function(list_vectors) {
         x1 = unlist( cur_vector)
         x2 = unlist( list_vectors)
         sqrt(sum((x1 - x2) ^ 2))}
       
       cur_function = my_cos
       if( dist == T){cur_function = euc.dist}
       
       # my_cos(by_actor$keyword_vectors[2])
       relation = lapply( FUN = cur_function, temp$keyword_vectors)
       
       temp$relation = relation
       # class(as.numeric(temp$cosines))
       out = temp %>%
         group_by(actor) %>%
         arrange( desc( as.numeric(relation)))
       
       return( out[1,]$actor)
     }
     
     actor_addition = function( titleA, titleB, dist = F){
       actorA =  filter( by_actor, actor == titleA)
       actorB = filter( by_actor, actor == titleB)
       x = actorA$keyword_vectors %>% unlist()
       y = actorB$keyword_vectors %>% unlist()
       AplusB = x + y
       result = closest_numeric_actor(AplusB, titleA, titleB, dist)
       paste( titleA, " + ", titleB, " = " ,result)
     }
     
     actor_addition(input$actorA, input$actorB, input$euc.dist == "Euclidean Distance")
   })
  
})
