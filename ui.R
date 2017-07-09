
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
unique_keywords = imdb$keywords %>% unlist() %>% unique()

n = imdb$keywords %>%
  unlist()%>%
  unique()%>%
  length()

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

keyword_vectors = lapply( FUN = vectorize, X = imdb$keywords)

imdb$keyword_vectors = keyword_vectors

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

by_actor = filter( by_actor, actor %in% popular_actors$actor)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  # Application title
  titlePanel("Defining an Inner Product in Movie Plot Keywords"),
 
    mainPanel(
      
      
      tabsetPanel(
        tabPanel("Summary",
                 withMathJax(),
                 helpText("This application compares movies and actors by the plot keywords of the the film as
                          as tagged on imdb.com.  The dataset can be downloaded from Kaggle.
                          
                          https://www.kaggle.com/deepmatrix/imdb-5000-movie-dataset
                          "),
                 
                 helpText("For each movie, there is a list of keywords.  For example, for the movie 'Monte Python and the Holy Grail', the 
                          keywords might be 'knight', 'camelot', 'wizard', 'holy grail', 'castle', etc.  For simplicity just assume there are
                          two keywords, 'camelot', and 'wizard.'  For a different movie, such as 'The Wizard of Oz', the keywords might be 'kansas',
                          and 'wizard'.  "),
                 
                 helpText("Here is what this a word space might look like.  In this
                          case the dimensions have been simplified to only three words.  This are 'camelot', 'wizard', and 'kansas'.  
                          
                          The word vector for Monty Python is <1, 1, 0>, and < 0, 1, 1> for the Wizard of Oz.  A potential match might be The 
                          Princess Bride."),
                 
                 img(src='3d_word_space.png', height = 500, width = 500),
                 
                 helpText("The closest movie is determined by minimizing the angle between the sum of the two vectors and a third movie which
                          is not one of the first two."),
                 
                 helpText("A similar algorithm follows for the addition of actors and actresses.  In this case, for each actor I take the sum
                          of all the keyword vectors from films where they have been in a leading role as defined by the data set.")
                 
                 
                 
                 
                 
                 
                 ),
        tabPanel("Movies", 
                 
                 selectInput(inputId = "MovieA", 
                             label = "First Movie: ", 
                             choices = imdb$title,
                             selected = NULL, 
                             multiple = FALSE, 
                             selectize = TRUE),
                 
                 selectInput(inputId = "MovieB", 
                             label = "Second Movie: ", 
                             choices = imdb$title,
                             selected = NULL, 
                             multiple = FALSE, 
                             selectize = TRUE),
                 
                 submitButton("Calculate"),
                 
                 verbatimTextOutput("addition_text_movie")),
        
        tabPanel("Actors",
                 
                            selectInput(inputId = "actorA", 
                             label = "First Actor: ", 
                             choices = by_actor$actor,
                             selected = NULL, 
                             multiple = FALSE, 
                             selectize = TRUE),
                 
                 selectInput(inputId = "actorB", 
                             label = "Second Actor: ", 
                             choices = by_actor$actor,
                             selected = NULL, 
                             multiple = FALSE, 
                             selectize = TRUE),
                 
                 selectInput(inputId = "euc.dist", 
                             label = "Relation Method: ", 
                             choices = c("Cosine of Angle", "Euclidean Distance"),
                             selected = NULL, 
                             multiple = FALSE, 
                             selectize = TRUE),
                 
                 
                 submitButton("Calculate"),
                 
                 verbatimTextOutput("addition_text_actor"))
        )
     
       # verbatimTextOutput("addition_text")
    )
  )
)
