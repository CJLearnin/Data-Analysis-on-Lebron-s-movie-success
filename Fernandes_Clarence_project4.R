################################################
###  Script For project 4
###  RQ: Can the number of faces on a move poster, duration, 
#          and budget of a movie predict the IMDB rating it will receive?
###  Clarence Fernandes 
###  last updated May 14 2019
############################################


#5 best fit questions
#4 general theory questions 
#3 interprtating questions
# 4 computational questions 


movies = read.csv(file.choose())
movies_16 = subset(movies,movies$title_year>=2013)
View(movies_16)

# Creating summary tables for each IV

library(summarytools)# FOR DESCR functions

descr(movies_16$budget)
descr(movies_16$duration)
descr(movies_16$facenumber_in_poster)
descr(movies_16$imdb_score)
descr(movies_16$movie_facebook_likes)
mean(movies_16$budget)



# Making my models
lm.movies_16 = lm(imdb_score ~ movie_facebook_likes+facenumber_in_poster+ budget + duration , data = movies_16)
summary(lm.movies_16)

# A 1 unit change in number of faces corresponds to a -6.1 decrease in the average IMDB score
# at a p value of 0.002 we can conclude it is stat sig
# a 1 unit change in IMDb score corresponds to a -.61 decrease in the number of faces in a movie poster. 
# a 1 unit change in the budget corres

# Effect size
(0.2177)/(1-.2177)

plot(lm.movies_16)

library(stargazer)
stargazer(lm.movies_16,model1, type="html",  covariate.labels = c(),
          out="models2.htm")
library(ggplot2)
ggplot(movies_16,aes(y=imdb_score,x=budget,color=duration))+geom_point()+stat_smooth(method="lm",se=FALSE)
ggplot(movies_16,aes(y=imdb_score,x=facenumber_in_poster,color=movie_facebook_likes))+geom_point()+stat_smooth(method="lm",se=FALSE)

