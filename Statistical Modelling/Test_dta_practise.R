library(gapminder)
h <- gapminder %>% group_by(country) %>% nest()
head(h)

Ken<- gapminder %>% filter(country == "Kenya")
mod <- lm(data = Ken,gdpPercap~year)

fu <- function(df){lm(data = df,gdpPercap~year)}

h.model <- h %>% mutate(model = map(data,fu),coef = map(model, broom::tidy))
coef <- h.model %>% unnest(coef) %>% filter(term == "(Intercept)") %>% ggplot(aes(x = estimate))+geom_density()
coef



head(gapminder)
y <- gapminder %>% group_by(country,continent) %>% nest()
tes <- function(df){lm(lifeExp~year,data =df)}
head(gapminder)
s <- y %>% mutate (model =map(data,tes),coef = map(model, broom::tidy))
s %>% unnest(coef)



# Crossvalidation
library(gapminder)
library(modelr)
df<- gapminder
head(df)

cross<- crossv_kfold(df,k =6)
y = map(cross$train,~lm(lifeExp~gdpPercap,data =.))

thende<-function(model,test){
  test<- data.frame(test)
  mod<-add_predictions(test,model)
  return(mod)
}
get_pred  <- function(model, test_data){
  data  <- as.data.frame(test_data)
  pred  <- add_predictions(data, model)
  return(pred)}
model1 <- map2_df(y,cross$test,thende,.id = "index")
head(model1)
model1 <- model1 %>% mutate(MSE = (lifeExp - pred)^2)
model1 %>% group_by(index) %>% summarise(M = mean(MSE)) %>% summarise(mean(M))

y2 <- map(cross$train,~lm(lifeExp~pop+gdpPercap+year+as.factor(country),data =.))
mod2<- map2_df(y2,cross$test,thende,.id ="index")
mod2 %>% mutate(MSE = (lifeExp - pred)^2) %>% group_by(index) %>% summarise(M = mean(MSE)) %>% summarise(mean(M))


y <- gapminder %>% group_by(country) %>% nest()
x <- function(df){lm(lifeExp~year+pop,data = df)}

map(y$data[1],x)
y %>% mutate(model =map(data,x),coef = map(model,broom::tidy) ) %>% unnest(coef) %>% 
  filter(term=='pop') %>% ggplot(aes(x = estimate))+geom_density()

library(modelr)
library(tidyr)
model <- lm(lifeExp~1,data = gapminder)
summary(step(model,scope=list(upper=lm(lifeExp ~ ., data=gapminder)),direction = 'forward'))

model <- lm(lifeExp~,data = gapminder)
summary(step(model,scope=list(upper=lm(lifeExp ~1 ., data=gapminder)),direction = 'forward'))


y <- iris
head(y)
H <-crossv_kfold(y,k =7)
model1 <-map(H$train,~lm(Sepal.Length~Species,data = .))
te <- function(model,test){
  test<- as.data.frame(test)
  pred <-add_predictions(test,model)
  return(pred)
}

pred<- map2_df(model1, H$test, te, .id = "Run")
pred<- pred %>% mutate(MSE =(Sepal.Length-pred)^2 )
pred %>% group_by(Run) %>% summarise(Mean = mean(MSE)) %>% summarise(mean(Mean))
