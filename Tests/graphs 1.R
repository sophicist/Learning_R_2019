library(dslabs)
library(tidyverse)
head(murders)
library(ggthemes)
library(ggrepel)


r <- murders %>%
  summarize(rate = sum(total) / sum(population) * 10^6) %>%
  pull(rate)

murders %>% ggplot(aes(x =population/10^6,y =total,color =region,label =abb))+
  geom_abline(intercept = log10(r), lty = 2, color = "darkgrey")+geom_point(size =3) +#+geom_hline(yintercept=mean(murders$total), linetype="dotted")+
  geom_text_repel(nudge_x = 0.065)+scale_x_log10()+scale_y_log10()+
  labs(title ="Murder vs population 2010",x ='Population (log scale)',y ="Murders (Log scale")+theme_economist()


head(heights)
H <-heights %>% filter(sex == 'Male')
H %>% ggplot(aes(x = height))+geom_histogram(binwidth = 1,fill ='blue',col ='black')+labs(title ="Male Heights Histogram")#+geom_density()

p <- heights %>% filter(sex == "Male") %>%
  ggplot(aes(sample = height))
p + geom_qq()+geom_abline()

heights %>%drop_na() %>%  ggplot(aes(sample = scale(height)) +
           geom_qq() +
           geom_abline()
         
         