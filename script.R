library(rvest)
library(tidyverse)
library(stringr)
library(stringi)
library(hrbrthemes)


################################################################################
## scrape-extract-clean data
################################################################################
#imdb list of screen times
url1 <- "http://www.imdb.com/list/ls076752033/"
url2 <- "http://www.imdb.com/list/ls076752033/?start=101"
# scrape relevant info ----
doc1 <- read_html(url1) %>% 
  html_nodes(".description")
doc1 <- doc1[-1]

doc2 <- read_html(url2) %>% 
  html_nodes(".description")
doc2 <- doc2[-1]

char1 <- read_html(url1) %>% 
  html_nodes(".info") %>% 
  html_node("a") %>% 
  html_text()

char2 <- read_html(url2) %>% 
  html_nodes(".info") %>% 
  html_node("a") %>% 
  html_text()

char <- c(char1,char2)

char <- str_replace(char,"'[a-zA-Z ]+' ","")
char[40]  <- "Pycelle" 
char[105] <- "Ebrose"
char[110] <- "Three-Eyed raven"
char[122] <- "Ray"
char[135] <- "Moro"
char[145] <- "Dontos"
char[153] <- "Wolkan"
# extract screen times ----
screen_regex <- "[Ss]eason [1-7]+: [0-9]+ episode[s]* (\\(main\\) )*<([0-9]+)*(:[0-9]+)*>"
screen_times <- c(stri_extract_all_regex(html_text(doc1),screen_regex),
                  stri_extract_all_regex(html_text(doc2),screen_regex))


#build data.frame ----
length_list <- map(screen_times,length) %>% unlist()
season <- map(screen_times,function(x) stri_extract_first_regex(x,"[0-9]+") %>% as.numeric) %>% unlist
time   <- map(screen_times,function(x) stri_extract_last_regex(x,"(?<=<)(.*?)(?=>)") )  %>% unlist

times_df <- tibble(character=rep(char,length_list),season=season,time=time)

times_df <- times_df %>% 
  mutate(time = str_replace_all(time,"^:","0:")) %>% 
  mutate(time = ifelse(str_detect(time,":"),time,paste0(time,":00"))) %>% 
  mutate(time = as.numeric(word(time,1,sep=":"))+as.numeric(word(time,2,sep=":"))/60) %>% 
  group_by(character,season) %>% 
  dplyr::summarise(time=sum(time))

#scrape the house of each character ----
scrape_house <- function(name){
  name <- str_replace_all(name," ","_")
  url <- paste0("http://gameofthrones.wikia.com/wiki/",name)
  read_html(url) %>% 
    html_node(".image.image-thumbnail.link-internal") %>% 
    html_attr("title")
}

house <- rep("",199)
for(i in 1:199){
  house[i] <- scrape_house(char[i])
}

times_df <- times_df %>% 
  left_join(tibble(character = char,house = house))
times_df$house[str_detect(times_df$house,"Baratheon")] <- "Baratheon"
write_csv(times_df,"data.csv")
#clean up ----
rm(char,doc,house,i,length_list,screen_regex,screen_times,season,time,url,scrape_house)

################################################################################
## analyse and plot
################################################################################
# screen time per house ----
times_df %>% 
  dplyr::filter(str_detect(house,"House")) %>% 
  mutate(house=str_replace(house,"House ","")) %>% 
  group_by(house,season) %>% 
  dplyr::summarise(total=sum(time)) %>% 
  mutate(sum=sum(total)) %>% 
  ungroup() %>% 
  dplyr::filter(dense_rank(-sum)<=10) %>% 
ggplot(aes(x = reorder(house,sum), y = total,fill=factor(season,7:1))) +
  geom_col()+
  coord_flip()+
  scale_fill_manual(values=ipsum_pal()(7),name="Season")+
  scale_y_continuous(breaks=c(0,300,600,900,1100))+
  theme_ipsum_rc(grid=F)+
  theme(legend.position=c(0.95,0.4),
        plot.caption = element_text(color="grey"))+
  guides(fill = guide_legend(reverse=T))+
  labs(x="House",y="Screen time (Minutes)",title="Screen time by house",caption="schochastics.net")

ggsave("time_house.png")


#screen time per character ----
times_df %>% 
  group_by(character) %>% 
  mutate(sum=sum(time)) %>% 
  ungroup() %>% 
  dplyr::filter(dense_rank(-sum)<=50) %>% 
  ggplot(aes(x = reorder(character,sum), y = time,fill=factor(season,7:1))) +
    geom_col()+
    coord_flip()+
    scale_fill_manual(values=ipsum_pal()(7),name="Season")+
    # scale_y_continuous(breaks=c(0,300,600,900,1100))+
    theme_ipsum_rc(grid=F)+
    theme(legend.position=c(0.95,0.4),
          plot.caption = element_text(color="grey"))+
    guides(fill = guide_legend(reverse=T))+
    labs(x="Character",y="Screen time (Minutes)",title="Screen time of characters",caption="schochastics.net")
ggsave("time_character.png",height=12,width=5)

#most screen time per season ----
times_df %>% 
  group_by(season) %>%
  top_n(10,time) %>% 
  ggplot(aes(x = reorder(character,time), y = time))+
  geom_col()+
  coord_flip()+
  facet_wrap(~season,scales = "free")

#create list of plots
times_df_fil <- times_df %>% 
  group_by(season) %>%
  top_n(10,time) %>% 
  ungroup()

myplots <- lapply(split(times_df_fil,times_df_fil$season), function(x){
  
  x$character <- factor(x$character, levels=x$character[order(x$time,decreasing=F)])
  
  #make the plot
  p <-   ggplot(x,aes(x = character, y = time))+
    geom_col()+
    coord_flip()+
    theme_ipsum_rc(grid=F)+
    labs(x="",y="",title=paste("Season",x$season[1]))
})

p <- do.call(gridExtra::arrangeGrob,(c(myplots, ncol=3)))
ggsave("char_per_season.png",plot=p,height=10,width=10)
