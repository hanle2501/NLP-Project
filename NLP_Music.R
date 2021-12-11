######################### FINAL PROJECT ####################################

library(wordcloud)
library(ggplot2)
library(udpipe)
library(dplyr)
library(genius)
library(lattice)
library(gridExtra)

##################### General Interest ##########################

spotify = read.csv('spotify_songs.csv')

spotify = na.omit(spotify[,c('track_name','track_artist','energy','track_popularity','key','valence')])

# Remove duplicated songs
spotify = distinct(spotify)

# The most used key in all songs
tapply(spotify$key,INDEX=spotify$key,FUN=function(x) length(x))

# Add variable energy_rank to spotify data frame
spotify$energy_rank = factor(ifelse(spotify$energy>=0&spotify$energy<=0.35,'Low Energy',
                                      ifelse(spotify$energy>0.35&spotify$energy<=0.7,'Medium Energy','High Energy')),
                               levels=c('Low Energy','Medium Energy','High Energy'))

head(spotify)

# Get all the info where track_artist is Ed Sheeran
ES = spotify[spotify$track_artist=='Ed Sheeran',]
head(ES)

# Extract the most popular song of Ed
ES[ES$track_popularity==max(ES$track_popularity),c(1,2,4)]

# Get all the info where track_artist is Taylor Swift
TS = spotify[spotify$track_artist=='Taylor Swift',]

# Rank the energy of the song based on the energy value  
TS$energy_rank = ifelse(TS$energy>=0&TS$energy<=0.35,'Low Energy',
                        ifelse(TS$energy>0.35&TS$energy<=0.7,'Medium Energy','High Energy'))

# What is the most popular song of Taylor Swift?
TS[TS$track_popularity==max(TS$track_popularity),c(1,2,4)]


####################### Lyrics Analysis #######################

# Function that helps cleaning the lyrics for analysis

cl = function(name_artist,name_song){
  
  lyrics = genius_lyrics(artist=name_artist,song=name_song)
  clean_lyrics = lapply(lyrics$lyric,FUN=function(x) tolower(x))
  clean_lyrics = gsub('\"','',clean_lyrics)
  clean_lyrics = gsub("n't",'',clean_lyrics)
  clean_lyrics = gsub("'s",' is',clean_lyrics)
  clean_lyrics = gsub("'ve",' have',clean_lyrics)
  clean_lyrics = gsub("'ll",' will',clean_lyrics)
  clean_lyrics = gsub("'re",' are',clean_lyrics)
  clean_lyrics = gsub("'m",' am',clean_lyrics)
  clean_lyrics = gsub('[[:punct:]]','',clean_lyrics)
  
  return(clean_lyrics)
}

# Extract only adjectives and nouns using udpipe package

# Function using udpipe package

ud_model = udpipe_download_model(language = 'english')
model = udpipe_load_model(file='english-ewt-ud-2.5-191206.udpipe')

adjective = function(lyric){
  
  annot = udpipe_annotate(model,lyric)
  x = data.frame(annot)
  adjectives = x[x$upos=='ADJ',6]
  
  return(adjectives)
}


############ TAYLOR SWIFT ################

# Choose the singer
singer_1 = 'Taylor Swift'

# Choose songs
songs_1 = c('Shake It Off','Look What You Made Me Do','22', 'Our Song',
          'Mean','Gorgeous','You Belong With Me','Bad Blood', 'End Game',
          'Wildest Dreams','You Need To Calm Down','We Are Never Ever Getting Back Together',
          'I Forgot That You Existed','Delicate')

song_lyric_1 = c()
for(i in 1:(length(songs_1))){
  tryCatch({new_lyric_1 = cl(singer_1,songs_1[i])
  adj_only_1 = adjective(new_lyric_1)
  song_lyric_1 = c(song_lyric_1,adj_only_1)}, error=function(e){}) # error handling that does nothing
}

song_lyric_1

# Create word cloud
wordcloud(song_lyric_1,min.freq=2,scale=c(5, 0.6),random.order=FALSE,rot.per=0.35,
          colors=brewer.pal(11,'RdBu'))

# Create bar chart to clearly shows the frequency of adjectives used
text_freq_1 = txt_freq(song_lyric_1)

text_freq_1$key = factor(text_freq_1$key,levels=rev(text_freq_1$key))
taylor_chart = barchart(key~freq,data=head(text_freq_1,15),main="Most Used Adjectives
                        in Taylor Swift's songs",xlab='Word Frequency',col='red')


############### ED SHEERAN #################

# Choose the singer
singer_2 = 'Ed Sheeran'

# Choose songs
songs_2 = c('South of the Border','Beautiful People','Thinking out Loud','Photograph',
          'The A Team','Shape of You','Cross Me','Happier','Galway Girl','Nothing On You',
          'Best Part of Me','Perfect Duet','Perfect',"I Don't Care")

song_lyric_2 = c()

for(j in 1:(length(songs_2))){
  tryCatch({new_lyric_2 = cl(singer_2,songs_2[j])
  adj_only_2 = adjective(new_lyric_2)
  song_lyric_2 = c(song_lyric_2,adj_only_2)}, error=function(e){})
}

song_lyric_2

# Create word cloud
wordcloud(song_lyric_2,min.freq=2,scale=c(5, 0.3),random.order=FALSE,rot.per=0.35,
          colors=brewer.pal(11,'RdBu'))


# Create bar chart to clearly shows the frequency of adjectives used
text_freq_2 = txt_freq(song_lyric_2)

text_freq_2$key = factor(text_freq_2$key,levels=rev(text_freq_2$key))
ed_chart = barchart(key~freq,data=head(text_freq_2,15),main="Most Used Adjectives
                    in Ed Sheeran's songs",xlab='Word Frequency',col='orange')

grid.arrange(ed_chart,taylor_chart)


#################### Some Relationship between Variables ####################

# Randomly select 300 data from the grand data Spotify
graph_data = spotify[sample(nrow(spotify),1000), ]

# Graph between energy vs. track_popularity

gg1 = ggplot(graph_data,aes(energy,track_popularity)) + geom_point(size=2) + 
  ggtitle('Relationship Between Energy and Track_popularity') + 
  geom_hline(yintercept=87.5,col='red') + 
  geom_vline(xintercept=c(0.35,1),col='red')

# Graph between valence vs. track_popularity with different energy rank

gg2 = ggplot(graph_data,aes(valence,track_popularity)) + geom_point(aes(col=energy_rank),size=2) + 
  ggtitle('Relationship Between Valence and Track_popularity')

gg3 = ggplot(spotify,aes(valence,fill=energy_rank)) + geom_density(alpha=0.4) +
  ggtitle('Valence of 400 Random Songs Distribution Categorized by Energy Rank')

grid.arrange(gg1,gg2)
gg3

