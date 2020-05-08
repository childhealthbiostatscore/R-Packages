library(plotly)

files = list.files("/home/tim/Desktop/CGMs",full.names = T)

l = lapply(files, function(x){
  id = sub(".*_","",tools::file_path_sans_ext(basename(x)))
  df = read.csv(x,stringsAsFactors = F,na.strings = "")
  df = df[,c(2,8)]
  colnames(df) = c("timestamp","sensorglucose")
  df$timestamp = sub("T"," ",df$timestamp)
  df$id = id
  df = df[,c("id","timestamp","sensorglucose")]
  df
})

df = do.call(rbind,l)

df$sensorglucose = suppressWarnings(as.numeric(df$sensorglucose))
df = df[complete.cases(df),]
df$id = as.factor(df$id)
df$timestamp = lubridate::ymd_hms(df$timestamp)
df$agp = lubridate::round_date(df$timestamp,unit = "5 minutes")
df$agp = as.POSIXct(strftime(df$agp,format = "%H:%M"),format = "%H:%M")

summ = df %>% dplyr::group_by(id,agp) %>%
  summarise(sg = mean(sensorglucose,na.rm = T))

summ = as.data.frame(summ)

# Plotly
summ %>% plotly::group_by(id) %>%
  plot_ly(x = ~agp, y = ~sg) %>%
  add_lines(text = id)

# ggplot2
ggplot(summ,aes(x = agp,y = sg)) + geom_smooth(aes(group = id))
