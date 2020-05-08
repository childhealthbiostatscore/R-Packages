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
  summarise(sg = mean(sensorglucose,na.rm = T)) %>% 
  mutate(label = format(agp,format = "%H:%M")) %>% ungroup()

# Plotly
p = summ %>% plotly::group_by(id) %>% 
  plot_ly(x = ~agp, y = ~sg,text=~paste(id,label,round(sg))) 
add_lines(p,alpha = 0.1,hoverinfo = 'text')

# ggplot2
p = ggplot(summ,aes(x = agp,y = sg)) + 
  geom_line(aes(group = id),alpha = 0.1) +
  scale_x_datetime(labels = function(x) format(x, format = "%H:%M")) +
  xlab("Time") + ylab("Sensor Glucose (mg/dL)")

ggplotly(p,text)
