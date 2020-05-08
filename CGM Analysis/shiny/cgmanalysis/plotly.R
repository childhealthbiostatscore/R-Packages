library(plotly)
library(dplyr)

files = list.files("/home/tim/Desktop/CGMs",full.names = T)

l = lapply(files, function(x){
  id = sub(".*_","",tools::file_path_sans_ext(basename(x)))
  df = read.csv(x,stringsAsFactors = F,na.strings = "")
  df = df[,c(2,8)]
  colnames(df) = c("timestamp","sensorglucose")
  df$timestamp = sub("T"," ",df$timestamp)
  df$id = id
  df = df[,c("id","timestamp","sensorglucose")]
  df = df[complete.cases(df),]
  df
})

df = do.call(rbind,l)

df$sensorglucose = suppressWarnings(as.numeric(df$sensorglucose))
df$timestamp = lubridate::ymd_hms(df$timestamp)
df = df[complete.cases(df),]
df$id = as.factor(df$id)
df$agp = lubridate::round_date(df$timestamp,unit = "5 minutes")
df$agp = as.POSIXct(strftime(df$agp,format = "%H:%M"),format = "%H:%M")

summ = df %>% dplyr::group_by(id,agp) %>%
  summarise(sg = mean(sensorglucose,na.rm = T)) %>% 
  mutate(label = format(agp,format = "%H:%M")) %>% ungroup()

summ = summ %>% group_by(id) %>%
  mutate(id_spline = as.numeric(predict(smooth.spline(sg))$y)) %>% ungroup()

smooth = loess(summ$sg~as.numeric(summ$agp))

# Plotly
# Regular AGP
summ %>% plotly::group_by(id) %>% 
  plot_ly(x = ~agp, y = ~id_spline,
          text=~paste0("ID:",id,"\n","Time:",label,"\n","Mean SG:",round(sg))) %>%
  add_lines(alpha = 0.2,hoverinfo = 'text') %>%
  add_lines(y=smooth$fitted,text=~paste0("Time:",label,"\n","Mean SG:",round(sg)),
            hoverinfo = 'text') %>%
  layout(
    xaxis = list(
    type = 'date',
    tickformat = "%H:%M",
    title = "Time of Day"), 
  yaxis = list(
    title = "Mean Sensor Glusose (mg/dL)",
    range = c(0,400)))
# Radial AGP
summ %>% plotly::plot_ly(type = 'scatterpolar',mode = 'lines') %>%
  add_trace(r = ~agp,theta = ~id_spline)
