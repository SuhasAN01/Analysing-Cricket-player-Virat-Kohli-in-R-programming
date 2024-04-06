find_player_id()
fetch_cricinfo()
?find_player_id()


vk=find_player_id("virat kohli")


vk=fetch_player_data(253802,"ODI")

View(vk)


vk100=vk|>filter((Runs>=100))
View(vk100)

vk150=vk|>filter((Runs>=150))
View(vk150)

vk150=vk|>
  filter(Runs>=150)|>
  ggplot(aes(x=Date,y=Runs))+geom_col()

View(vk150)
  
vk00=vk|>filter((Runs==0))
View(vk00)

vk_sum=vk|>
  group_by(Opposition)|>
  summarise(total_val=sum(Runs,na.rm = TRUE))

vk_sum1=vk |>
  group_by(Opposition) |>
  summarise(total_val = sum(Runs, na.rm = TRUE)) |>
  ggplot(aes(x = Opposition, y = total_val,fill=Opposition)) +geom_col()

vk_sum2=vk |>
  group_by(Opposition) |>
  summarise(total_time = sum(Mins, na.rm = TRUE)) |>
  ggplot(aes(x = Opposition, y = total_time,fill=Opposition)) +
  geom_col(width = 0.5,show.legend = FALSE)+
  labs(title="Virat Kohli,Playing time in mins")+
  geom_text(aes(label=total_time))+
  theme(axis.text.x = element_text(angle = 90))

vk_2016 = vk %>%
  filter(between(Date, as.Date("2016-01-01"), as.Date("2018-12-31"))) %>%
  summarise(total_runs = sum(Runs, na.rm = TRUE))

vk_2022 = vk %>%
  filter(between(Date, as.Date("2022-01-01"), as.Date("2023-12-31"))) %>%
  summarise(total_runs = sum(Runs, na.rm = TRUE))


vk_runs_by_period = data.frame(
  Period = c("2022-2023", "2016-2018"),
  Total_Runs = c(vk_2022$total_runs, vk_2016$total_runs)
)

# Create the bar graph
ggplot(vk_runs_by_period, aes(x = Period, y = Total_Runs)) +
  geom_col(fill = "#2CA02C") +  # Green color for the bars
  labs(title = "Virat Kohli's Total Runs in ODIs",
       x = "Period",
       y = "Total Runs") +
  theme_bw()


vk_runs_per_year = vk %>%
  mutate(Year = as.numeric(format(Date, "%Y"))) %>%  # Extract year from Date
  group_by(Year) %>%
  summarise(total_runs = sum(Runs, na.rm = TRUE))

# Create the line graph
ggplot(vk_runs_per_year, aes(x = Year, y = total_runs)) +
  geom_line(color = "#2CA02C", size = 1.5) +  # Green line
  geom_point(color = "#2CA02C", size = 3) +  # Green points
  labs(title = "Virat Kohli's Total Runs in ODIs per Year",
       x = "Year",
       y = "Total Runs") +
  theme_bw()

rs=find_player_id("ben stokes")
rs=fetch_player_data(311158,"test")
View(rs)