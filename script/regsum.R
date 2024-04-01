region_summary <- acled %>%
  group_by(ADMIN1_ABR) %>%
  summarize(Average_Fatalities = mean(FATALITIES),
            Total_Events = n())

# Events vs Fatalities
psumevfat <- ggplot(region_summary, aes(x = ADMIN1_ABR)) +
  geom_bar(aes(y = Average_Fatalities, fill = "Average Fatalities"), stat = "identity", 
           alpha = 0.7) +
  geom_line(aes(y = Total_Events * 0.001, color = "Total Events"), lwd = 1.5, group = 1) + 
  geom_point(aes(y = Total_Events * 0.001, color = "Total Events"), size = 3, group = 1) + 
  ylab("Average Fatalities") + xlab("Province (Abbreviation)") +
  scale_y_continuous(sec.axis = sec_axis(~.*100, name = "Total Events (scaled)")) +  
  scale_fill_manual(values = "red", name = "Legend") + 
  scale_color_manual(values = "blue", name = "Legend") +  
  theme_zvis_hgrid(9) +
  theme(legend.position = "top", legend.title = element_blank(), 
        legend.justification = 'right', plot.title = element_blank())

# Avg fatalities/region
pavgfatreg <- ggplot(region_summary, aes(x = ADMIN1_ABR, y = Average_Fatalities)) +
  geom_bar(stat = "identity", fill = "blue") +
  labs(title = "Average Fatalities per Region",
       x = "Region",
       y = "Average Fatalities")
