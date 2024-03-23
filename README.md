# cooking

intentando hacer una cosa sencilla

remotes::install_github("timelyportfolio/dataui")
library(dataui)
dui_sparkline(
  data = filter(muestreos11,ESTRATO_RIM=="PALANGRE_CN"),
  components = list(dui_sparklineseries())
)




table_name <-muestreos %>%filter(ESTRATO_RIM=="PALANGRE_CN")%>%
  group_by(ESP_MUE) %>%
  summarise(petal_width = list(P_VIVO))%>%
  reactable(.,
       columns = list(petal_width = colDef(cell = react_sparkline(.,
               height = 60, decimals = 1,
               highlight_points = highlight_points(max="red"),
     labels = c("max"),
             bandline = "innerquartiles",
              bandline_color = "darkgreen"))))

table_name %>%
  save_reactable("table.html")

