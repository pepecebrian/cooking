
rm(list = ls())
getwd()
muestreos <- read.csv("C:/Sireno/IEOUPMUEDESTOTJLCEBRIAN.TXT",locale = locale(encoding = "WINDOWS-1252"),  sep=";")
library(readr)
muestreos<- read_delim("C:/Sireno/IEOUPMUEDESTOTJLCEBRIAN.TXT", delim = ";", escape_double = FALSE, locale = locale(encoding = "WINDOWS-1252"), 
                       trim_ws = TRUE)%>%as.data.frame()
tallas<- read_delim("C:/Sireno/IEOUPMUEDESTOTJLCEBRIAN.TXT", 
                                      delim = ";", escape_double = FALSE, locale = locale(encoding = "WINDOWS-1252"), 
                                      trim_ws = TRUE)%>%as.data.frame()
tallas    <- read.csv("C:/Sireno/IEOUPMUETALJLCEBRIAN2.TXT", sep=";")
tallas<- read_delim("C:/Sireno/IEOUPMUETALJLCEBRIAN.TXT", 
                    delim = ";", escape_double = FALSE, locale = locale(encoding = "WINDOWS-1252"), 
                    trim_ws = TRUE)%>%as.data.frame()#pasa a 534


library(stringr)
library(lubridate)
library(dplyr)
library(data.table)
library(janitor)
head (muestreos)
head (tallas)
muestreos$FECHA_MUE<-as.character (muestreos$FECHA_MUE)
muestreos$FECHA_MUE<-str_replace_all(muestreos$FECHA_MUE, "ENE", "JAN")
muestreos$FECHA_MUE<-str_replace_all(muestreos$FECHA_MUE, "ABR", "APR")
muestreos$FECHA_MUE<-str_replace_all(muestreos$FECHA_MUE, "AGO", "AUG")
muestreos$FECHA_MUE<-str_replace_all(muestreos$FECHA_MUE, "DIC", "DEC")
muestreos$FECHA<-dmy(muestreos$FECHA_MUE)
muestreos$MES<-month(muestreos$FECHA)
muestreos$YEAR<-year(muestreos$FECHA)
muestreos$QUARTER<-year(muestreos$FECHA)
muestreos_tallas<-tallas
head(muestreos_tallas,1)
muestreos1<-muestreos_tallas[,c("COD_ID","MES","ESP_CAT" , "TALLA",
                                "JEM_MEDIDOS",  "FECHA", "PAIS", "PUERTO",  "ESTRATO_RIM", "COD_TIPO_MUE")]
muestreos1<-unique(muestreos1)
head (muestreos1)
tabyl
colSums(is.na(muestreos_tallas))
muestreos1<-muestreos_tallas[complete.cases(muestreos_tallas[c("EJEM_MEDIDOS", "P_MUE_VIVO")]),]
INRIM<-muestreos1%>%group_by(PUERTO)%>%
  summarise(
    MUESTREOS=length(unique(COD_ID)), 
    DIAS=length(unique(FECHA_MUE)),
    ESPECIES= uniqueN(ESP_CAT),
    MEDIDOS= sum(EJEM_MEDIDOS),
    med_por_muestr= round(MEDIDOS/MUESTREOS),
    med_por_dia= round(MEDIDOS/DIAS)
    
    )%>%arrange(-MEDIDOS)

head (INRIM)

write.xlsx(INRIM, "resumen RIM 2023.xlsx")

GS<-subset(INRIM, PUERTO %in% c("Finisterre", "Muros", "Santa Eugenia de Ribeira","Marín", "Vigo"
)  )%>%arrange(PUERTO) %>%as.data.frame()
GS

arte<-subset(GS, PUERTO %in% c( "Vigo", "Marín"
) )%>%as.data.frame()

arte
exportar(arte, "RIM_VIGO_2022.xlsx")
tabla<-tabyl(GS,ESTRATO_RIM,DIAS,PUERTO)
install.packages("gt")
install.packages("paletteer")
library(paletteer)
library(gt)
vigo<-as.data.frame(tabla$Vigo)%>%unique()
str(vigo)
vigo[,2:13]
vigo$TOTAL<-sum(vigo[2:12,2:13])
vigo%>%gt()



gt_tbl <- GS %>%
  #head() %>%
  gt() %>%
  tab_header(
    title = md("**Chicken Weight data**"), 
    subtitle = md("remember to *weight* your chickens!")
  ) %>%
  tab_footnote(footnote = "measured in seconds",
               locations = cells_column_labels(YEAR)) %>%
  tab_source_note(source_note = "From ChickenWeight Database") %>%
  tab_style(style = cell_fill(color = "lightblue"),
            locations = cells_body(
              columns = MUESTREOS,
              rows = MUESTREOS > DIAS
            )) %>%
  summary_rows(columns = c(DIAS,MUESTREOS),
               fns = list(
                 avg = ~mean(., na.rm = TRUE),
                 total = ~sum(., na.rm = TRUE),
                 s.d. = ~sd(., na.rm = TRUE))
  ) %>%
  cols_align(align = "right", columns = everything()) %>%
  data_color(
    columns = c(MUESTREOS,DIAS),
    colors = scales::col_numeric(
      palette = paletteer::paletteer_d(
        palette = "ggsci::red_material"
      ) %>% as.character(),
      domain = NULL
    ),
    alpha = 0.8
  )

gt_tbl 
gt_tbl <- 
  gt_tbl %>%
  tab_footnote(
    footnote = "Nuevos.",
    locations = cells_body(columns = ESTRATO_RIM, rows = c(8,13,15,20))
  )

# Show the gt table
gt_tbl


gt_tbl <- 
  gt_tbl %>% 
  tab_row_group(
    label = "Vigo",
    rows =c(1,4,8,10,15,17,20)
  ) %>%
  tab_row_group(
    label = "Ribeira",
    rows = c(3,7,9,11,14,16,19)
  ) %>%
  tab_row_group(
    label = "Fisterra",
    rows = c(5,13,21)
  ) %>%
  tab_row_group(
    label = "Muros",
    rows = c(6,12,18)
  ) %>%
  tab_row_group(
    label = "Marín",
    rows = c(2)
  ) 


gt_tbl%>% tab_spanner(
    label = "FACTORES",
    columns = c(ESTRATO_RIM, YEAR, PUERTO, COD_TIPO_MUE)
  ) %>%
  tab_spanner(
    label = "Measurement",
    columns = c(MUESTREOS, DIAS)
  ) 

  
  gt_tbl%>%   cols_label(
   COD_TIPO_MUE= md( "<br>COD_TIPO_MUE <br>(MT1, MT2A)") ,
  
    DIAS = md("<br>DIAS,<br>days")
  )

# Show the gt table
gt_tbl$PUERTO<-NULL







# formatable --------------------------------------------------------------

install.packages(("formattable"))
library(formattable)
prevalence<-GS
formattable(prevalence, align = c("l", rep("l", NCOL(prevalence) - 1)))
prevalence[, "MUESTREOS"] = prevalence[, "MUESTREOS"] / 100

#Simple column formatting
formattable(prevalence,
            align = c("l",rep("r", NCOL(prevalence) - 1)),
            list(`Indicator Name` = formatter("span", style = ~ style(color = "grey", font.weight = "bold")),
                 `PUERTO` = color_bar("lightblue"),
                 `MUESTREOS` = percent))

#More customized column formatting

formattable(prevalence,
        align = c("l",rep("r", NCOL(prevalence) - 3)),
        list(`DIAS` = formatter("span", style = ~ style(color = "blue", font.weight = "bold")),
             `YEAR` = color_bar("lightblue"),
            # `ESTRATO_RIM` = color_bar("lightslateblue") ,
             `ESTRATO_RIM` = color_bar(ifelse( prevalence$PUERTO== "Vigo", "lightblue",
                                       ifelse( prevalence$PUERTO== "Marín", "#E0EEEE", 
                                      ifelse( prevalence$PUERTO== "Santa Eugenia de Ribeira", "cyan",
                                              ifelse( prevalence$PUERTO== "Muros", "cornsilk",          
                                               "aquamarine"))))),
              #`RATIO` = percent,style =  ~ style(color = ifelse(RATIO<38.20,"green","red")))))),
              `MUESTREOS` = formatter("span",  x ~ percent(x / 100),
                       style =  ~ style(color = ifelse(MUESTREOS<0.08,"darkorange","darkorchid")))))


formattable(prevalence,
            align = c("r",rep("l", NCOL(prevalence) - 1)),
            list(`DIAS` = formatter("span", style = ~ style(color = "steelblue", font.weight = "bold")),
                 `YEAR` = color_bar("lightblue"),
                 `ESTRATO_RIM` = color_bar("lightslateblue"),
                 `MUESTREOS2` = formatter("span",
                                      MUESTREOS ~ icontext(ifelse(MUESTREOS < 0.03, "ok", "remove"), ifelse(MUESTREOS< 0.03, "Yes", "No")),
                                      style =  ~ style(color = ifelse(MUESTREOS<0.03, "red", "green")))))


#Formatting areas (ranges of cells)
formattable(prevalence, align = c("r",rep("l", NCOL(prevalence) - 1)),
            list(
              `DIAS` = formatter("span", style = ~ style(color = "steelblue", font.weight = "bold")),
              `YEAR` = color_bar("lightblue")  ,
              `ESTRATO_RIM` = color_bar("slateblue"),
              `MUESTREOS` = percent,
              area(col = 2) ~ color_tile("lightblue",  "blue"),
              area(col = 1) ~ color_tile("lightblue",  "blue"),
              area(col = 3) ~ color_tile("red",  "green")
            ))


#Custom formatters
custom_color_tile<-function (...)
{
  formatter("span",
            style = function(x) style(display = "block",
                                      padding = "0 4px",
                                      `color` = "blue",
                                      `border-radius` = "4px",
                                      `background-color` = csscolor(gradient(as.numeric(x),
                                                                             ...))))
}
formattable(prevalence, align = "c", list(
  `DIAS` = formatter("span", style = ~ style(color = "steelblue",
                                             font.weight = "bold")),
  `YEAR` = color_bar("lightblue")  ,
  `ESTRATO_RIM` = color_bar("slateblue")
  ,color_tile("red"),
  `MUESTREOS` = percent,
  #area(col = 2:9) ~ function(x) percent(x / 100, digits = 0),
  area(col = 5:5) ~ custom_color_tile("#B1CBEB", "#3E7DCC")))



library(glue)

# Define the start and end dates for the data range
start_date <- "2010-06-07"
end_date <- "2010-06-14"

# Create a gt table based on preprocessed
# `sp500` table data
str(sp500)
sp500 %>%
  filter(date >= start_date & date <= end_date) %>%
  select(-adj_close) %>%
  gt() %>%
  tab_header(
    title = md("**S&P 500**"),
    subtitle = glue("{start_date} to {end_date}")
  ) %>%
  fmt_date(
    columns = date,
    date_style = 2
  ) %>%
  fmt_currency(
    columns = c(open, high, low, close),
    currency = "EUR"
  ) %>%
  fmt_number(
    columns = volume,
    suffixing = TRUE
  )


head (muestreos1)
str(muestreos1)
muestreos11<-muestreos1%>%head()%>%gt()
muestreos11%>%
  fmt_date(
    columns =FECHA,
    date_style = 4
  ) 


?fmt_currency
library(DT)
INRIM<-arrange(INRIM, PUERTO)
datatable(INRIM)
DT::datatable(head(INRIM), editable = 'cell')

DT::datatable(INRIM, editable = list(
  target = 'row', disable = list(columns = c(1, 3, 4))
))

datatable(INRIM, filter = 'top', options = list(
  pageLength = 20, autoWidth = TRUE
))
datatable(pareja, filter = 'top', options = list(
  pageLength = 20, autoWidth = TRUE
))
datatable(INRIM, callback = JS('table.page("next").draw(false);'))

remotes::install_github("kcuilla/reactablefmtr")
library(reactable)
library(htmltools)

bar_chart <- function(label, width = "100%", height = "16px", fill = "#15607A", background = "#EEEEEE") {
  bar <- div(style = list(background = fill, width = width, height = height))
  chart <- div(style = list(flexGrow = 1, marginLeft = "8px", background = background), bar)
  div(style = list(display = "flex", alignItems = "center"), label, chart)
}


install.packages("reactablefmtr")
library(reactablefmtr)
install.packages("dataui")
library(dataui)
reactable(
  INRIM,filter = TRUE, 
  defaultColDef = colDef(
    cell = data_bars(INRIM, text_position = "outside-base")
  )
)




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

head (muestreos)

muestreos %>%filter(ESTRATO_RIM=="PALANGRE_CN")%>%
  group_by(ESP_MUE) %>%
  summarise(petal_width = list(P_VIVO)) %>%
  reactable(.,
  columns = list(petal_width = colDef(cell = react_sparkbar(.))))

muestreos11<-muestreos%>%#filter(ESP_MUE %in%
                                 # c("Nephrops norvegicus", "Conger conger",
                                            #   "Raja clavata"))%>%
  group_by(COD_ID,FECHA,MES, BARCO, ESTRATO_RIM,ESP_MUE,PUERTO, ORIGEN)%>%
  summarise(PESO_SP=sum(P_VIVO))

muestreos11<-muestreos11%>%group_by(ESP_MUE,ESTRATO_RIM)%>%
  mutate(mareas=n_distinct(COD_ID))%>%as.data.frame()
head (muestreos11)




  
  
table_name2T<-muestreos11%>% group_by(ESP_MUE,ESTRATO_RIM,PUERTO, color) %>%
  filter(mareas>1 & MES>3)%>%
  summarise(PESO = list(PESO_SP)) %>%
  mutate(flower_cols = case_when(
   # ESP_MUE == "Nephrops norvegicus"   ~ "purple",
   #ESP_MUE == "Conger conger"         ~ "darkgreen",
    ESP_MUE == "Merluccius merluccius" ~ "orange",
    TRUE ~color
  )) %>%select(ESP_MUE, ESTRATO_RIM, PESO,PUERTO, flower_cols)%>%
     reactable(.,filter=TRUE,
           columns = list(flower_cols = colDef(show=FALSE),
       PESO = colDef(cell = react_sparkbar(.,  height = 50,
          fill_color_ref = "colp"))))                                   
         fill_color_ref = "flower_cols"))))







iris_table <- reactable(iris)
save_reactable(iris_table, "iris_table.png")
 table_name2T %>%
     save_reactable("table_name2T.html")
 
 ## Add labels to particular bars
 muestreos11%>% group_by(ESP_MUE,ESTRATO_RIM) %>%arrange(-PESO_SP)%>%
   summarise(PESO = list(PESO_SP)) %>%
   reactable(.,
        columns = list(PESO = colDef(cell = react_sparkbar(.,
              height = 80,  decimals = 1,
              highlight_bars = highlight_bars(rainbow(4)),
              labels = c("all") ))))
          # highlight_bars = highlight_bars(first="blue",last="red"),
                     labels = c("first","last"))))) 

 
 
 
 ## Default sparkline bar chart
 muestreos11%>% group_by(ESP_MUE,ESTRATO_RIM) %>%
   arrange(-PESO_SP)%>%
   summarise(PESO = list(PESO_SP)) %>%
   reactable(.,
      columns = list(PESO = colDef(cell = react_sparkbar(.,  height = 80)))) 

 
 muestreos11%>% group_by(ESP_MUE,ESTRATO_RIM) %>%arrange(-PESO_SP)%>%
   summarise(PESO = list(PESO_SP)) %>%
   reactable(.,
             columns = list(PESO = colDef(cell = react_sparkbar(.,
                                       height = 80,
                               decimals = 1,
                                 statline = "mean")))) 

 ## Combine multiple elements together
 muestreos11%>% group_by(ESP_MUE,ESTRATO_RIM) %>%arrange(-PESO_SP)%>%
   filter(PESO_SP>1)%>%
   summarise(PESO = list(PESO_SP)) %>%
   reactable(.,
             columns = list(PESO = colDef(cell = react_sparkbar(.,
                                    height = 80,
                                       decimals = 0,
                                   statline = "mean",
                                 bandline = "innerquartiles")))) 
 
 
 
 colsp <- c("darkorange1","#FF61CC", "darkslateblue", "violet", "mediumorchid",  "firebrick","chartreuse",  "pink","brown","orange", "goldenrod", "violet",
            "tomato","chocolate", "magenta", "black", "seagreen", "#F8766D", "mediumslateblue", "turquoise", "navy", "red",
            "cadetblue", "#00BA38", "darkorchid", "slategray","#619CFF", "darkgreen", "darkmagenta", "#F8766D", "purple", "royalblue",
            "limegreen", "violetred", "gray96", "sandybrown", "indianred", "lightsalmon", "blue" , "darkseagreen", "cyan", "blue","darkgoldenrod",
            "tan", "cornsilk", "deeppink", "darkblue", "darkorchid", "mediumaquamarine", "Chartreuse", "blue", "limegreen", "cornsilk","forestgreen",
            "darkmagenta","#D39200",  "mediumslateblue","#FF61CC", "darkslateblue", "darkseagreen", "mediumorchid","#619CFF","Chartreuse" ,"#FF61CC",
            "goldenrod","deeppink","firebrick","darkorange","greenyellow","mediumturquoise","thistle",
            "skyblue","springgreen","violetred","sandybrown", "darkturquoise","lightsalmon","lightsalmon",
            "#FF61CC","#BB3099", "darkorange", "goldenrod",  "darkturquoise", "darksalmon",
            "darkslateblue", "darkviolet", "bisque", "burlywood", "antiquewhite",
            "cyan","chartreuse", "darkred", "red",
            "#00AFBB", "#E7B800", "#FC4E07","#BB3099","#EE0099","#0000AC",
            "#FFBE7D", "#59A14F", "#B07AA1", "#E15759", "#A0CBE8",
            "#8CD17D", "#4E79A7","#FC4E07","#BB3099",
            "darkslateblue",  "black")
 
 names(colsp)<-c("Merluccius merluccius","Trachurus spp","Micromesistius poutassou", "Scomber scombrus","Lophius spp", "Ommastrephidae", "Lepidorhombus spp",
                 "Rajidae", "Eledone spp","Triglidae", "Trisopterus spp.","Conger conger", "Molva molva", "Nephrops norvegicus" , "Brama brama",
                 "Helicolenus dactylopterus", "Sparus aurata", "Boops boops", "Sardina pilchardus", "Engraulis encrasicolus", "Scomber colias",
                 "S. canicula", "Octopus vulgaris",   "Phycis blennoides", "Phycis phycis", "Mullus spp","Pagellus acarne", "Scorpaeniformes",
                 "Molva macrophthalma", "Sepia spp", "Squilla mantis", "Loligo spp", "Parepenaeus longirostris", "Stichopus spp" , "Gadus morhua",
                 "Zeus faber", "Glyptocephalus cynoglossus", "Microstomus kitt", "Beryx spp.", "Dicentrarchus labrax", "Mora moro", "Pollachius pollachius",
                 "Galeus melastomus", "Solea solea", "Gadus morhua", "Polyprion americanus", "Pagellus bogaraveo", "Squaliformes", "Penaeus kerathurus","OTH", 
                 "Argyrosomus regius", "Parapenaeus longirostris", "Aphanopus carbo", "Pomatomus saltatrix", "Pagellus erythrinus", "Scyliorhinus canicula",
                 "Pagellus spp", "Lophius budegassa", "Lophius piscatorius",        "Lepidorhombus boscii", "Lepidorhombus whiffiagonis",
                 "Trachurus mediterraneus", "Trachurus picturatus", "Trachurus trachurus", "Trisopterus luscus","Mullus surmuletus",
                 "Chelidonichthys lucerna", "Raja clavata", "Eutrigla gurnardus", "Leucoraja naevus", "Lepidopus caudatus", "Dipturus oxyrinchus",
                 "Trigla lyra", "Raja montagui","Raja miraletus", "Leucoraja fullonica", "Beryx decadactylus",
                 "Beryx splendens", "Illex coindetii","Chelidonichthys cuculus","Chelidonichthys lucerna", "Trisopterus spp",
                 "Argentina sphyraena", "Scophthalmus maximus", "Spondyliosoma cantharus",
                 "Serranus cabrilla", "Labrus bergylta", "Sarda sarda", "Scomberesox saurus saurus",
                 "Diplodus spp", "Belone belone", "Beryx spp", "Pagrus pagrus",
                 "Plectorhinchus mediterraneus", "Bolinus brandaris", "Solea senegalensis", "Pegusa lascaris", "Dicologlossa cuneata", "Microchirus spp",
                 "Galeorhinus galeus", "Raja asterias", "Umbrina canariensis","Dentex spp",
                 "Balistes capriscus", "Maja squinado", "Sarpa salpa",
                 "Cepola macrophthalma", "Citharus linguatula", "Alloteuthis spp", "Trachinus draco")
 
 
 reactable(
   muestreos11,
   columns = list(
     ESP_MUE = colDef(maxWidth = 90),
     ESTRATO_RIM = colDef(maxWidth = 85),
     #cols = colDef(show = FALSE),
     PESO_SP =  colDef(
       style = color_scales(muestreos11)
     )
   )
 )
 
 
 head (muestreos11)

     unique( muestreos11[,c(4,5,8)])%>%filter(ESTRATO_RIM=="PALANGRE_CN")%>%
       reactable(.,
             defaultColDef =
               colDef(
                 cell = data_bars(., fill_color = viridis::mako(5), bar_height = 30)
                   )
       )
     
     
     
names<-as.data.frame(names(colsp))
colsp2<-as.data.frame(colsp)
names3<-cbind(colsp2, names)
names(names3)<-c("color", "ESP_MUE")
names3
muestreos11$color<-NULL
muestreos11<-muestreos11%>%left_join(names3)%>%as.data.frame()
muestreos11<-muestreos11%>%mutate(color=
            ifelse(is.na(muestreos11$color), "cornflowerblue", muestreos11$color))
head (muestreos11)
unique( muestreos11[,c(4,5,8,9,10)])%>%filter(ESTRATO_RIM=="PALANGRE_CN")%>%
  reactable(.,
          columns = list(color = colDef(show=FALSE),
                       PESO_SP = colDef(cell = react_sparkbar(.,
                      height = 80,  fill_color_ref = "oolor"))))











muestreos_tallas<-tallas

muestreos_tallas$FECHA_MUE<-as.character (muestreos_tallas$FECHA_MUE)
muestreos_tallas$FECHA_MUE<-str_replace_all(muestreos_tallas$FECHA_MUE, "ENE", "JAN")
muestreos_tallas$FECHA_MUE<-str_replace_all(muestreos_tallas$FECHA_MUE, "ABR", "APR")
muestreos_tallas$FECHA_MUE<-str_replace_all(muestreos_tallas$FECHA_MUE, "AGO", "AUG")
muestreos_tallas$FECHA_MUE<-str_replace_all(muestreos_tallas$FECHA_MUE, "DIC", "DEC")
muestreos_tallas$FECHA<-dmy(muestreos_tallas$FECHA_MUE)
muestreos_tallas$QUARTER<-quarter(muestreos_tallas$FECHA)
muestreos_tallas$MES<-month(muestreos_tallas$FECHA)
head(muestreos_tallas)
no_validado<-subset(muestreos_tallas, VALIDADO=="N" & PUERTO %in% c("Santa Eugenia de Ribeira", "Muros", "Marín",
                                                                    "Vigo", "Finisterre"))
head (no_validado,10)

tallas<-muestreos_tallas[,c("COD_PUERTO",  "COD_ID", "FECHA_MUE", "FECHA", "MES",
                            "QUARTER","ESTRATO_RIM", "PUERTO","COD_TIPO_MUE", "COD_BARCO",    "BARCO",
                            "COD_ESP_MUE",   "ESP_MUE", "CATEGORIA","COD_ESP_CAT",
                            "ESP_CAT","P_MUE_VIVO","P_VIVO", "TALLA", "EJEM_MEDIDOS", "EJEM_PONDERADOS","SOP",
                            "VALIDADO"
                        )]%>%distinct()
tallas<-tallas[complete.cases(tallas[c("EJEM_MEDIDOS", "P_MUE_VIVO")]),]
#header (tallas)
tail(tallas)
library(Hmisc)
library(data.table)
substring2(tallas$PUERTO, "CILLERO") <- "CELEIRO"
substring2(tallas$PUERTO, "Cillero") <- "Celeiro"
pesos<-tallas%>%
  group_by(COD_TIPO_MUE,
           COD_ID, ESTRATO_RIM,COD_PUERTO, PUERTO,FECHA,QUARTER,
           COD_BARCO,BARCO, COD_ESP_MUE,TAXON=ESP_MUE,CATEGORIA,COD_ESP_CAT, ESPECIE=ESP_CAT,P_VIVO) %>%
  summarise(
    EJEM_MEDIDOS_CAT=sum(EJEM_MEDIDOS),
    MUEST_SP_CAT= sum(SOP)
  )  %>%
  group_by(COD_ID, TAXON,ESPECIE) %>%
  mutate(
    MUEST_SP=sum(MUEST_SP_CAT)# este es el peso muestreado de la especie esa marea, de todas las categor?as
  )  %>%
  group_by(COD_ID, TAXON, CATEGORIA) %>%
  mutate(
    MUEST_CAT=sum(MUEST_SP_CAT)
  ) %>%
  group_by(COD_ID, ESPECIE) %>%
  mutate(
    PESO_SP_CAT=round((P_VIVO*MUEST_SP_CAT)/MUEST_CAT,2)
  )  %>%
  group_by(COD_TIPO_MUE,COD_ID,  ESTRATO_RIM, PUERTO,FECHA,
           BARCO , TAXON,ESPECIE)%>%
  mutate(
    EJEM_MEDIDOS_SP= sum(EJEM_MEDIDOS_CAT),#EJEMPLARES MEDIDOS DE LA SP EN LA MAREA
    PESO_SP=sum(PESO_SP_CAT), ##ESTE PESO DE LA ESPECIE EN LA MAREA
    PESO_SIRENO= sum(P_VIVO))#%>% #PESO MAL PONDERADO DE SIRENO
  

#header (pesos)


pesos1<-pesos[,c("COD_TIPO_MUE","COD_ID", "FECHA","QUARTER", "ESTRATO_RIM", "COD_PUERTO",  "PUERTO","COD_BARCO",
                 "BARCO",  "COD_ESP_MUE", "TAXON", "CATEGORIA", "COD_ESP_CAT",   "ESPECIE",
                 "MUEST_SP_CAT", "PESO_SP_CAT", "MUEST_SP", "PESO_SP", "PESO_SIRENO")]
pesos1<-unique(pesos1)

#header(pesos1)
#pesos1
head (tallas)
tabyl(tallas, ESP_CAT)
tallas1<-distinct(tallas[,c("COD_ID","FECHA","MES", "QUARTER", "CATEGORIA", "ESTRATO_RIM","COD_TIPO_MUE",
                            "ESP_MUE", "ESP_CAT","TALLA", "EJEM_MEDIDOS", "EJEM_PONDERADOS", "VALIDADO")])
head(tallas1)
colnames(tallas1)[colnames(tallas1) %in% c("ESP_MUE", "ESP_CAT")] <- c("TAXON", "ESPECIE")


tallas2<-full_join(pesos1, tallas1)%>%distinct()  %>%
  group_by(COD_ID,TALLA, ESPECIE)%>%
  mutate(
    EJEM_POND_CAT= round((PESO_SP_CAT*EJEM_MEDIDOS/MUEST_SP_CAT),2)
    
  )  %>% group_by(COD_ID,TALLA, ESPECIE)%>%
  mutate(
    EJEM_MED_TALLA=sum(EJEM_MEDIDOS),
    EJEM_POND_TALLA=sum(EJEM_POND_CAT),
    PESO_MUEST_TALLA= sum(MUEST_SP_CAT),
    PESO_DESEM_TALLA = sum (PESO_SP_CAT),
    EJEM_POND_METODOB= round((PESO_DESEM_TALLA*EJEM_MED_TALLA/PESO_MUEST_TALLA),2)
  )  %>%
  group_by( COD_ID, ESPECIE)  %>%
  
  mutate(PONDERADOS=ifelse(COD_TIPO_MUE==2, EJEM_POND_METODOB, EJEM_PONDERADOS))%>%
  
  group_by( COD_ID, ESPECIE)  %>%
  mutate(
    EJEM_MED_MAREA=sum(EJEM_MEDIDOS),
    TALLA_MEDIA_MAREA= round (weighted.mean(TALLA, EJEM_POND_TALLA),2),
    TALLA_MEDIA_METb = round (weighted.mean(TALLA, PONDERADOS),2))


tallas2<-tallas2%>%mutate(PUERTO=ifelse(COD_TIPO_MUE %in%  c(4,6), "A BORDO", PUERTO) ); head (tallas2)

table(tallas2$COD_TIPO_MUE)

tallas2$SUPERVISOR<-
  ifelse(tallas2$PUERTO %in% c ("Vigo", "Marín", "Santa Eugenia de Ribeira",
                                "Muros", "Finisterre"),"GS",
         
         ifelse(tallas2$PUERTO %in% c ("A Coruña", "Burela", "Cedeira", "Celeiro"), "GN",
                ifelse(tallas2$PUERTO %in% c ( "Avilés", "Gijón", "Llanes", "Luarca", "San Vicente de la Barquera",
                                               "Santander", "Santoña", "Suances"), "Ac", 
                       ifelse(tallas2$PUERTO %in% c ("A BORDO"), "OAB", "GC"))))


TALLAS<-tallas2[,c("COD_TIPO_MUE", "COD_ID","FECHA","MES", "QUARTER", "ESTRATO_RIM","COD_PUERTO", "PUERTO","COD_BARCO",
                   "BARCO","COD_ESP_MUE", "TAXON", "COD_ESP_CAT",
                   "ESPECIE", "TALLA_MEDIA_MAREA", "EJEM_MED_MAREA","PESO_SIRENO",
                   "PESO_SP", "VALIDADO")]%>% distinct()

as.data.frame(head (TALLAS))

substring2(TALLAS$PUERTO, "Santa Eugenia de Ribeira") <- "Ribeira"
TALLAS<-TALLAS%>%mutate(PUERTO=ifelse(COD_TIPO_MUE %in% c(4), "A BORDO",
                                      ifelse(COD_TIPO_MUE %in% c(6), "A BORDO",            
                                             PUERTO) ))   ; head (TALLAS)
colSums(is.na(TALLAS))
TALLAS<-na.omit(TALLAS, cols= c("PESO_SP", "TALLA_MEDIA_MAREA"))

table(TALLAS$COD_TIPO_MUE)

head (TALLAS)
fwrite(TALLAS, "TALLAS.csv")
getwd()
TALLAS2<-subset(TALLAS, EJEM_MED_MAREA>3  )
head (TALLAS2)
mod <- glm (TALLA_MEDIA_MAREA~ESPECIE*ESTRATO_RIM, data = TALLAS2)
#TALLAS2$TALLA_MEDIA_MAREA <- TALLAS2$TALLA_MEDIA_MAREA^2


TALLAS2<-TALLAS2%>%group_by  (COD_ID) %>%mutate(
  VALID=ifelse(QUARTER>3, "S",
               VALIDADO))%>%
  as.data.frame()
TALLAS2<-subset(TALLAS2,VALID=="S")
uniqueN(TALLAS$COD_ID)
colSums(is.na(TALLAS2))
dim(TALLAS2)
mod <- lm(TALLA_MEDIA_MAREA~ESPECIE*ESTRATO_RIM, data=TALLAS)
summary(mod)
library(performance)
r2(mod)



modlog <- lm(log(TALLA_MEDIA_MAREA)~ESPECIE*ESTRATO_RIM, data=TALLAS2)
gmod<- glm (TALLA_MEDIA_MAREA~ESPECIE*ESTRATO_RIM, data = TALLAS2)
summary(modlog)



library(performance)

r2(mod)
r2(modlog)
r2(gmod)
cooksd <- cooks.distance(mod)

cooksd2<-as.data.frame(cooksd)   #añadimos numero de observacion para cruzarlas
dim(cooksd2)

cooksd2$ObsNumber <- 1:length(cooksd)
TALLAS2$ObsNumber <- 1:length(cooksd)
#dim(TALLAS2)

sp2<-full_join(TALLAS2, cooksd2)%>%distinct()%>%arrange(ESTRATO_RIM, PUERTO, COD_ID)%>%
  arrange((ObsNumber))%>%as.data.frame()
#tail (as.data.frame(sp2))
#sp2<-sp2%>%group_by(ESTRATO_RIM, ESPECIE)%>%mutate(sample_size=length(unique(COD_ID)))%>%arrange(ESTRATO_RIM, PUERTO, COD_ID)%>%
 # as.data.frame()



#head (sp2)
dMean <- sp2 %>%
  group_by(ESPECIE, ESTRATO_RIM) %>%
  summarise(MN = mean(cooksd))%>%arrange(-MN)
#header (dMean)
dMean<-dMean[complete.cases(dMean[c("MN")]),]
sp3<-left_join(sp2, dMean)%>%distinct()%>%arrange(FECHA)
#header(sp3)
#³sapply(sp3, function(y) sum(length(which(is.na(y)))))
sp3<-sp3[complete.cases(sp3[c("MN")]),]

sp3<-sp3%>%group_by(ESTRATO_RIM,ESPECIE)%>%
  mutate(mareas=n_distinct(COD_ID),
         #MAX=1.4*max(cooksd),
         # peso_max= max(PESO_SP),
         # peso_min= min(PESO_SP),
         mareas=length(unique(COD_ID)),
         #cmax=max(cooksd),
         #cmin=min (cooksd),
         tmax= max(TALLA_MEDIA_MAREA),
         tmin=min(TALLA_MEDIA_MAREA)
  )%>%as.data.frame(); head(sp3)
sp3<-na.omit(sp3, cols = "cooksd")

sp3<-sp3%>%group_by(ESTRATO_RIM, ESPECIE)%>%mutate(
  FILTRO=ifelse(any(cooksd>4*MN),
                "OUTLIER", "NO_OUT"))%>%
as.data.frame()

sp3<-sp3%>%mutate(
  VALID=ifelse(sp3$QUARTER>3, "S",
                sp3$VALIDADO))%>%
  as.data.frame()




head (sp3)
table
sp3$PUERTO <- toupper(stri_trans_general(
sp3$PUERTO,    "Latin-ASCII"))
#install.packages("ggbeeswarm")
#library(ggbeeswarm)
library(ggrepel)
AC<-subset(sp3, ESTRATO_RIM %in% c("RAPANTER_AC", "MERLUCER_AC", "ENMALLE_AC",
                                   "PALANGRE_AC", "BACA_AC")& mareas>5 & VALID=="S")# & mareas>2 & FILTRO=="keep")%>%as.data.table()



AC<-subset(sp3, ESTRATO_RIM %in% c("BACA_APN", "BACA_CN", "BETA_CN", "CERCO_CN",
                                   "JURELERA_CN", "LIN_CABALLA", "NASAPULP_CN",
                                   "PALANGRE_CN","PAREJA_CN", "RASCO_CN","TRASMALL_CN",
                                   "VOLANTA_CN")& mareas>6 & VALID=="S") & FILTRO=="keep")%>%as.data.table()

AC<-subset(sp3, ESTRATO_RIM %in% c("CERCO_GC",   "ENMALLE_GC", "TRASMALL_GC","VORACERA_GC", 
                                   "PALANGRE_GC")& mareas>5 & VALID=="S") & FILTRO=="keep"  )%>%as.data.table()

AC<-subset(sp3, FILTRO=="keep")
AC<-subset(sp33,TAXON=="Trachurus spp")
head (AC)

uniq_species = unique(AC$ESPECIE)
setwd("~/2022/QC_2022/2T/KEEP")
library(ggplot2)
library(ggbeeswarm)
library(ggrepel)
getwd()

for (i in uniq_species) {
  
  temp_plot = ggplot(data= subset(AC, ESPECIE == i),aes(col=factor(PUERTO))) + 
    # geom_point( aes(x=TALLA_MEDIA_MAREA, y=cooksd , size=EJEM_MED_MAREA)) +
    geom_quasirandom(aes(colour = PUERTO, size=2,x=TALLA_MEDIA_MAREA, y=cooksd),  
                     method = "smiley")  +
    
    
    geom_hline(show.legend=FALSE, data = subset(AC, ESPECIE == i),
               aes(yintercept =4*MN, linetype= "dashed"),size=1, col="tomato1") +
    
    #geom_smooth( 
     #            aes(x = TALLA_MEDIA_MAREA,y = cooksd),
      #            fullrange=TRUE,col="blue", method=lm)+
  #  geom_smooth( 
   #   aes(x = TALLA_MEDIA_MAREA,y = cooksd),
    #  fullrange=TRUE,col="blue", method=lm,formula=y ~ poly(x,2))+
    
  
    guides(colour = guide_legend(override.aes = list(size = 5)))     +
    guides(scale= "none",size=FALSE,fill=guide_legend(override.aes=list(size=4)))   +
    scale_size(range=c(3,8))  +
    facet_wrap(ESTRATO_RIM~. , scales="free")  +
    labs(title=i,subtitle="Influential Obs by Cooks distance (cooksd>8*mean)",
         caption = "AÑO= 2020") +
    
    #theme_grey(base_size=6)    +  #theme(legend.position = "none") +
    theme(strip.text.x = element_text(size=12, angle=0,face="bold", colour="white"),
          strip.text.y = element_text(size=12, face="bold",colour="white"),
          strip.background = element_rect(colour="white", fill=c( "steelblue")))  +
    theme(plot.title = element_text(hjust=0.5,lineheight=7, face="bold", size=16),
          plot.subtitle = element_text(hjust=0.5,lineheight=10, face="bold.italic", 
                                       size = 14)) +
    scale_colour_manual(values=colp,limits = force)   +
    theme(legend.text = element_text(colour="blue", size = 12, face = "bold"))+
    theme(axis.text=element_text(angle=0, size=12, face="bold")) +
    
    #geom_blank(aes(x = 0.99*tmin)) + 
    #geom_blank(aes(x = 1.05*tmax)) +
    #a  geom_label_repel(show.legend=FALSE,data=subset(sp3, PESO_SP==peso_min & ESP_CAT==i ) ,  aes(x=PESO_SP, y=cooksd,
    #     label = paste(round(PESO_SP,1), "kg", "\n",ESTRATO_RIM, "\n", YEAR, "\n", PUERTO)),
    # vjust=-0.5,  label.size = 0, fill = "white")   +
    geom_label_repel(show.legend=FALSE,data=subset(AC, ESPECIE == i & EJEM_MED_MAREA>0 ),
            aes(fontface="bold",  TALLA_MEDIA_MAREA,cooksd, 
                 label = ifelse(cooksd>4*MN,paste(round(TALLA_MEDIA_MAREA,2), "cm", "\n",
                  FECHA, " ", "\n",EJEM_MED_MAREA, "Ejem"),"")  ,
                         vjust=0, hjust=0.5)) +
    

    #scale_x_sqrt(breaks=scales::pretty_breaks())+
    guides(colour = guide_legend(override.aes = list(size=5,linetype=4)))+
    ggtitle(i)
  
  ggsave(temp_plot, file=paste0("GC_2023_plot_tallas_MEDIAS ", i,".png"), width = 35, height =25, units = "cm")
}

impute_outliers <- function(x, removeNA = TRUE){
  quantiles <- quantile(x, c(0.05, 0.95), na.rm = removeNA)
  x[x<quantiles[1]] <- mean(x, na.rm = removeNA)
  x[x>quantiles[2]] <- median(x, na.rm = removeNA)
  x
}

imputed_data <- impute_outliers(AC$TALLA_MEDIA_MAREA)

par(mfrow = c(1,2))

boxplot(AC$TALLA_MEDIA_MAREA, main = "Presión con outliers")
boxplot(imputed_data, main = "Presión sin outliers")






library(MASS)
sp33<-subset(sp3,FILTRO=="keep") 
sp33<-sp33[,c(1,2,3,4,6,8,10,12,14,15,16,18,21,23,24,27)]%>%unique()%>%as.data.frame()

head (sp33)
sp33<-sp33%>%group_by(ESTRATO_RIM, ESPECIE)%>%mutate(
  mean_size= mean(TALLA_MEDIA_MAREA),
  max_cook=max(cooksd),
  min_cook=min(cooksd))%>%as.data.frame()
str(sp33)
sp33$ESPECIE<-as.factor(sp33$ESPECIE)
sp33$ESTRATO_RIM<-as.factor(sp33$ESTRATO_RIM)
sp33<-sp33%>%tidyr::unite("unidos", "ESPECIE","ESTRATO_RIM",remove = FALSE)
head (sp33,2)
tabyl(sp33, ESPECIE, ESTRATO_RIM)
sp33$ESPECIE<-as.factor(sp33$ESPECIE)
install.packages("ggtech")
library(cowplot)
library(ggbeeswarm)
#create scatterplot for data frame with outliers
head (sp33)
sp33$mean_size<-as.factor(sp33$mean_size)
formula <- TALLA_MEDIA_MAREA ~ ESTRATO_RIM+ESPECIE
uniqSP = unique(sp33$unidos)
for (i in uniqSP) {
  

outliers_plot <-  ggplot(data= subset(sp33, unidos == i)
)+ 

  
  
  geom_quasirandom(aes(colour = PUERTO, size=2,x=TALLA_MEDIA_MAREA, y=cooksd  ),
                   method = "smiley")  +
 # geom_smooth(aes(x = TALLA_MEDIA_MAREA,
     #             y = cooksd)) +
  geom_smooth(aes(x = TALLA_MEDIA_MAREA,
                  y = cooksd), method = lm,fill = "lightgray") +
  geom_hline(show.legend=FALSE, data =subset(sp33, unidos==i ), 
             aes(yintercept =4*MN, linetype= "dashed"),size=2, col="tomato1")   +
 
  #theme(legend.position = "none")+
  ggtitle("With Outliers")  +
 # labs(subtitle = paste(i,"_",j))   +
  scale_color_manual(values=colp, limits=force)   + 
 facet_wrap(ESTRATO_RIM~ESPECIE)   +
  ggthemes:: theme_economist()+
 # theme_bw(6)+
  guides(colour = guide_legend(override.aes = list(size = 5)))     +
  guides(scale= "none",size=FALSE,fill=guide_legend(override.aes=list(size=4)))   +
 
  #expand_limits(y = sp33$max_cook)+
#  ggsci::  scale_color_futurama()+
       geom_label_repel(show.legend=FALSE,data=subset(sp33,unidos==i  &
                 cooksd>4*MN ),
        aes(fontface="bold",  TALLA_MEDIA_MAREA,cooksd, col=PUERTO,
            label = ifelse(cooksd>4.5*MN,
            paste(round(TALLA_MEDIA_MAREA,2), "cm", "\n",
            FECHA, " ", "\n",EJEM_MED_MAREA, "Ejem", " y ",
            PESO_SP, "Kg"),"")), size=3) +

 # stat_regline_equation()+
theme(legend.position = "none")
#ggtitle(i)


no_outliers_plot <- ggplot(data = subset(sp33,unidos==i   & cooksd<4*MN )
                       )  +


  geom_quasirandom(aes(colour = PUERTO, size=2,x=TALLA_MEDIA_MAREA, y=cooksd  ),
                   method = "smiley")  +
  # geom_smooth(aes(x = TALLA_MEDIA_MAREA,
  #             y = cooksd)) +
  geom_smooth(aes(x = TALLA_MEDIA_MAREA,
                  y = cooksd), method = lm,fill = "lightgray")
  geom_hline(show.legend=FALSE, data =subset(sp33, unidos==i ), 
             aes(yintercept =4*MN, linetype= "dashed"),size=2, col="tomato1")   +
  ggthemes:: theme_economist()+
 # stat_regline_equation()+ 
  theme(legend.position = "right")+
  ggtitle("No Outliers")  +
  # labs(subtitle = paste(i,"_",j))   +
  scale_color_manual(values=colp, limits=force)   + 
  facet_wrap(ESTRATO_RIM~ESPECIE)   +

  # theme_bw(6)+
  guides(colour = guide_legend(override.aes = list(size = 5)))     +
  guides(scale= "none",size=FALSE,
         fill=guide_legend(override.aes=list(size=4))) +
geom_blank(aes(y = max_cook))
 # ggsci::  scale_color_futurama()

#GRIDPLOT<- gridExtra::grid.arrange( outliers_plot,no_outliers_plot, ncol=2)
GRIDPLOT= cowplot::plot_grid(outliers_plot,no_outliers_plot, labels=c('', ''), ncol=2)
ggsave(GRIDPLOT, file=paste0(i,"_", "GRIDPLOT_2T_2021_plot_tallas_MEDIAS ", ".png"), width = 35, height =25, units = "cm")
}


n_puertos<-n_distinct(sp33$PUERTO)
palette1 = scales::hue_pal()(n_puertos)

names(palette1) = unique(sp33$PUERTO)







tabyl(sp33, ESPECIE)
outliers_plot
no_outliers_plot
getwd()
#create scatterplot for data frame with no outliers
no_outliers_plot <- ggplot(data = subset(sp4, cooksd<4*MN ) )+
  geom_point(aes(x = TALLA_MEDIA_MAREA,
                 y = cooksd, col=PUERTO), size=3.5) +
  geom_smooth(aes(x =TALLA_MEDIA_MAREA,
                              y = cooksd), method = lm) +
  geom_smooth(aes(x = TALLA_MEDIA_MAREA,
                  y = cooksd)) +
  
  stat_cor(
    aes(x = TALLA_MEDIA_MAREA,
        y = cooksd,
      label = paste(..rr.label.., ..p.label.., sep = "~`,`~")))+ 
 #   label.x = 40)
#  )+

  geom_hline(show.legend=FALSE, data =subset(sp33, cooksd<4*MN), 
                                aes(yintercept =4*MN,
                                    linetype= "dashed"),size=2, col="tomato1")+

theme_bw(10)+#stat_regline_equation()+

#ylim(sp33$min_cook,sp33$max_cook) +
  theme(legend.position = "none")+
  ggtitle("No Outliers")+

  facet_wrap(ESPECIE~ESTRATO_RIM, scales = "free")   +
 geom_blank(aes(y = max_cook))+
  # scale_color_manual(values=colp)
 ggsci::  scale_color_futurama()
no_outliers_plot


#plot the two scatterplots side by side
gridExtra::grid.arrange( outliers_plot,no_outliers_plot)


no_out<-subset(sp33, cooksd<4*MN ); head (no_out)




methods("predict")
dat<- subset(sp3,   ESPECIE=="Scomber scombrus")
head (dat)


fitlm = lm(dat$TALLA_MEDIA_MAREA*dat$ESTRATO_RIM , color=dat$ESTRATO_RIM,data = dat)
dat$predlm = predict(fitlm)

ggplot(dat, aes(x = TALLA_MEDIA_MAREA, y =cooksd, color = ESTRATO_RIM) ) +
  geom_point(size=2) +
  geom_line(aes(y = predlm), size = 1)+
  geom_smooth(aes(x = TALLA_MEDIA_MAREA,
                 y = cooksd), method = lm, col="navy")+
  geom_hline(show.legend=FALSE, data =dat,
             aes(yintercept =4*MN, linetype= "dashed"),size=1, col="tomato")+
  facet_wrap(~ESTRATO_RIM, scales = "free_y")


predslm = predict(fitlm, interval = "confidence")
head(predslm)

datlm = cbind(dat, predslm)
head(datlm)

ggplot(subset(datlm), aes(x = TALLA_MEDIA_MAREA, y =cooksd, color = ESTRATO_RIM) ) +
  geom_point(size=2) +
 # geom_ribbon( aes(ymin = lwr, ymax =upr, fill = ESTRATO_RIM, color = NULL), alpha = .15) +
  geom_line( aes(y = fit), size = 1)+
  geom_hline(show.legend=FALSE, data =dat,
             aes(yintercept =4*MN, linetype= "dashed"),size=1, col="tomato1")+
  facet_wrap(~ESTRATO_RIM, scales = "free_y")+
  geom_smooth(aes(x = TALLA_MEDIA_MAREA,
                  y = cooksd), method = lm, col="navy")

head( seq(min(dat$TALLA_MEDIA_MAREA), max(dat$TALLA_MEDIA_MAREA), by = .1) )


newdat = expand.grid(TALLA_MEDIA_MAREA = seq(min(dat$TALLA_MEDIA_MAREA), max(dat$TALLA_MEDIA_MAREA), by = .1),
                     ESTRATO_RIM= unique(dat$ESTRATO_RIM) )
head (newdat)

newdat$predlm = predict(fitlm, newdata = newdat)

ggplot(dat, aes(x = TALLA_MEDIA_MAREA, y = cooksd, color = ESTRATO_RIM) ) +
  geom_point() +
  geom_line(data = newdat, aes(y = predlm), size = 1)+
  facet_wrap(~ESTRATO_RIM, scales = "free")+
  geom_hline(show.legend=FALSE, data =dat,
             aes(yintercept =4*MN, linetype= "dashed"),size=1, col="navy")

library(nlme)
fitlme = lme(cooksd ~ ESTRATO_RIM + TALLA_MEDIA_MAREA, 
             random = ~1|ESPECIE,
             data = dat)

newdat.lme = data.frame(ESTRATO_RIM= dat$ESTRATO_RIM,
                        cooksd= dat$cooksd,
                       TALLA_MEDIA_MAREA = median(dat$TALLA_MEDIA_MAREA) )
head(newdat.lme)

newdat.lme$predlme = predict(fitlme, newdata = newdat.lme, level = 0)

ggplot(dat, aes(x =  TALLA_MEDIA_MAREA , y = cooksd, color = ESTRATO_RIM) ) +
  geom_rug(sides = "b", size = 1) +
  geom_line(data = newdat.lme, aes(y = predlme), size = 1)+
  facet_wrap(~ESTRATO_RIM, scales = "free")




par(mfrow=c(1, 2))
plot(datos$TALLA_MEDIA_MAREA, datos$cooksd,  main="With Outliers", xlab="speed", ylab="dist", pch="*", col="red", cex=2)
abline(lm(TALLA_MEDIA_MAREA ~ cooksd, data=datos), col="blue", lwd=3, lty=2)

# Plot of original data without outliers. Note the change in slope (angle) of best fit line.
plot(cars1$speed, cars1$dist, xlim=c(0, 28), ylim=c(0, 230), main="Outliers removed \n A much better fit!", xlab="speed", ylab="dist", pch="*", col="red", cex=2)
abline(lm(dist ~ speed, data=cars1), col="blue", lwd=3, lty=2)


colp <- c("A Coruña" = "steelblue", "Santa Eugenia de Ribeira" = "blue", "Cillero"="darkgreen",
          "Vigo" = "orange", "Avilés-Gijón" = "darkblue","Avilés"="red", "Gijón"="#00BFC4","Ribeira"="darkslateblue",
          "Santoña" = "#7CAE00", "Cedeira"="forestgreen", "Finisterre"= "brown",
          "Luarca" = "chartreuse4", "Muros"= "#619CFF", "Celeiro"="darkgreen", "Burela" ="yellowgreen",
          "Marín"= "mediumorchid", "San Vicente de la Barquera"= "tomato", "Isla Cristina" ="steelblue",
          "Punta Umbría" = "slateblue3", "Barbate"= "red3","Santander"= "darkorchid","Puerto de Santa María"="darkorchid2",
          "Cádiz"="Chartreuse2", "Tarifa"= "coral1", "Ayamonte"= "coral3", "Sanlúcar de Barrameda"= "darksalmon",
          "Castletown Bere" = "deeppink3", "A BORDO"= "black", "Llanes"="deeppink", "Rota"= "yellowgreen")




#PESOS!!!!
muestreos_pesos<-muestreos
muestreos_pesos <-fread("D:/Usuarios/jlcebrian/Documents/2022/QC_2021/IEOUPMUEDESTOTJLCEBRIAN.TXT")
head (muestreos_pesos)
muestreos_pesos$FECHA_MUE<-toupper(muestreos_pesos$FECHA_MUE)
muestreos_pesos$FECHA_MUE<-as.character (muestreos_pesos$FECHA_MUE)
muestreos_pesos$FECHA_MUE<-str_replace_all(muestreos_pesos$FECHA_MUE, "ENE", "JAN")
muestreos_pesos$FECHA_MUE<-str_replace_all(muestreos_pesos$FECHA_MUE, "ABR", "APR")
muestreos_pesos$FECHA_MUE<-str_replace_all(muestreos_pesos$FECHA_MUE, "AGO", "AUG")
muestreos_pesos$FECHA_MUE<-str_replace_all(muestreos_pesos$FECHA_MUE, "DIC", "DEC")
muestreos_pesos$FECHA<-dmy(muestreos_pesos$FECHA_MUE)
muestreos_pesos$QUARTER<-quarter(muestreos_pesos$FECHA)
muestreos_pesos$MES<-month(muestreos_pesos$FECHA)
muestreos_pesos$PUERTO <- toupper(stri_trans_general(
  muestreos_pesos$PUERTO,    "Latin-ASCII"))
head(muestreos_pesos)
library(data.table)

muestreos_pesos1<-muestreos_pesos%>%
  group_by(COD_TIPO_MUE,
           COD_ID, ESTRATO_RIM, PUERTO,FECHA,QUARTER,MES,
           BARCO,TAXON=ESP_MUE) %>%
  summarise(
    PESO_TAXON=sum(P_VIVO)
    
  )  %>%as.data.table()

head(muestreos_pesos1)
tabyl(muestreos_pesos1, ESTRATO_RIM)
subset(muestreos_pesos1, COD_ID=="202300049")%>%
  as.data.frame()

muestreos_pesos1<-na.omit(muestreos_pesos1, cols = "P_VIVO")

head(muestreos_pesos1,10)
muestreos_pesos1<-muestreos_pesos1 %>%
  group_by(ESTRATO_RIM, TAXON) %>%arrange(ESTRATO_RIM, TAXON, PESO_TAXON)%>%
  mutate(proportion = sum(PESO_TAXON)) %>%
  mutate(Perc = cumsum(100*proportion/sum(proportion))) %>%
  select(-proportion)%>%
  subset(Perc>5)%>%
  as.data.frame()

colSums(is.na(muestreos_pesos1))
muestreos_pesos1<-na.omit(muestreos_pesos1, cols="PESO_TAXON")
mod_PESOS <- lm(PESO_TAXON~TAXON*ESTRATO_RIM, data=muestreos_pesos1)

cooksd_pesos <- cooks.distance(mod_PESOS)

cooksd2<-as.data.frame(cooksd_pesos)   #añadimos numero de observacion para cruzarlas

rm(dat, muestreos, tallas, tallas1, dat, dMean, pesos, pesos1, sp2, tallas2, temp_plot)
cooksd2$ObsNumber <- 1:length(cooksd_pesos)
muestreos_pesos1$ObsNumber <- 1:length(cooksd_pesos)
#dim(TALLAS)

sp2<-full_join(muestreos_pesos1, cooksd2)%>%distinct()%>%arrange(ESTRATO_RIM, PUERTO, COD_ID)%>%
  arrange((ObsNumber))%>%as.data.table()
#tail (as.data.frame(sp2))
sp2<-sp2%>%group_by(ESTRATO_RIM,TAXON)%>%mutate(sample_size=length(unique(COD_ID)))%>%arrange(ESTRATO_RIM, PUERTO, COD_ID)%>%
  as.data.table()



head (sp2)
dMean <- sp2 %>%
  group_by(TAXON, ESTRATO_RIM) %>%
  summarise(MN = mean(cooksd_pesos))%>%arrange(-MN)
header (dMean)
dMean<-dMean[complete.cases(dMean[c("MN")]),]
sp3<-left_join(sp2, dMean)%>%distinct()%>%arrange(FECHA)
header(sp3)
#³sapply(sp3, function(y) sum(length(which(is.na(y)))))
sp3<-sp3[complete.cases(sp3[c("MN")]),]
sp3<-na.omit(sp3, cols="MN")
sp3<-sp3%>%group_by(ESTRATO_RIM,TAXON)%>%
  mutate(#num_cook=n_distinct(COD_ID),
    #MAX=1.4*max(cooksd),
    peso_max= max(PESO_TAXON),
    peso_min= min(PESO_TAXON),
    mareas=length(unique(COD_ID))
    #cmax=max(cooksd),
    #cmin=min (cooksd),
    #tmax= max(TALLA_MEDIA_MAREA),
    #tmin=min(TALLA_MEDIA_MAREA)
  )%>%as.data.frame(); head (sp3)








head (sp3)

sp3<-sp3%>%group_by(ESTRATO_RIM, TAXON)%>%mutate(
  FILTRO=ifelse(any(cooksd_pesos>20*MN),
                "OUT", "NO_OUT"             ))%>%
  as.data.table()
sp3$PUERTO <- toupper(stri_trans_general(
 sp3$PUERTO,    "Latin-ASCII"))
head (sp3)
subset(sp3, COD_ID=="202300005")
subset(tallas2, COD_ID=="202200005")%>%as.data.frame()
#& FILTRO=="keep"
AC<-subset(sp3, ESTRATO_RIM %in% c("RAPANTER_AC", "MERLUCER_AC", "ENMALLE_AC",
                                   "PALANGRE_AC", "BACA_AC")& mareas>5) & FILTRO=="OUT")%>%as.data.table()



AC<-subset(sp3, ESTRATO_RIM %in% c("BACA_APN", "BACA_CN", "BETA_CN", "CERCO_CN",
                                   "JURELERA_CN", "LIN_CABALLA", "NASAPULP_CN",
                                   "PALANGRE_CN","PAREJA_CN", "RASCO_CN","TRASMALL_CN",
                                   "VOLANTA_CN")& mareas>5) %>%as.data.table()

AC<-subset(sp3, ESTRATO_RIM %in% c("CERCO_GC",   "ENMALLE_GC", "TRASMALL_GC","VORACERA_GC", 
                                   "PALANGRE_GC")& mareas>2)%>%as.data.table()


AC<-subset(sp3, ESTRATO_RIM %in% c("VOLANTA_CN" , "BACA_AC"))
library(ggplot2)
library(ggrepel)
library(ggbeeswarm)
head (AC)
getwd()
setwd("D:/Usuarios/jlcebrian/Documents/2022/QC_2022/PESOS")
uniq_species = unique(AC$TAXON)
for (i in uniq_species) {
  
  temp_plot = ggplot(data= subset(AC, TAXON == i),aes(col=factor(PUERTO))) + 
    # geom_point( aes(x=PESO_TAXON, y=cooksd_pesos , size=PESO_TAXON, 
    #shape=factor(COD_TIPO_MUE))) +
  
    geom_quasirandom(aes(colour = PUERTO, x=PESO_TAXON, y=cooksd_pesos ,
                         size=PESO_TAXON, shape=factor(COD_TIPO_MUE)))  +
    geom_hline(show.legend=FALSE, data =subset(AC, TAXON == i),
               aes(yintercept =4*MN, linetype= "dashed"),size=1, col="tomato1") +
    
    
    # geom_hline(show.legend=FALSE, data = subset(AC, ESPECIE == i),
    #   aes(yintercept =10*(cmax-cmin)/sqrt(mareas), linetype= "dashed"),size=1, col="steelblue") +
    
    guides(colour = guide_legend(override.aes = list(size = 5)))     +
    guides(scale= "none",size=FALSE,fill=guide_legend(override.aes=list(size=4)))   +
    scale_size(range=c(2,6))  +
    facet_wrap(.~ESTRATO_RIM, scales = "free")  +
    labs(title=i,subtitle="Influential Obs by Cooks distance (cooksd>20*mean)",
         caption = "AÑO= 2023") +
    
    #theme_grey(base_size=6)    +  #theme(legend.position = "none") +
    theme(strip.text.x = element_text(size=12, angle=0,face="bold", colour="white"),
          strip.text.y = element_text(size=12, face="bold",colour="white"),
          strip.background = element_rect(colour="white", fill=c( "steelblue")))  +
    theme(plot.title = element_text(hjust=0.5,lineheight=7, face="bold", size=16),
          plot.subtitle = element_text(hjust=0.5,lineheight=10, face="bold.italic", 
                                       size = 14)) +
    scale_colour_manual(values=colp,limits = force)   +
    #theme(legend.text = element_text(colour="blue", size = 12, face = "bold"))+
    theme(axis.text=element_text(angle=0, size=12, face="bold")) +
    #theme(legend.position = "none")+
    #geom_blank(aes(x = 0.99*tmin)) + 
   # geom_blank(aes(x = peso_max)) +
    #a  geom_label_repel(show.legend=FALSE,data=subset(sp3, PESO_SP==peso_min & ESP_CAT==i ) ,  aes(x=PESO_SP, y=cooksd,
    #     label = paste(round(PESO_SP,1), "kg", "\n",ESTRATO_RIM, "\n", YEAR, "\n", PUERTO)),
    # vjust=-0.5,  label.size = 0, fill = "white")   +
    geom_label_repel(show.legend=FALSE,data=subset(AC, TAXON == i),
                     aes(fontface="bold", PESO_TAXON,cooksd_pesos, 
                         label = ifelse(cooksd_pesos>4*MN,paste(round(PESO_TAXON,1),
                       "KG", "\n", FECHA, " ","\n", BARCO),"")  ,
                         vjust=0, hjust=0.5)) +
    
    scale_x_sqrt (breaks=scales::pretty_breaks())  +   
    guides(colour = guide_legend(override.aes = list(size=5,linetype=4)))  
   # ggtitle(AC$ESTRATO_RIM)
  
  ggsave(temp_plot, file=paste0("CM_2023_plot_PESOS_TAXON ", i,".png"), width = 35, height =25, units = "cm")
}
getwd()


iris2<-subset(sp3, TAXON %in% c("Merluccius merluccius", "Micromesistius poutassou",
                                "Scomber scombrus"))

iris2<-sp4

head (iris2)



install.packages("qcc")
library(qcc)
head (sp3)
tabyl(unique(sp3[,c(2,3)] )  , ESTRATO_RIM)%>%arrange(-n)
sp4<-subset(sp3, ESTRATO_RIM=="BACA_CN" & ESPECIE=="Micromesistius poutassou")%>%arrange(FECHA)
qcc(data=sp4$TALLA_MEDIA_MAREA,type="xbar.one", restore.par=FALSE)


sp4%>%clean_names()
library(FleetSegmentation)
data<-sp4
   
list.files()
list.dirs()
dir()
muestreos_pesos1$MES<-month(muestreos_pesos1$FECHA)
tabyl(muestreos_pesos1, ESTRATO_RIM)
muestreos_pesos1<-na.omit(muestreos_pesos1, cols = "PESO_TAXON")
colSums(is.na(muestreos_pesos1))
sp4<-subset(muestreos_pesos1, ESTRATO_RIM=="PALANGRE_CN" & MES>3)
tail(sp4)
#data<-sp4
ap4<-sp4%>%
  rename(
  stock   =  TAXON,
  ship_ID =COD_ID,
  landings=PESO_TAXON
)%>%select( 2,9,10)%>%unique()%>%as.data.frame()
#ap4<-ap4[,c(2,9,10)]%>%unique()%>%as.data.frame()
head (ap4)
#library(FleetSegmentation)
colSums(is.na(ap4))
ap4
catchdata <- catchdata_transformation(data = ap4)
#head (catchdata)
numberclust_table(catchdata = catchdata)
numberclust_plot(catchdata = catchdata)
clustering <- segmentation_clustering(catchdata = catchdata,n_cluster =6)
clustering$ship_ID<-as.factor(clustering$ship_ID)
ap4$ship_ID<-as.factor(ap4$ship_ID)

clustering_stockshares_table(data = ap4,
                             clustering = clustering)
n_cluster<-n_distinct(clustering$cluster)
colours= c("#88cafc","#9fd5fd", "#bbe1fd" ,  "#deeefd" )
names(colours)= rev(levels(n_cluster))
stockshares_table <- clustering_stockshares_table(data = ap4,
                                                  clustering = clustering)
p1<-clustering_stockshares_plot(data = ap4,clustering = clustering )+
  labs(title = sp4$ESTRATO_RIM)+ 
  #scale_fill_manual(values=colours)
  scale_fill_manual(values = rev(c( "#EBF5FD","#A3C0E4", "#5D7AB9",
                                    "#4662AC","#A3C0E4","#274B9F")),#"#274B9F"
                    labels=c("very high (> 75%)","high (50-75%)",
                             "medium (25-50%)","low (10-25%)","minimal (< 10%)"),
                    guide = guide_legend(reverse=TRUE))
  #scale_alpha_continuous(range=c(.5,0.5,0.6,.5))+
  #scale_alpha_manual(values=c(0.5, 0.5, 1,0.1, 0.5, 1,0.1, 0.5, 1))
p1


ap44<-merge(ap4, clustering)
ap44<-ap44%>%group_by(cluster)%>%summarise(size=length(unique(ship_ID)))
head (ap44)
p2<-ggplot(ap44, aes(cluster,size))+
  geom_col(colour="black",fill="#5D7AB9",alpha=0.8)+
  geom_text(aes(label=size), vjust=-0.5, size=4, fontface="bold")+
  labs(y="num Mareas por Cluster",x="cluster", title = " ")+
  scale_x_discrete(labels=labels)  +
  theme_bw()+
  labs(title = sp4$ESTRATO_RIM, y="num Mareas por Cluster", x="Cluster")+
  theme(axis.title.y =  element_text(face="bold",size=10),
        axis.title.x = element_blank())
gridExtra::grid.arrange(p1,p2, ncol=2)
cluster_size_plot(clustering = clustering) +
  scale_alpha_manual(values = c(1, .3), guide = F)
scale_fill_manual(values="red")+
  labs(title = sp4$ESTRATO_RIM, y="num Mareas por Cluster", x="Cluster")# For A)

final_segmentation <- clustering
# Create FS-column
final_segmentation$FS <- NA
# Call clusters by factor level
# and assign fleet segments
# 1) Coastal mixed demersal fishery
final_segmentation$FS[final_segmentation$cluster %in%
                        levels(final_segmentation$cluster)[c(1,7)]] <-
  "CHOPA-DORADA"
# 2) High seas shrimp fishery
final_segmentation$FS[final_segmentation$cluster %in%
                        levels(final_segmentation$cluster)[c(2)]] <-
  "CABALON"
# 3) High seas mixed demersal fishery
final_segmentation$FS[final_segmentation$cluster %in%
                        levels(final_segmentation$cluster)[c(3)]] <-
  "JUREL"
# 4) Coastal saithe fishery
final_segmentation$FS[final_segmentation$cluster %in%
                        levels(final_segmentation$cluster)[c(4)]] <-
  "SARDINELLA"
# 5) Coastal herring fishery
final_segmentation$FS[final_segmentation$cluster %in%
                        levels(final_segmentation$cluster)[c(5)]] <-
  "SARDA"
# 6) Coastal herring fishery
final_segmentation$FS[final_segmentation$cluster %in%
                        levels(final_segmentation$cluster)[c(6)]] <-
  "DENTON"
# 7) Coastal herring fishery
final_segmentation$FS[final_segmentation$cluster %in%
                        levels(final_segmentation$cluster)[c(8)]] <-
  "SARDINA"

# 8) Coastal herring fishery
final_segmentation$FS[final_segmentation$cluster %in%
                        levels(final_segmentation$cluster)[c(9)]] <-
  "BOQUERON"

# 9) Coastal herring fishery
final_segmentation$FS[final_segmentation$cluster %in%
                        levels(final_segmentation$cluster)[c(10)]] <-
  "BOGA"


# check if we forgot to assign any cluster
anyNA(final_segmentation$FS)
## [1] FALSE
# if FALSE, make fleet segment a factor variable
# otherwise check assigning code
final_segmentation$FS <- as.factor(final_segmentation$FS)
# list the segments
levels(final_segmentation$FS)
final_segmentation <- final_segmentation %>%
  left_join(ap4)

sp4$ship_ID<-sp4$COD_ID
sp4$ship_ID<-as.factor(sp4$ship_ID)
final_segmentation <- final_segmentation %>%
  left_join(sp4[,c("ship_ID","ESTRATO_RIM", "PUERTO", "BARCO", "FECHA")])%>%arrange(
    ship_ID, -landings) %>%unique()
#subset(sp4, COD_ID=="202201776")%>%select(1,2,3,4,5,7,8,9)
tail(final_segmentation)

tabla_puerto<-tabyl(unique(final_segmentation[,c(1,2,3,6)]), PUERTO,FS)
parejacluster<-final_segmentation
cercocluster<-final_segmentation
cerco_GC_cluster<-final_segmentation
setwd("D:/Usuarios/jlcebrian/Documents/2022/QC_2022/2T")
fwrite(final_segmentation, "tabla_puerto_CERCO_GC.csv")
palangrecluster<-final_segmentation
trasmallocluster<-final_segmentation
cercocluster<-final_segmentation
volantacluster<-final_segmentation
betacluster<-final_segmentation
bacacluster<-final_segmentation
subset(muestreos_pesos1, COD_ID=="202200332")%>%as.data.frame()


head (palangrecluster)
cluster1T<-rbind(betacluster, trasmallocluster, palangrecluster, volantacluster,
                 parejacluster)
tail(cluster1T)
cluster1T<-cluster1T%>%rename(COD_ID=ship_ID)
sp3$COD_ID<-as.factor(sp3$COD_ID)
cluster1T<-cluster1T%>%left_join(sp3[,c("COD_ID", "ESTRATO_RIM")])%>%
  unique()%>%as.data.frame()
head (cluster1T)
fwrite(cluster1T,"metier_estrato.txt")
install.packages("ggxmean")
remotes::install_github("EvaMaeRey/ggxmean")


library(readxl)
head (muestreos_pesos1)
metier_estrato_1T <- read_excel("2022/metierización_SIRENO/metier_estrato_1T.xlsx", 
                                sheet = "metier_estrato")
as.data.frame(head (metier_estrato_1T))
metier_estrato_1T<-left_join(metier_estrato_1T, muestreos[,c(2,3,14,15,19)])%>%unique()
as.data.frame(head (metier_estrato_1T,10))
subset(muestreos_pesos1, COD_ID=="202200004")
fwrite(metier_estrato_1T,"metier_estrato2.txt")


palangre <- read_excel("2022/metierización_SIRENO/metier_estrato_1T.xlsx", 
                                sheet = "ARRASTRE")%>%as.data.frame()

beta <-subset(read_excel("~/2022/metierización_SIRENO/metier_estrato_1T.xlsx", 
                       sheet = "metier_estrato"), ESTRATO_RIM=="BETA_CN")%>%as.data.frame()
beta<-beta[,c(1:7)]
head (beta) 


pareja <-subset(read_excel("~/2022/metierización_SIRENO/metier_estrato_1T.xlsx", 
                         sheet = "metier_estrato"), ESTRATO_RIM=="PALANGRE_CN")%>%as.data.frame()

pareja<-parejacluster[,c(1:6,9)]
str(pareja)

str(beta)
pareja<-pareja%>%rename(COD_ID=ship_ID)
pareja$COD_ID<-as.numeric(as.character((pareja$COD_ID)))
head (pareja)

reactable(
 pareja,filter = TRUE, 
  defaultColDef = colDef(
    cell = data_bars(pareja, 
                     fill_color = c("lightblue","royalblue","navy"),
                     fill_gradient = TRUE,                 
                     text_position = "outside-base")
  )
)

reactable(pareja,
          defaultColDef = colDef(
            cell = data_bars(pareja,
                             fill_color = c("lightblue","royalblue","navy"),
                             fill_gradient = TRUE,
                             text_position = "outside-end")))%>%
  add_title("This is a title") %>% 
  add_subtitle("This is a subtitle") %>% 
  add_source("This is a source")


datatable(pareja, filter = 'top', options = list(
  pageLength = 20, autoWidth = TRUE
))%>%
  formatStyle(
    'FS',
    transform = 'rotateX(45deg) rotateY(20deg) rotateZ(30deg)',
    backgroundColor = styleEqual(
      unique(pareja$FS), c('lightblue', 'lightgreen', 'lightpink', "cyan",
                              "violet")
    )
  )%>%
  formatStyle(
    'landings',
    background = styleColorBar(pareja$landings, 'steelblue'),
    backgroundSize = '100% 90%',
    backgroundRepeat = 'no-repeat',
    backgroundPosition = 'center'
  ) 

gt_tbl <-pareja%>%#filter(FS !=c(  "Lirio", "Merluza"))%>%
  arrange(cluster) %>%
  #head() %>%
  gt() %>%
  tab_header(
   title = md("**Mareas cluster**"), 
   # title = md(beta$ESTRATO_RIM), 
    subtitle = md("Mareas tecleadas en SIRENO como PAREJA_CN")
  ) %>%
  tab_footnote(footnote = "nombre del cluster",
               locations = cells_column_labels(FS)) %>%
  tab_source_note(source_note = "From SIRENO Database") %>%
  tab_style(style = cell_fill(color = "lightblue"),
            locations = cells_body(
              columns = FS,
              rows = FS=="Trasmallo"
            )) %>%
#  summary_rows(columns = c(DIAS,MUESTREOS),
 #              fns = list(
  #               avg = ~mean(., na.rm = TRUE),
   #              total = ~sum(., na.rm = TRUE),
    #             s.d. = ~sd(., na.rm = TRUE))
  #) %>%
  cols_align(align = "right", columns = everything()) %>%
  data_color(
    columns = c(COD_ID),
    colors = scales::col_numeric(
      palette = paletteer::paletteer_d(
       # palette = "ggsci::blue_material"
        palette = "dichromat::DarkRedtoBlue_18"
      ) %>% as.character(),
      domain = NULL
    ),
    alpha = 0.7
  )

gt_tbl
gt_tbl %>% gtsave(filename = "beta.html")
gt_
tbl %>% gtsave(filename = "pareja.html")
gt_tbl <- 
  gt_tbl %>%
  tab_footnote(
    footnote = "Nuevos.",
    locations = cells_body(columns = ESTRATO_RIM, rows = c(8,13,15,20))
  )

# Show the gt table
gt_tbl



sp4<-subset(sp3, ESTRATO_RIM=="BACA_CN")
arrastre<- read_excel("2022/metierización_SIRENO/metier_estrato_1T.xlsx", 
                       sheet = "ARRASTRE")%>%as.data.frame()
df <- data.frame(x = 1:15,
                   y = c (3, 14, 23, 25, 23, 15, 9, 5, 9, 13, 17, 24, 32, 36, 46))

# crear una gráfica de dispersión de la gráfica x vs. y
plot(df$x, df$y, pch = 19 , xlab = ' x ', ylab = ' y ')
lm(TALLA_MEDIA_MAREA~ESPECIE, data=sp4)
plot(sp4$TALLA_MEDIA_MAREA, sp4$cooksd)
fit1 <- lm ( TALLA_MEDIA_MAREA~cooksd, data=sp4)
fit2 <- lm (y ~ poly (x, 2, raw = TRUE ), data = df)
fit3 <- lm (y ~ poly (x, 3, raw = TRUE ), data = df)
fit4 <- lm (y ~ poly (x, 4, raw = TRUE ), data = df)
fit5 <- lm (y ~ poly (x, 5, raw = TRUE ), data = df)

#crea una gráfica de dispersión de x vs. y
plot (sp4$ESPECIE,sp4$TALLA_MEDIA_MAREA, pch = 19, xlab = ' x ', ylab = ' y ')

#define los valores del eje x
x_axis<- seq (1,120, longitud = 10 )
library(graphics)
plot(cars, main = "Stopping Distance versus Speed")
lines(stats::lowess(cars))
#add curva de cada modelo para trazar
línes (sp4$TALLA_MEDIA_MAREA, predict (fit1, data.frame (x = sp4$TALLA_MEDIA_MAREA)), col = ' green ')
lines (x_axis, predict (fit2, data. frame (x = x_axis)), col = ' red ')
lines (x_axis, predict (fit3, data. frame (x = x_axis)), col = ' purple ')
líneas (eje_x, predecir (ajuste4, marco de datos (x = eje_x)), col = ' azul ')
líneas (eje_x, predecir (ajuste5, marco de datos (x = eje_x)), col = ' naranja ')




list.files(pattern=".xlsx") # use the pattern argument to define a common pattern  for import files with regex. Here: .csv
list.files()
# create a list from these files
list.filenames<-list.files(pattern=".xlsx")
list.filenames

list.files()
library(openxlsx)
names <- list('Sheet1' = Enero2020, 'Sheet2' = Febrero2020, 'Sheet3' = Marzo2020,
              'Sheet4' = Abril2020, 'Sheet5'= Mayo2020, 'Sheet6' = Junio2020 ,
              'Sheet7' =Julio2020 , 'Sheet8' = Agosto2020 ,  'Sheet9' = Septiembre2020,
              'Sheet10' = Octubre2020,  'Sheet11' = Noviembre2020, , 'Sheet12' = Diciembre2020 )
write.xlsx(names, file = 'data.xlsx')


list.data<-list()

# create a loop to read in your data
for (i in 1:length(list.filenames))
{
  list.data[[i]]<-read_excel(list.filenames[i])
}


for (i in 1:length(list.filenames))
{
  list.data[[i]]<-read.xlsx(colNames=FALSE,list.filenames[i])
}


# add the names of your data to the list
names(list.data)<-list.filenames

# now you can index one of your tables like this
list.data$deforestation2010.xlsx

# or this
list.data[1]
list.data[6]
seis<-as.data.frame(list.data$`2016_CL.xlsx`); tail (seis)





AC<-subset(sp3, ESTRATO_RIM=="CERCO_CN" & TAXON=="Sardina pilchardus")
outliers_plot <- ggplot(data = AC, aes(x = FECHA, y = PESO_TAXON)) +
  geom_point() +
  geom_smooth(method = lm) +facet_wrap(~TAXON, scales = "free")+
  facet_wrap(~TAXON, scales = "free")+
 # ylim(0, 200) +
  ggtitle("With Outliers")+
  stat_regline_equation()


NO_outliers_plot <- ggplot(data = subset(AC, PESO_TAXON<5000), aes(x = FECHA, y = PESO_TAXON)) +
  geom_point() +
  geom_smooth(method = lm) +facet_wrap(~TAXON, scales = "free")+

 ylim(0, 15000) +
ggtitle("WithOUT Outliers")
gridExtra::grid.arrange( outliers_plot,NO_outliers_plot, ncol=2)
n <- nrow(AC)
plot(cooksd, main = "Cooks Distance for Influential Obs")
abline(h = 4/n, lty = 2, col = "steelblue") # add cutoff line




install.packages("lares")
devtools::install_github("laresbernardo/lares")
library(lares)
lares::mplot_lineal(tag = results$label, 
                    score = results$pred,
                    subtitle = "Salary Regression Model",
                    model_name = "simple_model_02")


install.packages("easystats", repos = "https://easystats.r-universe.dev")
library(easystats)
easystats::install_suggested()
cso_model <- lm(cso_part ~ education_level + mortality_rate + democracy,data = vdem_90)
head (TALLAS2)
mod<-lm(TALLA_MEDIA_MAREA~ESTRATO_RIM+PUERTO+ESPECIE, data=TALLAS2)
performance::check_model(mod)
m <- lm(mpg ~ wt + cyl + gear + disp, data = mtcars)
check_autocorrelation(mod)
check_collinearity(m)
# plot results
if (require("see")) {
  x <- check_collinearity(mod)
  plot(x)
}
if (require("lme4")) {
  data(cbpp)
  set.seed(1)
  cbpp$x <- rnorm(nrow(cbpp))
  cbpp$x2 <- runif(nrow(cbpp))
  model <- glmer(
    cbind(incidence, size - incidence) ~ period + x + x2 + (1 + x | herd),
    data = cbpp,
    family = binomial()
  )
  check_convergence(model)
}
check_distribution(mod)
if (require("lme4") && require("parameters") &&
    require("see") && require("patchwork") && require("randomForest")) {
  data(sleepstudy)
  model <<- lmer(Reaction ~ Days + (Days | Subject), sleepstudy)
  check_distribution(model)
  plot(check_distribution(model))
}


m <- lm(mpg ~ wt + cyl + gear + disp, data = mtcars)
check_model(m)
if (require("lme4")) {
  m <- lmer(Reaction ~ Days + (Days | Subject), sleepstudy)
  check_multimodal 17
  check_model(m, panel = FALSE)
}
if (require("rstanarm")) {
  m <- stan_glm(mpg ~ wt + gear, data = mtcars, chains = 2, iter = 200)
  check_model(mod)
}
## End(Not run)
check_multimodal Check if a distributi


head (sp4)
sp4<-subset(sp3, ESTRATO_RIM=="PAREJA_CN" & ESPECIE=="Merluccius merluccius")

data <- mtcars # Size nrow(data) = 32
# For single variables ------------------------------------------------------
outliers_list <- check_outliers(sp4$TALLA_MEDIA_MAREA) # Find outliers
outliers_list # Show the row index of the outliers
as.numeric(outliers_list) # The object is a binary vector...
filtered_data <- data[!outliers_list, ] # And can be used to filter a dataframe
nrow(filtered_data) # New size, 28 (4 outliers removed)
# Find all observations beyond +/- 2 SD
check_outliers(sp4$TALLA_MEDIA_MAREA, method = "zscore", threshold = 1)
x=sp4$TALLA_MEDIA_MAREA
check_outliers(sp4$TALLA_MEDIA_MAREA, method = "cook")
check_outliers(sp4[,c("TALLA_MEDIA_MAREA")],
               method = "zscore", threshold = NULL)

check_outliers(TALLAS2[,c( "ESPECIE","TALLA_MEDIA_MAREA")])
check_model(modlog)
model <- lm(mpg ~ wt + am + gear + vs * cyl, data = mtcars)

# checking model assumptions
check_heteroscedasticity(model)
y# For dataframes ------------------------------------------------------
check_outliers(TALLAS2) # It works the same way on dataframes
# You can also use multiple methods at once
outliers_list <- check_outliers(sp4, method = c(
  "mahalanobis",
  "iqr",
  "zscore"
))
outliers_list
# Using `as.data.frame()`, we can access more details!
outliers_info <- as.data.frame(outliers_list)
head(outliers_info)
outliers_info$Outlier # Including the probability of being an outlier
# And we can be more stringent in our outliers removal process
filtered_data <- data[outliers_info$Outlier < 0.1, ]
# We can run the function stratified by groups using `{dplyr}` package:
if (require("poorman")) {
  iris %>%
    group_by(Species) %>%
    check_outliers()
}
## Not run:
# You can also run all the methods
check_outliers(sp4, method = "all")
# For statistical models ---------------------------------------------
# select only mpg and disp (continuous)
mt1 <- mtcars[, c(1, 3, 4)]
# create some fake outliers and attach outliers to main df
mt2 <- rbind(mt1, data.frame(
  mpg = c(37, 40), disp = c(300, 400),
  hp = c(110, 120)
))
# fit model with outliers
model <- lm(disp ~ mpg + hp, data = mt2)
outliers_list <- check_outliers(model)
if (require("see")) {
  plot(outliers_list)
}
insight::get_data(model)[outliers_list, ] # Show outliers data
if (require("MASS")) {
  check_outliers(model, method = c("mahalabonis", "mcd"))
}
if (require("ICS")) {
  # This one takes some seconds to finish...
  check_outliers(model, method = "ics")
}
## End(Not run)


data(iris)
lm1 <- lm(Sepal.Length ~ Species, data = iris)
lm2 <- lm(Sepal.Length ~ Species + Petal.Length, data = iris)
lm3 <- lm(Sepal.Length ~ Species * Petal.Length, data = iris)
compare_performance(lm1, lm2, lm3)
compare_performance(lm1, lm2, lm3, rank = TRUE)
if (require("lme4")) {
  m1 <- lm(mpg ~ wt + cyl, data = mtcars)
  m2 <- glm(vs ~ wt + mpg, data = mtcars, family = "binomial")
  m3 <- lmer(Petal.Length ~ Sepal.Length + (1 | Species), data = iris)
  compare_performance(m1, m2, m3)
}

install.packages("lm4")
library(lm4)

fit1 <- lm ( ESPECIE~TALLA_MEDIA_MAREA, data=sp3)
fit2 <- lm (ESPECIE ~ poly (TALLA_MEDIA_MAREA, 2, raw = TRUE ), data = sp3)
fit3 <- lm (y ~ poly (x, 3, raw = TRUE ), data = df)
fit4 <- lm (y ~ poly (x, 4, raw = TRUE ), data = df)
fit5 <- lm (y ~ poly (x, 5, raw = TRUE ), data = df)



colSums(is.na(sp4))

sp4<-subset(sp3, ESTRATO_RIM==c("MERLUCER_AC", "RAPANTER_AC") & ESPECIE==
             c("Lophius budegassa", "Lophius piscatorius"))
head (sp4)
lm1 <- lm(TALLA_MEDIA_MAREA ~ ESTRATO_RIM, data = sp3)
lm2 <- lm(TALLA_MEDIA_MAREA ~ COD_ID+ESPECIE + ESTRATO_RIM, data = sp3)
lm3 <- lm(TALLA_MEDIA_MAREA~ ESPECIE * ESTRATO_RIM, data =sp3)
lm4 <- lmer(TALLA_MEDIA_MAREA~ ESPECIE + ESTRATO_RIM, data = sp4)
compare_performance(lm1, lm2, lm3)
compare_performance(lm1, lm2, lm3,lm4, rank = TRUE)
if (require("lme4")) {
  m1 <- lm(mpg ~ wt + cyl, data = mtcars)
  m2 <- glm(vs ~ wt + mpg, data = mtcars, family = "binomial")
  m3 <- lmer(Petal.Length ~ Sepal.Length + (1 | Species), data = iris)
  compare_performance(lm1, lm2, lm3)
}

plot(compare_performance(lm1, lm2, lm3, rank = TRUE))


model <- lm(mpg ~ wt + cyl, data = mtcars)
mp <- model_performance(lm3)
display(mp)
model_performance(lm2)


x <- rbind(
  matrix(rnorm(100, sd = 0.3), ncol = 2),
  matrix(rnorm(100, mean = 1, sd = 0.3), ncol = 2)
)
colnames(x) <- c("x", "y")
model <- kmeans(x, 2)
model_performance(lm3)


m <- lm(mpg ~ wt + cyl + gear + disp, data = mtcars)
check_model(lm2)+facet_grid(~ESPECIE)


mplot_full(dfr$class2$tag, dfr$class2$scores,
           model_name = "Titanic Survived Model"
)




model <- lm(mpg ~ cyl * wt, data = mtcars)

# step-2
results <- fortify(lm3)
RESULT<-subset(results,ESPECIE=="Lophius budegassa")
# step-3
ggplot(results) +
  geom_point(aes(x = TALLA_MEDIA_MAREA, y =.cooksd, 
                 color = factor(ESTRATO_RIM))) +
  facet_grid(results$ESPECIE)+
  geom_line(aes(x = TALLA_MEDIA_MAREA, y = .fitted, color = factor(ESPECIE)))+
  geom_line(aes(x = TALLA_MEDIA_MAREA, y = .fitted))
remove.packages("rlang")
update.packages()
install.packages("rlang")
library(ggplot2)
library(performance)
install.packages("see", repos = "https://easystats.r-universe.dev")
library(see)

model <- lm(wt ~ mpg, data = mtcars)
check <- check_model(lm2)

plot(check, type = "qq")

library(lares)
data(dfr) # Results for AutoML Predictions
head (dfr)
lapply(dfr, head)


library(MASS)
data(housing)
model <- polr(Sat ~ Infl + Type + Cont, weights = Freq, data = housing)
r2(lm3)






rm(AC, modin, muestreos_tallas, tallas, TALLAS, temp_plot)
rm(tallax)







remove.packages(cuarto)
detach("package:cuarto", unload=TRUE)

devtools:: install_github   ("https://github.com/pepeceb/cuarto.git", force = TRUE)

library(cuarto)
library(stringi)

library(ggrepel)


muestreos_tallas<- read_delim("C:/Sireno/IEOUPMUETALJLCEBRIAN.TXT", 
                    delim = ";", escape_double = FALSE, 
                    locale = locale(encoding = "WINDOWS-1252"), 
                    trim_ws = TRUE)%>%subset(CALADERO_DCF=="Iberian (VIIIc-IX)")%>% as.data.frame()
 
head (muestreos_tallas)


TALLAS<-muesting()
head(TALLAS)
#subset(TALLAS, COD_ID==202300019)

sp3<-modelando(TALLAS)
r2(mod)
head (sp3)
subset(sp3, COD_ID==202302020)
AC<-subset(sp3, ESPECIE== "Merluccius merluccius" & ESTRATO_RIM=="BACA_CN")
head (AC)
cocinando(AC)
library(performance)
#importante
r2(mod)
getwd()

tallax<-subset(muestreos_tallas, ESTRATO_RIM== "PARERJA_CN" & ESP_CAT=="Merluccius merluccius")

head (tallax)
densing(tallax)




??? Run dplyr::last_dplyr_warnings() to see the 179 remaining warnings. 
2: Groups with fewer than two data points have been dropped. 
3: Groups with fewer than two data points have been dropped. 
4: Groups with fewer than two data points have been dropped. 
5: Groups with fewer than two data points have been dropped. 
6: In max(ids, na.rm = TRUE) :
  ningun argumento finito para max; retornando -Inf
7: In max(ids, na.rm = TRUE) :
  ningun argumento finito para max; retornando -Inf
8: In max(ids, na.rm = TRUE) :
  ningun argumento finito para max; retornando -Inf
9: In max(ids, na.rm = TRUE) :
  ningun argumento finito para max; retornando -Inf















cocinando <- function(x) {
  
  colp <- c("A CORUNA" = "steelblue", "SANTA EUGENIA DE RIBEIRA" = "blue","RIBEIRA" = "blue", "CILLERO"="darkgreen",
            "VIGO" = "orange", "AVILES-GIJON" = "darkblue","AVILES"="red", "GIJON"="#00BFC4",
            "SANTONA" = "#7CAE00", "CEDEIRA"="forestgreen", "FINISTERRE"= "darkgoldenrod2",
            "LUARCA" = "chartreuse4", "MUROS"= "#619CFF", "CELEIRO"="darkgreen", 
            "BURELA" ="yellowgreen","SUANCES"="deeppink3",
            "MARIN"= "mediumorchid", "SAN VICENTE DE LA BARQUERA"= "tomato",
            "ISLA CRISTINA" ="steelblue", "LLANES"= "darksalmon",
            "PUNTA UMBRIA" = "slateblue3", "BARBATE"= "red3","SANTANDER"= "red",
            "PUERTO DE SANTA MARIA"="darkorchid2","ROTA"="orange","A BORDO" = "black",
            "CADIZ"="Chartreuse2", "TARIFA"= "coral1", "AYAMONTE"= "coral3",
            "SANLUCAR DE BARRAMEDA"= "darksalmon","PUNTA DEL MORAL"= "red",
            "CASTLETOWN BERE" = "deeppink3", "PUERTO DE LA VEGA"="black", "MUXIA"="tomato2")    
  
  library(beeswarm)
  temp_plot  <-ggplot(data =AC,
                      mapping = aes(y = cooksd, x=TALLA_MEDIA_MAREA, col=factor(PUERTO)))  +
    #geom_point(aes(color = factor(PUERTO2)), position = "jitter",size=3) +  
    
    
    
    geom_quasirandom(aes(colour = PUERTO, size=PESO_SP,x=TALLA_MEDIA_MAREA, y=cooksd  ),
                     method = "smiley")  +
    
    geom_hline(data = AC, aes(yintercept = 8*AC$MN),size=1.5, colour="red")  +
    guides(colour = guide_legend(override.aes = list(size = 3)))     +
    guides(scale= "none",size=FALSE,fill=guide_legend(override.aes=list(size=3))) +
    scale_size(range=c(4,8))  +
    facet_wrap(~ESTRATO_RIM, scales="free")   +
    
    labs(title=AC$ESPECIE,subtitle="Influential Obs by Cooks distance (cooksd>8*mean)",
         caption = "AÑO= 2023") +
    
    theme_minimal()    +  #theme(legend.position = "none") +
    theme(strip.text.x = element_text(size=10, angle=0,face="bold", colour="white"),
          strip.text.y = element_text(size=10, face="bold",colour="white"),
          strip.background = element_rect(colour="white", fill=c( "steelblue")))  +
    
    
    
    theme(plot.title = element_text(hjust=0.5,lineheight=7, face="bold", size=16),
          plot.subtitle = element_text(hjust=0.5,lineheight=10, face="bold.italic", 
                                       size = 14)) +
    scale_colour_manual(values=colp,limits = force)   +
    theme(legend.text = element_text(colour="steelblue", size = 10, face = "bold"))+
    theme( legend.title = element_text(colour="blue", size=10, face="bold"))+
    theme(axis.text=element_text(angle=0, size=12, face="bold")) +
    
    #geom_blank(aes(x = 0.99*t_min)) + 
   # geom_blank(aes(x = 1.05*t_max)) +
   # geom_blank(aes(y = 0.99*MAX)) +
    
    
    
    geom_label_repel(size = 4, max.overlaps = Inf,show.legend=FALSE,data=subset(AC,  EJEM_MED_MAREA>5 ),
                     aes(fontface="bold",  TALLA_MEDIA_MAREA,cooksd, 
                         label = ifelse(cooksd>8*MN,paste(round(TALLA_MEDIA_MAREA,2), "cm", "\n",
                                                          FECHA, " ", "\n",EJEM_MED_MAREA, "Ejemplares"),"")  ,
                         vjust=0, hjust=0.5)) 
    # scale_x_sqrt()
    
    ggsave(temp_plot, file=paste0("2023_plot_TALLAS_MEDIAS ",unique(AC$ESPECIE)," ", ".png"),
           width = 35, height =25, units = "cm")
  
  temp_plot
  
}


sp3<-modin(AC)



dens<-subset(tallas2, ESTRATO_RIM== "PAREJA_CN" & ESPECIE=="Merluccius merluccius")
as.data.frame(head (dens))


dens$PUERTO <- toupper(stri_trans_general(
  dens$PUERTO,    "Latin-ASCII"))



ggplot(dens)  + 
  geom_density(bw=0.8,aes(x = TALLA, weight= PONDERADOS,fill=PUERTO))+
  facet_wrap(~COD_ID)+
  scale_fill_manual(values=colp)
subset(TALLAS, COD_ID=="202302493" & ESTRATO_RIM=="PAREJA_CN")%>%as.data.frame()



ggplot() +
  geom_density(data=dens,bw=0.8,
               aes(x=TALLA, weight=PONDERADOS, fill=PUERTO),alpha=0.6,
               adjust=2)+
  facet_wrap(~COD_ID, scales="free")   +
  scale_fill_manual(values=colp)



ggplot(dens, aes(x = TALLA, fill = PUERTO, weight=PONDERADOS)) +
  geom_density(aes(y = ..count..), stat="density")+
  facet_wrap(~COD_ID)   +
  scale_fill 





ggplot(dens,aes(x=TALLA,fill=PUERTO)) + 
  geom_density(alpha=.5)+
  xlim(c(median(TALLA)-0.001,median(TALLA)+0.001))



modelando <- function(x) {
  mod <- lm(TALLA_MEDIA_MAREA ~ ESTRATO_RIM * ESPECIE, data = TALLAS)
  cooksd <- cooks.distance(mod)
  cooksd2 <- as.data.frame(cooksd)
  cooksd2$ObsNumber <- 1:length(cooksd)
  TALLAS$ObsNumber <- 1:length(cooksd)
  sp2<-full_join(TALLAS, cooksd2)%>%distinct()%>%arrange(ESTRATO_RIM, PUERTO, COD_ID)%>%
    arrange((ObsNumber))%>%as.data.frame()
  #tail (as.data.frame(sp2))
  sp2<-sp2%>%group_by(ESTRATO_RIM, ESPECIE)%>%mutate(sample_size=length(unique(COD_ID)))%>%arrange(ESTRATO_RIM, PUERTO, COD_ID)%>%
    as.data.frame()
 colSums(is.na(sp2)) 
sp2<-sp2[complete.cases(sp2[c("cooksd")]),]
  
  #head (sp2)
  dMean <- sp2 %>%
    group_by(ESPECIE, ESTRATO_RIM) %>%
   dplyr:: summarise(MN = mean(cooksd))%>%arrange(-MN)%>%as.data.frame()
  #header (dMean)
  dMean<-dMean[complete.cases(dMean[c("MN")]),]
  
  dim(sp2)
  dim(dMean)
  sp3<-full_join(sp2, dMean)%>%distinct()%>%arrange(FECHA)
  #header(sp3)
  #³sapply(sp3, function(y) sum(length(which(is.na(y)))))
  sp3<-sp3[complete.cases(sp3[c("MN")]),]
  
  sp3<-sp3%>%group_by(ESTRATO_RIM,ESPECIE)%>%
    mutate(mareas=n_distinct(COD_ID),
           #MAX=1.4*max(cooksd),
           # peso_max= max(PESO_SP),
           # peso_min= min(PESO_SP),
           mareas=length(unique(COD_ID)),
           #cmax=max(cooksd),
           #cmin=min (cooksd),
           tmax= max(TALLA_MEDIA_MAREA),
           tmin=min(TALLA_MEDIA_MAREA)
    )%>%as.data.frame(); head(sp3)
  sp3<-na.omit(sp3, cols = "cooksd")
  
  sp3<-sp3%>%group_by(ESTRATO_RIM, ESPECIE)%>%mutate(
    FILTRO=ifelse(any(cooksd>20*MN),
                  "keep", "delete"))%>%
    as.data.frame()
  
  
  
  #fwrite(OUTLIERS, "OUTLIERS.txt")
  library(openxlsx)
  write.xlsx(OUTLIERS, "OUTLIERS.xlsx")
  rm(sp2)
  
}
