# NEEDS ASSMT
# library(rCharts)
# needs12 <-
#   needspread %>%
#   filter(FY == 2012)
# n1 <- nPlot(throughput ~ CMHSP, 
#             group = "Population", 
#             data = needs12, 
#             type = "multiBarChart",
#             #ylab = "Cost per 1,000 beneficiaries",
#             id = "Needs Assessment")
# n1$xAxis(axisLabel = 'CMHSP', width = 62)
# n1$yAxis(axisLabel = 'Select y:', width = 62)
# n1$addControls("y", value = "throughput", 
#                values = c("throughput","in_nonMH","in_req","req_screenout",
#                           "assmt_ffs_HP","inelig_rfrMH","elig_urg_imm","elig_wait"))
# n1$chart(color = c("#85D4E3", "#F4B5BD", "#9C964A", "#CDC08C", "#FAD77B"),
#          forceY = c(0,100))
# n1
# require(base64enc)
# n1$save('charts/NeedsAssess_Bar.html', standalone = TRUE)

# Process
# proc <- nPlot(Change ~ CMHSP, 
#               group = "Phase", 
#               data = needphase, 
#               type = "multiBarChart",
#               id = "Needs Assessment")
# proc$xAxis(axisLabel = 'CMHSP', width = 62)
# proc$yAxis(axisLabel = 'People who reached phase', width = 62)
# proc$addFilters("Population")
# proc$chart(forceY = c(-100,0))
# proc
# proc$save('charts/AccessProcess_Bar.html', standalone = TRUE)
# 
# proc <- nPlot(Running ~ Phase, 
#               group = "CMHSP",
#               data = needphase, 
#               type = 'lineChart') # OR 'lineWithFocusChart'
# proc$yAxis(axisLabel = "% of starting population", width = 62)
# proc$xAxis(axisLabel = "Phase of Access", width = 62)
# proc$addFilters("Population")
# proc
# proc$save('charts/AccessProcess_Line.html', standalone = TRUE)
# 
# needphase$CMHpop <- as.factor(needphase$CMHpop)
# BottleRocket <- c("#A42820", "#5F5647", "#9B110E", "#3F5151", "#4E2A1E", "#550307", "#0C1707")
# 
# needphaseCMH %>%
#   filter(FY == 2013 & PIHPname == "LRP") %>%
#   droplevels %>%
#   ggvis(x = ~Phase, y = ~Running, stroke = ~CMHSP) %>%
#   layer_points(fill = ~CMHSP) %>%
#   layer_lines() %>%
#   add_axis("x", title = "Access Phase") %>%
#   add_axis("y", title = "% of starting population (2013)") %>%
#   scale_numeric("y",domain = c(0,100))
# 
# needphaseCMH %>%
#   filter(FY == 2014 & PIHPname == "MSHN") %>%
#   droplevels %>%
#   ggvis(x = ~Phase, y = ~Running, stroke = ~CMHSP) %>%
#   layer_points(fill = ~CMHSP) %>%
#   layer_lines() %>%
#   add_axis("x", title = "Access Phase") %>%
#   add_axis("y", title = "% of starting population (2014)") %>%
#   scale_numeric("y",domain = c(0,100))

## PARALLEL SETS

library(parsetR)

tst <-
needs %>%
  filter(FY == 2013 & PIHPname == "LRP") %>%
  select(CMHSP:Phase,People) 

  parset(tst, dimensions = colnames(tst)[-4],
         # use some JavaScript to inform parset that Freq has the value
         value = htmlwidgets::JS("function(d){return d.count}"))

## 

library(googleVis) 
needSankey <-  
  gvisSankey(need_network, from="from", to="to", weight="people",
             options=list(width=800, height=600,
                          sankey="{link: {color: { fill: '#d799ae' } },
                               node: { color: { fill: '#a61d4c' },
                               label: { color: '#871b47' } }}"))
plot(needSankey)

    
library(networkD3)

links <- unique(c(levels(need_network$from),
                  levels(need_network$to)))

name_df <- data.frame("name" = links, "id" = 0:18)

n <- need_network %>%
  left_join(name_df, by = c("from" = "name")) %>%
  rename(from_id  = id) %>%
  left_join(name_df, by = c("to" = "name")) %>%
  rename(to_id = id)

sankeyNetwork(Links = n, Nodes = name_df, 
              Source = "from_id",
              Target = "to_id", 
              Value = "people", 
              NodeID = "name",
              width = 700,
              fontSize = 12, nodeWidth = 30)

sankeyNetwork(Links = need_network,
              Nodes = as.data.frame(need_network$from),
              Source = "from",
              Target = "to", 
              Value = "people",
              fontSize = 12, nodeWidth = 30)


URL <- "https://cdn.rawgit.com/christophergandrud/networkD3/master/JSONdata/energy.json"
Energy <- jsonlite::fromJSON(URL)  
  sankeyNetwork(Links = Energy$links, Nodes = Energy$nodes, Source = "source",
                Target = "target", Value = "value", NodeID = "name",
                fontSize = 12, nodeWidth = 30)

## DATA TABLE

library(DT)
needphaseCMH %>%
  datatable(rownames = FALSE,
            colnames = c('FY',
                         'PIHP','CMHSP',
                         'Phase','# of people','% change',
                         '% of Start'),
            caption = 'Access Throughput to CMHSP Services, by Intake Phase',
            options = list(pageLength = 4, lengthMenu = c(4, 20, 100))) %>%
  formatStyle('People',
              background = styleColorBar(needphaseCMH$People, 'lightseagreen'),
              backgroundSize = '100% 90%',
              backgroundRepeat = 'no-repeat',
              backgroundPosition = 'center') %>%
  formatStyle('Change',
              background = styleColorBar(needphaseCMH$Change, 'lightsteelblue'),
              backgroundSize = '100% 90%',
              backgroundRepeat = 'no-repeat',
              backgroundPosition = 'center') %>%
  formatStyle('Running',
              background = styleColorBar(needphaseCMH$Running, 'lightblue'),
              backgroundSize = '100% 90%',
              backgroundRepeat = 'no-repeat',
              backgroundPosition = 'center') 