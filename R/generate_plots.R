
library(ggplot2)

in_dir <- "joe-listings/data"
out_dir <- "joe-listings/plots"
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
library(data.table)
library(ggplot2)
library(lubridate)
library(snakecase)
library(stringr)
library(svglite)
library(readxl)
theme_set(theme_minimal(base_family = "sans"))

dest_file <- tempfile(fileext = ".xlsx")
file_url <- "https://www.aeaweb.org/joe/resultset_xls_output.php?mode=xls_xml&q=eNplj0EKwkAMRe-SdYXShYseQBC8Q5jOxBqdZiCZVkrp3R2REcFd8v5P-H-DC1tmGU9JXCfoN2BB5zMvBL3MMTbwoPWZNKCRU3-r1KiYktT1TrGObDaXY-ja7nhoO2ggKY8sLp7_FJ9mybqi0vjzzNxCAa8pBlKr0DsJHFxcJjSvbhriN6CSXCfJmCSuFcVPXCfDEpwUh1wilGKF5fmdGh3s-wuYbliD"
download.file(file_url, destfile = dest_file, mode = "wb")

my_data <- read_excel(dest_file, sheet = 1)

data_current <- setDT(read_excel(dest_file))

past_data_files <- list.files(in_dir, full.names = T)

past_data <- setDT(do.call(rbind,lapply(past_data_files, read_excel)))
full_data <- rbind(past_data, data_current, fill=T)
full_data[,dt_noyr := yday(Date_Active)]

full_data <- full_data[order(Date_Active)][,min_dt := head(dt_noyr,1), by="joe_issue_ID"]
full_data[,dt_noyr_corr := dt_noyr]
full_data[dt_noyr < min_dt, dt_noyr_corr := 365+dt_noyr]
full_data[,day_of_cycle := dt_noyr_corr - min(dt_noyr_corr)+1, by="joe_issue_ID"]

full_data_cumul <- full_data[,.N, by=c("joe_issue_ID","day_of_cycle")][order(day_of_cycle)][,N_cumul := cumsum(N), by="joe_issue_ID"]
full_data_cumul[,current_yr := joe_issue_ID =="2026-02"]
full_data_cumul[,`Cycle Year` := as.factor(str_split_i(joe_issue_ID, "-",1))]

cutoff <- 100

g <- ggplot() + geom_step(data=full_data_cumul[current_yr==0 & day_of_cycle < cutoff ], aes(x=day_of_cycle, y=N_cumul, col=`Cycle Year`),alpha=0.5, linetype=2) + 
  geom_step(data=full_data_cumul[current_yr==1 & day_of_cycle < cutoff ], aes(x=day_of_cycle, y=N_cumul, col=`Cycle Year`), alpha=1) + theme_bw() + 
  xlab("Day of cycle") + ylab("Cumulative postings") + scale_color_discrete(name="Cycle Year")+ ggtitle(label="Cumulative postings by day of cycle", subtitle="All position categories")
print(g)  
ggsave(file.path(out_dir, "cumul_postings.svg"),device="svg", height=5,width=8)


full_data_cumul_type <- full_data[,.N, by=c("joe_issue_ID","day_of_cycle","jp_section")][order(day_of_cycle)][,N_cumul := cumsum(N), by=c("joe_issue_ID","jp_section")]
full_data_cumul_type[,`Cycle Year` := as.factor(str_split_i(joe_issue_ID, "-",1))]
full_data_cumul_type[,current_yr := joe_issue_ID =="2026-02"]
cutoff <- 100

sections <- unique(full_data_cumul_type$jp_section)
job_type_files <- lapply(sections, to_any_case, case="snake")
for (section_f in sections){
  g <- ggplot() + geom_step(data=full_data_cumul_type[current_yr==0 & day_of_cycle < cutoff & jp_section==section_f], aes(x=day_of_cycle, y=N_cumul, col=as.factor(`Cycle Year`)),alpha=0.5, linetype=2) + 
    geom_step(data=full_data_cumul_type[current_yr==1 & day_of_cycle < cutoff & jp_section ==section_f], aes(x=day_of_cycle, y=N_cumul, col=as.factor(`Cycle Year`)), alpha=1) + theme_bw() + 
    xlab("Day of cycle") + ylab("Cumulative postings") + scale_color_discrete(name="Cycle Year")+ ggtitle(label="Cumulative postings by day of cycle", subtitle=section_f)
  
  print(g)  
  ggsave(file.path(out_dir, paste("cumul_", to_any_case(section_f, case = "snake"),".svg", sep='')),device="svg",height=5,width=8)
}


writeLines(format(Sys.time(), "%Y-%m-%d %H:%M %Z"),
           file.path(out_dir, "last_updated.txt"))
