# Define server logic to read selected file ----
library(dplyr)
library(openxlsx)
cbdat <- read.xlsx("./longgold_codebook_new.xlsx", sep = ";")

server <- function(input, output) {
  output$introduction <- renderUI({
    includeHTML("introduction.html")
  })

  output$outscale <- renderTable({
    scale_cols <- as.vector(input$info)
    select_info <- as.vector(c(as.character("name_full"),
                               as.character("scale_family"),
                               as.character("language"),
                               as.character(scale_cols))
                               )
    df <- subset(cbdat, 
                 name == input$scales &
                 cbdat$language == input$language,
                 select = select_info)
    if(!is.null(df)){
      df <- df %>% mutate_if(is.numeric, round, 3)
    }
  }, digits = 2,
  spacing = "xs",
  hover = T,
  caption = "Self-rating Scales",
  caption.placement = "top")
  
  output$outtest <- renderTable({
    select_info <- as.vector(c(as.character("name_full"), 
                               as.character("adaptivness"),
                               as.character("threshold_guess_prob"),
                               as.character(input$info)))
    
    bat <- NA
    bat[!is.na(input$bat)] <-  as.character("bat")
    bat_vers <- NA
    bat_vers <- as.character(input$bat)
    bat_vec <- as.vector(subset(cbdat, 
                                name == bat & 
                                cbdat$language == input$language &
                                cbdat$version == bat_vers,
                                select = select_info))
    
    edt <- NA
    edt[!is.na(input$edt)] <-  as.character("edt")
    edt_vers <- NA
    edt_vers <- as.character(input$edt)
    edt_vec <- as.vector(subset(cbdat, 
                                name == edt & 
                                cbdat$language == input$language &
                                cbdat$version == edt_vers,
                                select = select_info))
    
    jaj <- NA
    jaj[!is.na(input$jaj)] <-  as.character("jaj")
    jaj_vers <- NA
    jaj_vers <- as.character(input$jaj)
    jaj_vec <- as.vector(subset(cbdat, 
                                name == jaj & 
                                  cbdat$language == input$language &
                                  cbdat$version == jaj_vers,
                                select = select_info))
    
    mdt <- NA
    mdt[!is.na(input$mdt)] <-  as.character("mdt")
    mdt_vers <- NA
    mdt_vers <- as.character(input$mdt)
    mdt_vec <- as.vector(subset(cbdat, 
                                name == mdt & 
                                  cbdat$language == input$language &
                                  cbdat$version == mdt_vers,
                                select = select_info))
    
    miq <- NA
    miq[!is.na(input$miq)] <-  as.character("miq")
    miq_vers <- NA
    miq_vers <- as.character(input$miq)
    miq_vec <- as.vector(subset(cbdat, 
                                name == miq & 
                                  cbdat$language == input$language &
                                  cbdat$version == miq_vers,
                                select = select_info))
    
    mps <- NA
    mps[!is.na(input$mps)] <-  as.character("mps")
    mps_vers <- NA
    mps_vers <- as.character(input$mps)
    mps_vec <- as.vector(subset(cbdat, 
                                name == mps & 
                                  cbdat$language == input$language &
                                  cbdat$version == mps_vers,
                                select = select_info))
    
    mpt <- NA
    mpt[!is.na(input$mpt)] <-  as.character("mpt")
    mpt_vers <- NA
    mpt_vers <- as.character(input$mpt)
    mpt_vec <- as.vector(subset(cbdat, 
                                name == mpt & 
                                  cbdat$language == input$language &
                                  cbdat$version == mpt_vers,
                                select = select_info))
    
    rat <- NA
    rat[!is.na(input$rat)] <-  as.character("rat")
    rat_vers <- NA
    rat_vers <- as.character(input$rat)
    rat_vec <- as.vector(subset(cbdat, 
                                name == rat & 
                                  cbdat$language == input$language &
                                  cbdat$version == rat_vers,
                                select = select_info))
    
    sss <- NA
    sss[!is.na(input$sss)] <-  as.character("sss")
    sss_vers <- NA
    sss_vers <- as.character(input$sss)
    sss_vec <- as.vector(subset(cbdat, 
                                name == sss & 
                                  cbdat$language == input$language &
                                  cbdat$version == sss_vers,
                                select = select_info))
    
    pit <- NA
    pit[!is.na(input$pit)] <-  as.character("pit")
    pit_vers <- NA
    pit_vers <- as.character(input$pit)
    pit_vec <- as.vector(subset(cbdat, 
                                name == pit & 
                                  cbdat$language == input$language &
                                  cbdat$version == pit_vers,
                                select = select_info))
    
    bds <- NA
    bds[!is.na(input$bds)] <-  as.character("bds")
    bds_vers <- NA
    bds_vers <- as.character(input$bds)
    bds_vec <- as.vector(subset(cbdat, 
                                name == bds & 
                                  cbdat$language == input$language &
                                  cbdat$version == bds_vers,
                                select = select_info))
    
    df_tests <- as.data.frame(rbind(bat_vec, edt_vec, jaj_vec, mdt_vec, miq_vec,
                              mps_vec, mpt_vec, rat_vec, sss_vec, pit_vec,
                              bds_vec))
    if(!is.null(df_tests)){
      df_tests <- df_tests %>% mutate_if(is.numeric, round, 3)
    }
  }, digits = 2,
  spacing = "xs",
  hover = T,
  caption = "Performance Tests",
  caption.placement = "top")
  
  output$download_codebook <- downloadHandler(
    filename = "LongGold_Codebook.csv",
    content = function(file){
      scale_cols <- as.vector(input$info)
      select_info <- as.vector(c(as.character("name_full"),
                                 as.character("scale_family"),
                                 as.character("type"),
                                 as.character("adaptivness"),
                                 as.character("threshold_guess_prob"),
                                 as.character("language"),
                                 as.character("version"),
                                 as.character(scale_cols)
                                 ))
      bat <- NA
      bat[!is.na(input$bat)] <-  as.character("bat")
      
      edt <- NA
      edt[!is.na(input$edt)] <-  as.character("edt")
      
      jaj <- NA
      jaj[!is.na(input$jaj)] <-  as.character("jaj")
      
      mdt <- NA
      mdt[!is.na(input$mdt)] <-  as.character("mdt")
      
      miq <- NA
      miq[!is.na(input$miq)] <-  as.character("miq")
      
      mps <- NA
      mps[!is.na(input$mps)] <-  as.character("mps")
      
      mpt <- NA
      mpt[!is.na(input$mpt)] <-  as.character("mpt")
      
      rat <- NA
      rat[!is.na(input$rat)] <-  as.character("rat")

      sss <- NA
      sss[!is.na(input$sss)] <-  as.character("sss")
      
      pit <- NA
      pit[!is.na(input$pit)] <-  as.character("pit")
      
      bds <- NA
      bds[!is.na(input$bds)] <-  as.character("bds")
      
      test_names <- as.vector(c(bat, edt, jaj, mdt, miq,
                              mps, mpt, rat, sss, pit,
                              bds)) 
                                      
      names <- as.vector(c(input$scales, test_names))
      df <- as.data.frame(NULL)
      df <- subset(cbdat, 
                   cbdat$name == names &
                   cbdat$language == input$language,
                   select = select_info,
                   drop =F)
      
      if(!is.null(df)){
        df <- df %>% mutate_if(is.numeric, round, 3)
      }
      write.table(df, file,
                    sep = ";", 
                    row.names = F,
                    fileEncoding = "UTF-8")
    })
  
  output$CB_custom <- renderUI({
    div(
      downloadButton("download_codebook", "Download LongGold Codebook (CSV)")
    )
    })
    
  output$glossary <- renderUI({
    includeHTML("glossary.html")
  })

}