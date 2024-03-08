obtain_demography = function( country, ten_year_brackets = FALSE ){
  
  if ( ten_year_brackets ){
    if ( country == "Australia" ){
      #Source:  
      #https://www.abs.gov.au/statistics/people/population/regional-population-age-and-sex/2021#data-download
      
      #For 5 year age brackets, 0-4yr, 5-9yr, ..., 80-84yr, 85+yr
      agegroup_sizes <- c(1509959, 
                          1616774,
                          1623892,
                          1479632,	
                          1623384,	
                          1822031,	
                          1899620,	
                          1867387,	
                          1654500,	
                          1650035,	
                          1611554,	
                          1550507,	
                          1465025,	
                          1280143,
                          1146773,	
                          807195,	
                          545408,	
                          534260 )
      
      agegroup_sizes <- c( "0-09yr"= sum( agegroup_sizes[ 1:2 ] ),
                           "10-19yr"= sum( agegroup_sizes[ 3:4 ] ),
                           "20-29yr"= sum( agegroup_sizes[ 5:6 ] ),
                           "30-39yr"= sum( agegroup_sizes[ 7:8 ] ),
                           "40-49yr"= sum( agegroup_sizes[ 9:10 ] ),
                           "50-59yr"= sum( agegroup_sizes[ 11:12 ] ),
                           "60-69yr"= sum( agegroup_sizes[ 13:14 ] ),
                           "70-79yr"= sum( agegroup_sizes[ 15:16 ] ),
                           "80+yr"= sum( agegroup_sizes[ 17:18 ] ) )
      
    } else {
      
      
      suppressMessages(
        df_demography <- read_excel( paste0( path_core_functions, "data/eurostat_demography_data_5year_brackets.xlsx" ), sheet=3 )
      )
      
      colnames( df_demography )[1] <- "A"
      
      df_demography %<>% filter( grepl( "Prop", A ) | grepl( "GEO", A ) )
      
      df_demography <- df_demography[ , which( !is.na( df_demography[1,] ) ) ]
      
      colnames( df_demography )[1] <- "agegroup"
      colnames( df_demography )[2:ncol(df_demography)] <- df_demography[1,2:ncol(df_demography)]
      
      df_demography <- df_demography[2:nrow(df_demography),]
      
      colnames( df_demography )[grepl( "Kosovo", colnames( df_demography ) )] <- "Kosovo"
      colnames( df_demography )[grepl( "Germany", colnames( df_demography ) )] <- "Germany"
      colnames( df_demography )[grepl( "European Union", colnames( df_demography ) )] <- "EU"
      
      #5-9 years row is in the wrong place, reorder rows:
      df_demography <- bind_rows( df_demography[1,], df_demography[12,], df_demography[2:11,], df_demography[13:17,] )
      
      df_demography$agegroup <- str_remove(df_demography$agegroup, "Proportion of population aged ")
      df_demography$agegroup <- str_replace(df_demography$agegroup, " years", "yr")
      df_demography$agegroup[ df_demography$agegroup=="80yr and more"] <- "80+yr"
      
      agegroup_distribution <- as.numeric( df_demography[[country]] )
      
      agegroup_distribution <- c( "0-09yr"=sum( agegroup_distribution[ 1:2 ] ),
                                  "10-19yr"=sum( agegroup_distribution[ 3:4 ] ),
                                  "20-29yr"=sum( agegroup_distribution[ 5:6 ] ),
                                  "30-39yr"=sum( agegroup_distribution[ 7:8 ] ),
                                  "40-49yr"=sum( agegroup_distribution[ 9:10 ] ),
                                  "50-59yr"=sum( agegroup_distribution[ 11:12 ] ),
                                  "60-69yr"=sum( agegroup_distribution[ 13:14 ] ),
                                  "70-79yr"=sum( agegroup_distribution[ 15:16 ] ),
                                  "80+yr"= agegroup_distribution[ 17 ] )
      
      agegroup_distribution <- agegroup_distribution/sum(agegroup_distribution)
    }
  }else{
    #df_demography <- read_excel( paste0( path_core_functions, "data/eurostat_demography_data.xlsx" ), sheet=3 )
    df_demography <- read_fst( paste0( path_core_functions, "data/eurostat_demography_data.fst" ) )
    
    colnames( df_demography )[1] <- "A"
    
    df_demography %<>% filter( grepl( "Prop", A ) | grepl( "GEO", A ) )
    
    df_demography <- df_demography[ , which( !is.na( df_demography[1,] ) ) ]
    
    colnames( df_demography )[1] <- "agegroup"
    colnames( df_demography )[2:ncol(df_demography)] <- df_demography[1,2:ncol(df_demography)]
    
    df_demography <- df_demography[2:nrow(df_demography),]
    
    colnames( df_demography )[grepl( "Kosovo", colnames( df_demography ) )] <- "Kosovo"
    colnames( df_demography )[grepl( "Germany", colnames( df_demography ) )] <- "Germany"
    colnames( df_demography )[grepl( "European Union", colnames( df_demography ) )] <- "EU"
    
    #5-9 years row is in the wrong place, reorder rows:
    df_demography <- bind_rows( df_demography[1,], df_demography[8,], df_demography[2:7,], df_demography[9:13,] )
    
    
    df_demography$agegroup <- str_remove(df_demography$agegroup, "Proportion of population aged ")
    df_demography$agegroup <- str_replace(df_demography$agegroup, " years", "yr")
    df_demography$agegroup[ df_demography$agegroup=="80yr and more"] <- "80+yr"
    
    agegroup_distribution <- as.numeric( df_demography[[country]] )
    
    agegroup_distribution <- c( "0-04yr"=agegroup_distribution[ 1 ],
                                "05-09yr"=agegroup_distribution[ 2 ],
                                "10-14yr"=agegroup_distribution[ 3 ],
                                "15-17yr"=agegroup_distribution[ 4 ]*3/5, #"15-17yr" is approximately 3/5*"15-19yr"
                                "18-24yr"=agegroup_distribution[ 4 ]*2/5 + agegroup_distribution[ 5 ], #"19-24yr" is approximately 2/5*"15-19yr"+"20-24yr" 
                                "25-49yr"=agegroup_distribution[ 6 ],
                                "50-59yr"=agegroup_distribution[ 7 ] + agegroup_distribution[ 8 ],
                                "60-69yr"=agegroup_distribution[ 9 ] + agegroup_distribution[ 10 ],
                                "70-79yr"=agegroup_distribution[ 11 ] + agegroup_distribution[ 12 ],
                                "80+yr"=agegroup_distribution[ 13 ] )/100
    
  }
  
  if ( country != "Australia" ){
    #df_population <- read_excel(  paste0( path_core_functions, "data/eurostat_population.xlsx" ), sheet=3 )
    df_population <- read_fst( paste0( path_core_functions, "data/eurostat_population.fst" ) )
    df_population[ 18, 1 ] <- "Germany" 
    
    country_row <- which( country == df_population[ ,1 ] )
    country_population <- as.numeric( df_population[ country_row,24 ] )
    
    agegroup_sizes <- country_population*agegroup_distribution
  }
  
  return( agegroup_sizes )
  
}
