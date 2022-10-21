



NSSCTables <- c("AGENCIES"
                ,"ARCHIVE_SAMPLES"
                ,"COARSE_FRAGS"
                ,"CODES"
                ,"COLOURS"
                ,"CourseFragCodes"
                ,"CRACKS"
                ,"CUTANS"
                ,"DISTURBANCES"
                ,"ELEM_GEOMORPHS"
                ,"EXOTICS"
                ,"FABRICS"
                ,"HORIZONS"
                ,"LAB_METHOD_TYPES"
                ,"LAB_METHODS"
                ,"LAB_PROPERTIES"
                ,"LAB_RESULTS"
                ,"LabMethodQA"
                ,"LAND_CAPABILITIES"
                ,"LAND_USES"
                ,"MICRORELIEFS"
                ,"MOTTLES"
                ,"NatSoil_Lab_Method_GroupQA"
                ,"NatSoil_Lab_Method_QA"
                ,"OBS_MNG_PRACS"
                ,"OBSERVATIONS"
                ,"OFFICERS"
                ,"PANS"
                ,"PATT_GEOMORPHS"
                ,"PHS"
                ,"PORES"
                ,"PROJECTS"
                ,"ROCK_OUTCROPS"
                ,"ROOTS"
                ,"SAMPLES"
                ,"SEGREGATIONS"
                ,"SITE_MNG_PRACS"
                ,"SITES"
                ,"STATES"
                ,"STRENGTHS"
                ,"STRUCTURES"
                ,"SUB_MINERAL_COMPS"
                ,"SURF_COARSE_FRAGS"
                ,"SURF_CONDITIONS"
                ,"sysdiagrams"
                ,"VEG_SPECIES"
                ,"VEG_STRATA"
)




sqlFromFile <- function(file){
  require(stringr)
  sql <- readLines(file)
  sql <- unlist(str_split(paste(sql,collapse=" "),";"))
  sql <- sql[grep("^ *$", sql, invert=T)]
  sql
}


dbSendQueries <- function(con,sql){
  dummyfunction <- function(sql,con){
    dbSendQuery(con,sql)
  }
  lapply(sql, dummyfunction, con)
}

doQuery <- function(conn, sql){
  
  q1 <-str_replace_all(sql, 'dbo_', '')
  #print(q1)
  #q2 <-str_replace_all(sql, '"', "\'")
  qry <- dbSendQuery(conn, q1)
  res <- dbFetch(qry)
  dbClearResult(qry)
  return(res)
}







pointsInAust <- function(df, lat_fld, lon_fld){
  
  outdf <- df[df[lon_fld] > 112.9211 &  df[lon_fld] < 153.6386 &  df[lat_fld] > -43.64309 &  df[lat_fld] < -9.229727,  ]
  return(outdf)
  
}






