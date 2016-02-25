#===============================================================================
# Name   : Get GMD metadata 
# Author : Heike Sprenger
# Date   : 2014-07-15
# Version: 0.1
# Aim    : Get GMD metadata (Dw, Fw, Sorbitol, AvgAnnotated) 
#          and join them with Phenotyper metadata (phenotyper_results_joined)
#===============================================================================

# 1st version: use INNER JOIN
func_get_gmd_metadata <- function(experiment_id, phenotyper_result_joined){
  gmd_meta <- sqlQuery(dbhandle, paste("set nocount on; DECLARE @taglist uniqueidentifier
                             set @taglist =", "\'", experiment_id, "\'",       
                                       "DECLARE @c TABLE(id uniqueidentifier PRIMARY KEY NOT NULL)
                                       INSERT @c(id)
                                       Select distinct FK_chromatogram from tf.IntensityValue
                                       where FK_TagList = @taglist
                                       --select id from @c
                                       
                                       DECLARE @TotalMstIntensityIntermediate TABLE(chromatogram uniqueidentifier PRIMARY KEY NOT NULL, value float);
                                       INSERT @TotalMstIntensityIntermediate(chromatogram, value)
                                       select i.FK_chromatogram as chromatogram, sum(i.value) as value
                                       from tf.IntensityValue i 
                                       --inner join @c c on i.FK_chromatogram = c.id
                                       where i.FK_TagList = @taglist
                                       group by i.FK_chromatogram     
                                       
                                       DECLARE @MstPerAnalyteCount TABLE(analyte uniqueidentifier PRIMARY KEY NOT NULL, Msts int);
                                       INSERT @MstPerAnalyteCount (analyte, Msts)
                                       select FK_Analyte, count(distinct fk_mst) as Anzahl
                                       from tf.MSTAnnotation a
                                       where FK_TagList = @taglist 
                                       and [Is Quantitative Cluster] = 1
                                       and FK_Analyte is not null
                                       group by FK_Analyte
                                       
                                       DECLARE @MST_AnzahlKorrekt float;
                                       select @MST_AnzahlKorrekt = sum(Msts) from @MstPerAnalyteCount 
                                       where analyte not in (select FK_Analyte from tf.InternalStandards where FK_TagList = @taglist and enabled = 1)
                                       --print @MST_AnzahlKorrekt
                                       
                                       DECLARE @DW TABLE(chromatogram uniqueidentifier PRIMARY KEY NOT NULL, value float);
                                       INSERT @DW(chromatogram, value)
                                       SELECT        v.FK_chromatogram as chromatogram, cast(i.value as float) as value
                                       FROM          @c c
                                       INNER JOIN (select distinct FK_chromatogram, FK_sample from Vial) v ON c.id = v.FK_chromatogram
                                       INNER JOIN tf.SampleInfo i on v.FK_sample = i.FK_Sample
                                       WHERE        (i.attribute = \'TROST/PhenoTyper CSchudoma DWcalc [mg]\')
                                       
                                       DECLARE @FW TABLE(chromatogram uniqueidentifier PRIMARY KEY NOT NULL, value float);
                                       INSERT @FW(chromatogram, value)
                                       SELECT        v.FK_chromatogram as chromatogram, cast(i.value as float) as value
                                       FROM          @c c
                                       INNER JOIN (select distinct FK_chromatogram, FK_sample from Vial) v ON c.id = v.FK_chromatogram
                                       INNER JOIN tf.SampleInfo i on v.FK_sample = i.FK_Sample
                                       WHERE        (i.attribute = \'TROST/PhenoTyper CSchudoma FreshWeight [mg]\')
                                       
                                       DECLARE @IS TABLE(chromatogram uniqueidentifier PRIMARY KEY NOT NULL, value float);
                                       INSERT @IS(chromatogram, value)
                                       select i.FK_chromatogram, SUM(i.value) / (
                                       Select SUM(msts)
                                       from @MstPerAnalyteCount c 
                                       inner join tf.InternalStandards s on c.analyte = s.FK_Analyte
                                       where s.QISTD = 1 
                                       and s.enabled = 1 
                                       and s.FK_TagList = @taglist)
                                       from tf.IntensityValue i 
                                       inner join tf.MSTAnnotation a on i.FK_TagList = a.FK_TagList and i.FK_MST = a.FK_MST
                                       inner join tf.InternalStandards s on i.FK_TagList = s.FK_TagList and a.FK_Analyte = s.FK_Analyte
                                       where a.FK_TagList = @taglist
                                       and s.QISTD = 1
                                       and a.[Is Quantitative Cluster] = 1
                                       group by i.FK_chromatogram
                                       
                                       DECLARE @AvgAnnotated TABLE(chromatogram uniqueidentifier PRIMARY KEY NOT NULL, value float);
                                       INSERT @AvgAnnotated(chromatogram, value)
                                       select i.FK_chromatogram as chromatogram, SUM(i.value) / @MST_AnzahlKorrekt as AvgChr 
                                       from tf.IntensityValue i
                                       inner join tf.MSTAnnotation a on i.FK_MST = a.FK_MST and i.FK_TagList = a.FK_TagList
                                       where a.FK_Analyte not in (Select FK_Analyte from tf.InternalStandards where FK_TagList = @taglist)
                                       AND a.FK_Analyte IS NOT NULL
                                       AND a.FK_TagList = @taglist
                                       group by i.FK_chromatogram
                                       
                                       SELECT c.id as chromatogram, [Is].value as [Is]
                                       , aa.value as [AvgAnnotated], [Dw].value as [Dw], [Fw].value as [Fw]
                                       FROM @c AS c  
                                       INNER JOIN @Dw as dw ON c.id = [Dw].chromatogram
                                       INNER JOIN @Fw as fw ON c.id = [Fw].chromatogram
                                       INNER JOIN @Is as [is] ON c.id = [Is].chromatogram
                                       INNER JOIN @AvgAnnotated as aa ON c.id = aa.chromatogram
                                       ", sep=""))
  # order table
  gmd_meta <- gmd_meta[order(gmd_meta$chromatogram),]
  
  # change class from factor to numeric
  gmd_meta$Is <- as.numeric(as.character(gmd_meta$Is))
  gmd_meta$AvgAnnotated <- as.numeric(as.character(gmd_meta$AvgAnnotated))
  gmd_meta$Dw <- as.numeric(as.character(gmd_meta$Dw))
  gmd_meta$Fw <- as.numeric(as.character(gmd_meta$Fw))
    
  # join metadata from gmd with metadata from phenotyper (phenotyper_result_joined)
  library(plyr)
  gmd_meta_joined <- join(gmd_meta, phenotyper_result_joined, by="chromatogram")
     
  return(gmd_meta_joined)  
  
}
  
# 2nd version: use LEFT OUTER JOIN  
func_get_gmd_metadata_all <- function(experiment_id, phenotyper_result_joined){ 
    gmd_meta <- sqlQuery(dbhandle, paste("set nocount on; DECLARE @taglist uniqueidentifier
                             set @taglist =", "\'", experiment_id, "\'",       
                                         "DECLARE @c TABLE(id uniqueidentifier PRIMARY KEY NOT NULL)
                                         INSERT @c(id)
                                         Select distinct FK_chromatogram from tf.IntensityValue
                                         where FK_TagList = @taglist
                                         --select id from @c
                                         
                                         DECLARE @TotalMstIntensityIntermediate TABLE(chromatogram uniqueidentifier PRIMARY KEY NOT NULL, value float);
                                         INSERT @TotalMstIntensityIntermediate(chromatogram, value)
                                         select i.FK_chromatogram as chromatogram, sum(i.value) as value
                                         from tf.IntensityValue i 
                                         --inner join @c c on i.FK_chromatogram = c.id
                                         where i.FK_TagList = @taglist
                                         group by i.FK_chromatogram     
                                         
                                         DECLARE @MstPerAnalyteCount TABLE(analyte uniqueidentifier PRIMARY KEY NOT NULL, Msts int);
                                         INSERT @MstPerAnalyteCount (analyte, Msts)
                                         select FK_Analyte, count(distinct fk_mst) as Anzahl
                                         from tf.MSTAnnotation a
                                         where FK_TagList = @taglist 
                                         and [Is Quantitative Cluster] = 1
                                         and FK_Analyte is not null
                                         group by FK_Analyte
                                         
                                         DECLARE @MST_AnzahlKorrekt float;
                                         select @MST_AnzahlKorrekt = sum(Msts) from @MstPerAnalyteCount 
                                         where analyte not in (select FK_Analyte from tf.InternalStandards where FK_TagList = @taglist and enabled = 1)
                                         --print @MST_AnzahlKorrekt
                                         
                                         DECLARE @DW TABLE(chromatogram uniqueidentifier PRIMARY KEY NOT NULL, value float);
                                         INSERT @DW(chromatogram, value)
                                         SELECT        v.FK_chromatogram as chromatogram, cast(i.value as float) as value
                                         FROM          @c c
                                         LEFT OUTER JOIN (select distinct FK_chromatogram, FK_sample from Vial) v ON c.id = v.FK_chromatogram
                                         LEFT OUTER JOIN tf.SampleInfo i on v.FK_sample = i.FK_Sample
                                         WHERE        (i.attribute = \'TROST/PhenoTyper CSchudoma DWcalc [mg]\')
                                         
                                         DECLARE @FW TABLE(chromatogram uniqueidentifier PRIMARY KEY NOT NULL, value float);
                                         INSERT @FW(chromatogram, value)
                                         SELECT        v.FK_chromatogram as chromatogram, cast(i.value as float) as value
                                         FROM          @c c
                                         LEFT OUTER JOIN (select distinct FK_chromatogram, FK_sample from Vial) v ON c.id = v.FK_chromatogram
                                         LEFT OUTER JOIN tf.SampleInfo i on v.FK_sample = i.FK_Sample
                                         WHERE        (i.attribute = \'TROST/PhenoTyper CSchudoma FreshWeight [mg]\')
                                         
                                         DECLARE @IS TABLE(chromatogram uniqueidentifier PRIMARY KEY NOT NULL, value float);
                                         INSERT @IS(chromatogram, value)
                                         select i.FK_chromatogram, SUM(i.value) / (
                                         Select SUM(msts)
                                         from @MstPerAnalyteCount c 
                                         LEFT OUTER JOIN tf.InternalStandards s on c.analyte = s.FK_Analyte
                                         where s.QISTD = 1 
                                         and s.enabled = 1 
                                         and s.FK_TagList = @taglist)
                                         from tf.IntensityValue i 
                                         LEFT OUTER JOIN tf.MSTAnnotation a on i.FK_TagList = a.FK_TagList and i.FK_MST = a.FK_MST
                                         LEFT OUTER JOIN tf.InternalStandards s on i.FK_TagList = s.FK_TagList and a.FK_Analyte = s.FK_Analyte
                                         where a.FK_TagList = @taglist
                                         and s.QISTD = 1
                                         and a.[Is Quantitative Cluster] = 1
                                         group by i.FK_chromatogram
                                         
                                         DECLARE @AvgAnnotated TABLE(chromatogram uniqueidentifier PRIMARY KEY NOT NULL, value float);
                                         INSERT @AvgAnnotated(chromatogram, value)
                                         select i.FK_chromatogram as chromatogram, SUM(i.value) / @MST_AnzahlKorrekt as AvgChr 
                                         from tf.IntensityValue i
                                         LEFT OUTER JOIN tf.MSTAnnotation a on i.FK_MST = a.FK_MST and i.FK_TagList = a.FK_TagList
                                         where a.FK_Analyte not in (Select FK_Analyte from tf.InternalStandards where FK_TagList = @taglist)
                                         AND a.FK_Analyte IS NOT NULL
                                         AND a.FK_TagList = @taglist
                                         group by i.FK_chromatogram
                                         
                                         SELECT c.id as chromatogram, [Is].value as [Is]
                                         , aa.value as [AvgAnnotated], [Dw].value as [Dw], [Fw].value as [Fw]
                                         FROM @c AS c  
                                         LEFT OUTER JOIN @Dw as dw ON c.id = [Dw].chromatogram
                                         LEFT OUTER JOIN @Fw as fw ON c.id = [Fw].chromatogram
                                         LEFT OUTER JOIN @Is as [is] ON c.id = [Is].chromatogram
                                         LEFT OUTER JOIN @AvgAnnotated as aa ON c.id = aa.chromatogram
                                         ", sep=""))  
  
  # order table
  gmd_meta <- gmd_meta[order(gmd_meta$chromatogram),]
  
  # change class from factor to numeric
  gmd_meta$Is <- as.numeric(as.character(gmd_meta$Is))
  gmd_meta$AvgAnnotated <- as.numeric(as.character(gmd_meta$AvgAnnotated))
  gmd_meta$Dw <- as.numeric(as.character(gmd_meta$Dw))
  gmd_meta$Fw <- as.numeric(as.character(gmd_meta$Fw))
   
  # join metadata from gmd with metadata from phenotyper (phenotyper_result_joined)
  library(plyr)
  gmd_meta_joined <- join(gmd_meta, phenotyper_result_joined, by="chromatogram", type="inner")
  
  return(gmd_meta_joined)  
  
  #print(length(intersect(gmd_meta$chromatogram, phenotyper_result_joined$chromatogram)))
}

# 3rd version: use LEFT OUTER JOIN, WITHOUT PHENOTYPER JOIN  
func_get_gmd_metadata_3 <- function(experiment_id){
  gmd_meta <- sqlQuery(dbhandle, paste("set nocount on; DECLARE @taglist uniqueidentifier
                                       set @taglist =", "\'", experiment_id, "\'",       
                                       "DECLARE @c TABLE(id uniqueidentifier PRIMARY KEY NOT NULL)
                                       INSERT @c(id)
                                       Select distinct FK_chromatogram from tf.IntensityValue
                                       where FK_TagList = @taglist
                                       --select id from @c
                                       
                                       DECLARE @TotalMstIntensityIntermediate TABLE(chromatogram uniqueidentifier PRIMARY KEY NOT NULL, value float);
                                       INSERT @TotalMstIntensityIntermediate(chromatogram, value)
                                       select i.FK_chromatogram as chromatogram, sum(i.value) as value
                                       from tf.IntensityValue i 
                                       --inner join @c c on i.FK_chromatogram = c.id
                                       where i.FK_TagList = @taglist
                                       group by i.FK_chromatogram     
                                       
                                       DECLARE @MstPerAnalyteCount TABLE(analyte uniqueidentifier PRIMARY KEY NOT NULL, Msts int);
                                       INSERT @MstPerAnalyteCount (analyte, Msts)
                                       select FK_Analyte, count(distinct fk_mst) as Anzahl
                                       from tf.MSTAnnotation a
                                       where FK_TagList = @taglist 
                                       and [Is Quantitative Cluster] = 1
                                       and FK_Analyte is not null
                                       group by FK_Analyte
                                       
                                       DECLARE @MST_AnzahlKorrekt float;
                                       select @MST_AnzahlKorrekt = sum(Msts) from @MstPerAnalyteCount 
                                       where analyte not in (select FK_Analyte from tf.InternalStandards where FK_TagList = @taglist and enabled = 1)
                                       --print @MST_AnzahlKorrekt
                                       
                                       DECLARE @DW TABLE(chromatogram uniqueidentifier PRIMARY KEY NOT NULL, value float);
                                       INSERT @DW(chromatogram, value)
                                       SELECT        v.FK_chromatogram as chromatogram, cast(i.value as float) as value
                                       FROM          @c c
                                       INNER JOIN (select distinct FK_chromatogram, FK_sample from Vial) v ON c.id = v.FK_chromatogram
                                       INNER JOIN tf.SampleInfo i on v.FK_sample = i.FK_Sample
                                       WHERE        (i.attribute = \'TROST/PhenoTyper CSchudoma DWcalc [mg]\')
                                       
                                       DECLARE @FW TABLE(chromatogram uniqueidentifier PRIMARY KEY NOT NULL, value float);
                                       INSERT @FW(chromatogram, value)
                                       SELECT        v.FK_chromatogram as chromatogram, cast(i.value as float) as value
                                       FROM          @c c
                                       INNER JOIN (select distinct FK_chromatogram, FK_sample from Vial) v ON c.id = v.FK_chromatogram
                                       INNER JOIN tf.SampleInfo i on v.FK_sample = i.FK_Sample
                                       WHERE        (i.attribute = \'TROST/PhenoTyper CSchudoma FreshWeight [mg]\')
                                       
                                       DECLARE @IS TABLE(chromatogram uniqueidentifier PRIMARY KEY NOT NULL, value float);
                                       INSERT @IS(chromatogram, value)
                                       select i.FK_chromatogram, SUM(i.value) / (
                                       Select SUM(msts)
                                       from @MstPerAnalyteCount c 
                                       inner join tf.InternalStandards s on c.analyte = s.FK_Analyte
                                       where s.QISTD = 1 
                                       and s.enabled = 1 
                                       and s.FK_TagList = @taglist)
                                       from tf.IntensityValue i 
                                       inner join tf.MSTAnnotation a on i.FK_TagList = a.FK_TagList and i.FK_MST = a.FK_MST
                                       inner join tf.InternalStandards s on i.FK_TagList = s.FK_TagList and a.FK_Analyte = s.FK_Analyte
                                       where a.FK_TagList = @taglist
                                       and s.QISTD = 1
                                       and a.[Is Quantitative Cluster] = 1
                                       group by i.FK_chromatogram
                                       
                                       DECLARE @AvgAnnotated TABLE(chromatogram uniqueidentifier PRIMARY KEY NOT NULL, value float);
                                       INSERT @AvgAnnotated(chromatogram, value)
                                       select i.FK_chromatogram as chromatogram, SUM(i.value) / @MST_AnzahlKorrekt as AvgChr 
                                       from tf.IntensityValue i
                                       inner join tf.MSTAnnotation a on i.FK_MST = a.FK_MST and i.FK_TagList = a.FK_TagList
                                       where a.FK_Analyte not in (Select FK_Analyte from tf.InternalStandards where FK_TagList = @taglist)
                                       AND a.FK_Analyte IS NOT NULL
                                       AND a.FK_TagList = @taglist
                                       group by i.FK_chromatogram
                                       
                                       SELECT c.id as chromatogram, [Is].value as [Is]
                                       , aa.value as [AvgAnnotated], [Dw].value as [Dw], [Fw].value as [Fw]
                                       FROM @c AS c  
                                       INNER JOIN @Dw as dw ON c.id = [Dw].chromatogram
                                       INNER JOIN @Fw as fw ON c.id = [Fw].chromatogram
                                       INNER JOIN @Is as [is] ON c.id = [Is].chromatogram
                                       INNER JOIN @AvgAnnotated as aa ON c.id = aa.chromatogram
                                       ", sep=""))
  # order table
  gmd_meta <- gmd_meta[order(gmd_meta$chromatogram),]
  
  # change class from factor to numeric
  gmd_meta$Is <- as.numeric(as.character(gmd_meta$Is))
  gmd_meta$AvgAnnotated <- as.numeric(as.character(gmd_meta$AvgAnnotated))
  gmd_meta$Dw <- as.numeric(as.character(gmd_meta$Dw))
  gmd_meta$Fw <- as.numeric(as.character(gmd_meta$Fw))

  return(gmd_meta)  
  
}