#Create dataframe####

df_sparrow<-data.frame(
  #REQUIRED
  #HYBAS_ID = data1$HYBAS_L12,
  waterid = data1$HYRIV_ID,
  HYBAS_ID = data1$HYBAS_ID,
  #SUB_AREA = data1$SUB_AREA,
  #UP_AREA = data1$UP_AREA,
  fnode = data1$fnode.x,
  tnode = data1$tnode.y,
  frac = 1, # no diversion of water/mass in hydroatlas 
  iftran = data1$iftran.x,
  demiarea = data1$CATCH_SKM,#reach incremental drainage area
  hydseq = NA,
  termflag = data1$termflag.x,
  rchtype =data1$rchtype.x,
  calsites = data1$calsites,
  #FIXED
  target = data1$target,  #Terminal target reach (1=target; 0=non target) for computing 
  #load delivery from each upstream reach to the nearest 
  #downstream target reach (e.g., input to estuary or reservoir)
  lat=data1$lat_wq,
  lon=data1$lon_wq,
  rchname = data1$HYRIV_ID,
  demtarea = data1$UPLAND_SKM,
  length = data1$LENGTH_KM - data1$LENGTH_KM_in,
  meanq = data1$dis_m3_pyr,
  rchtot = NA,#Reach time of travel
  hload  = data1$hload.y,#Areal hydraulic load for impoundments
  staid = NA,#Unique station ID # hydrologically ordered 
  station_id = NA,
  station_name = NA,
  #depvar = data1$LOAD_A_006,
  depvar = data1$Load,
  depvar_se = NA, #data1$SLOAD_A_00600,
  #depvar_se = NA,
  weight = NA,
  PPT30MEAN = data1$pre_mm_cyr,
  meanTemp = data1$tmp_dc_cyr,
  #point = data1$point,
  Hylak_id = data1$Hylak_id,
  LENGTH_KM_in = data1$LENGTH_KM_in,
  HYRIV_RCH = data1$HYRIV_RCH,
  #buffer = data1$buffer,
  #nzsegment = data1$nzsegment,
  sID = data1$siteid
  #load_ton_kg_year = data1$load_ton_kg_year,
  #load_kg_year = data1$load_kg_year
)

#remove duplicates
df_sparrow<-df_sparrow[!duplicated(df_sparrow$waterid),]
