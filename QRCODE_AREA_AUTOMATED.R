
prov_cod<-"09"

ae <- st_read("shp files/LIMITE_INTERNO_DE_AE.shp") %>% 
  dplyr::filter(CodProv==prov_cod)

imrs_id <- unique(ae$IMRS_ID)

for (i in imrs_id) {
  
  file_name = i
  locat = i
  
  l_codigo <- uniqID_maker(user = FALSE, locat, level = 1:125)
  
  l_codigo$in.d_number <- sprintf("%02d", rep(1:25, each = 5))
  l_codigo$ind_number <- sprintf("%02d", rep(1:5))
  
  l_codigo$label <- paste(l_codigo$ind_string,l_codigo$in.d_number,l_codigo$ind_number, sep="")
  
  custom_create_PDF(Labels =l_codigo, name =file_name, ErrCorr = "H", Fsz = 13, Across =T, ERows =0, numrow = 25, numcol = 5,type="matrix",
                    page_width = 8, page_height = 12, width_margin = 0.3,
                    height_margin = 0.2)
  
}