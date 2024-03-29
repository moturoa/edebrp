
# Remake the datasets in this package
# Allen zijn koppeltabellen voor ethniciteit bepalingen


library(dplyr)


geboorte_land_hercodering <- read.csv("scripts/data/geboorte_land_hercodering.csv", colClasses = "character")

land_code_labels <- read.csv("scripts/data/land_code_labels.csv", colClasses = "character")

buurt_key <- tibble::tribble(
    ~buurt_code, ~buurt_code_cipers,
    "0101","101",
    "0102","102",
    "0103","103",
    "0104","104",
    "0105","105",
    "0201","201",
    "0202","202",
    "0203","203",
    "0204","204",
    "0205","205",
    "0206","206",
    "0301","301",
    "0302","302",
    "0303","303",
    "0305","305",
    "0306","306",
    "0307","307",
    "0401","401",
    "0402","402",
    "0403","403",
    "0501","501",
    "1001","601",
    "1002","602",
    "1003","603",
    "1004","604",
    "1101","611",
    "1102","612",
    "1103","613",
    "1201","621",
    "1202","622",
    "1203","623",
    "1204","624",
    "1301","631",
    "1302","632",
    "1303","633",
    "1304","634",
    "1305","635",
    "1306","636",
    "2031","731",
    "2032","732",
    "2051","751",
    "3001","801",
    "3002","802",
    "3003","803",
    "3004","804",
    "3031","831",
    "3051","851",
    "4001","941",
    "4002","942",
    "4003","943",
    "4031","944",
    "4032","945",
    "4033","946",
    "4034","947",
    "4051","948",
    "5001","951",
    "5031","952",
    "6001","961",
    "6031","962",
    "7001","971",
    "7002","972",
    "7031","973",
    "7051","974",
    "8001","981",
    "8031","982",
    "8051","983",
    "9001","991",
    "9031","992",
    "9051","993",
    "9052","994",
    "9053","995"
  )


# Stap 5: is de indeling naar de verschillende categorieën. Die ik eerder al had gestuurd.
geboorte_land_cat_key <- tibble::tribble(
  ~geboorte_land_code_her, ~land_categorie_code,
  '6043','1',  
  '5022','2',
  '5007','3',  
  '7011','4', 
  '5010','6',  
  '5015','6',  
  '9089','6',  
  '6002','6',  
  '5002','6',  
  '6003','6',  
  '6007','6',  
  '7044','6',  
  '6018','6',  
  '5009','6',  
  '7050','6',  
  '6037','6',  
  '6039','6',  
  '5039','6',  
  '5040','6',  
  '7065','6',  
  '7064','6',  
  '7066','6',  
  '5017','6',  
  '7003','6',  
  '7028','6',  
  '5049','6',  
  '6067','6',  
  '6066','6',
  '7024','6',  
  '7047','6',
  '6024','7',
  '5034','8', 
  '7005','8',  
  '8014','8',  
  '6055','8',  
  '6011','8',  
  '6045','8',  
  '8034','8',
  '6012','8',  
  '5032','8', 
  '6027','8',  
  '6028','8',  
  '9049','8',  
  '5045','8',  
  '5003','8',  
  '7088','8',  
  '7030','8',  
  '5001','8',  
  '7092','8',  
  '5065','8',  
  '6014','8',
  '8002','8',  
  '6016','8',  
  '8043','8',  
  '6032','8',  
  '6054','8',  
  '8001','8',  
  '5080','8',  
  '8027','8',  
  '8009','8',  
  '9056','8',
  '9094','8',  
  '8003','8',  
  '7057','8',  
  '7099','8',  
  '5013','8',  
  '8006','8',  
  '8021','8',  
  '5071','8',  
  '8022','8',  
  '5076','8',
  '8028','8',  
  '9090','8',  
  '8005','8',  
  '5077','8',  
  '6062','8',
  '7035','8',
  '6030','9',
  '5107','9'  # Curacao hoort bij Nederland in Ede
)

geboorte_land_cat_labels <- tibble::tribble(
  ~land_categorie_code, ~land_categorie,
  "1", "Turkije",
  "2", "Marokko",
  "3", "Suriname",
  "4", "(voormalig) Nederlandse Antillen + Aruba",
  "5", "Overige niet-westerse landen",
  "6", "EU-landen",
  "7", "Indonesie",
  "8", "Overige westerse landen",
  "9", "Nederland"
)

geboorte_land_western_labels <- tibble::tribble(
  ~alltotc_omschrijving, ~alltotc_nwesters_code, ~alltotc_nwesters,
  "Turkije", 3, "niet-westers",
  "Marokko", 3, "niet-westers",
  "Suriname", 3, "niet-westers",
  "(voormalig) Nederlandse Antillen + Aruba", 3, "niet-westers",
  "Overige niet-westerse landen", 3, "niet-westers",
  "EU-landen", 2, "westers",
  "Indonesie", 2, "westers",
  "Overige westerse landen",2,"westers",
  "Nederland",1,"Nederland"
)

geboorte_land_code_key <- tibble::tribble(
  ~geboorte_land_code, ~ethniciteit_cbs_code,
  '6043','1',
  '5022','2',
  '5007','3',
  '7011','4', 
  '5010','6',
  '5015','6',
  '9089','6',
  '6002','6',
  '5002','6',
  '6003','6',
  '6007','6',
  '7044','6',
  '6018','6',
  '5009','6',
  '7050','6',
  '6037','6',
  '6039','6',
  '5039','6',
  '5040','6',
  '7065','6',
  '7064','6',
  '7066','6',
  '5017','6',
  '7003','6',
  '7028','6',
  '5049','6',
  '6067','6',
  '6066','6',
  '7024','6',
  '7047','6',  
  '6024','7',
  '5034','8',
  '7005','8',
  '8014','8',
  '6055','8',
  '6011','8',
  '6045','8',
  '8034','8',
  '6012','8',
  '5032','8',
  '6027','8',
  '6028','8',
  '9049','8',
  '5045','8',
  '5003','8',
  '7088','8',
  '7030','8',
  '5001','8',
  '7092','8',
  '5065','8',
  '6014','8',
  '8002','8',
  '6016','8',
  '8043','8',
  '6032','8',
  '6054','8',
  '8001','8',
  '5080','8',
  '8027','8',
  '8009','8',
  '9056','8',
  '9094','8',
  '8003','8',
  '7057','8',
  '7099','8',
  '5013','8',
  '8006','8',
  '8021','8',
  '5071','8',
  '8022','8',
  '5076','8',
  '8028','8',
  '9090','8',
  '8005','8',
  '5077','8',
  '6062','8',
  '7035','8',
  '6030','9',
  '5107','9'  # Curacao hoort bij NL in Ede.
)
# ELSE 5!

land_code_key <- tibble::tribble(
  ~ethniciteit_cbs_code, ~ethniciteit_cbs_omschrijving, 
  '1', 'Turkije', 
  '2', 'Marokko', 
  '3', 'Suriname', 
  '4', '(vm.) Ned. Antillen + Aruba',
  '5', 'Ov. niet-westerse landen',
  '6', 'EU-landen',
  '7', 'Indonesië',
  '8', 'Ov. westerse landen',
  '9', 'Nederland')


geboorte_land_code_key <- dplyr::left_join(geboorte_land_code_key,
                                           land_code_key,
                                           by = 'ethniciteit_cbs_code')



# categorie 1
eth_code_cat1 <- tibble::tribble(
  ~ethniciteit_cbs_code, ~ethniciteit_cat1_code,
  '9', '1',
  '6', '2',
  '7', '2',
  '8', '2',
  '1', '3',
  '2', '3',
  '3', '3',
  '4', '3',
  '5', '3')

eth_code_cat1_labels <- tibble::tribble(
  ~ethniciteit_cat1_code, ~ethniciteit_cat1_omschrijving,
  '1', 'Nederland',
  '2', 'westers',
  '3', 'niet-westers')

eth_code_cat1 <- left_join(eth_code_cat1, eth_code_cat1_labels,
                           by = "ethniciteit_cat1_code")

geboorte_land_code_key <- left_join(geboorte_land_code_key,
                                    eth_code_cat1, 
                                    by = "ethniciteit_cbs_code")

# MOE landen.
ethniciteit_moe_key <- tibble::tribble(
  ~geboorte_land_code, ~ethniciteit_moe_code,
  '5034','1111',
  '7005','1111',
  '5010','1111',
  '9089','1111',
  '7024','7024',
  '5040','1111',
  '5015','1111',
  '8014','1111',
  '6002','1111',
  '5002','1111',
  '6055','1111',
  '6003','1111',
  '6039','1111',
  '5017','5017',
  '6007','1111',
  '6011','1111',
  '7044','1111',
  '6045','1111',
  '5049','5049',
  '8034','1111',
  '6012','1111',
  '6018','1111',
  '7003','1111',
  '8035','1111',
  '5032','1111',
  '6030','6030',
  '6027','1111',
  '5009','1111',
  '7028','7028',
  '7050','1111',
  '7047','7047',
  '6028','1111',
  '9049','1111',
  '7064','7064',
  '7065','7065',
  '7066','7066',
  '6037','1111',
  '7048','1111',
  '6066','6066',
  '6067','6067',
  '6043','1111',
  '5045','1111',
  '5039','1111',
  '5003','1111',
  '5107', '6030')  # curacao in NL in Ede
# ELSE 2222

ethniciteit_moe_labels <- tibble::tribble(
  ~ethniciteit_moe_code,  ~ethniciteit_moe_omschrijving,
  '7024', 'Bulgarije',
  '5017', 'Hongarije',
  '5049', 'Slovenie',
  '7028', 'Polen',
  '7047', 'Roemenie',
  '7065', 'Estland',
  '7064', 'Letland',
  '7066', 'Litouwen',
  '6067', 'Slowakije',
  '6066', 'Tsjechie',
  '6030', 'Nederland',
  '1111', 'Overig Europa',
  '2222', 'Overige werelddelen')

ethniciteit_moe_key <- left_join(ethniciteit_moe_key, 
                                 ethniciteit_moe_labels,
                                 by = "ethniciteit_moe_code")


geboorte_land_code_key <- left_join(geboorte_land_code_key,
                                    ethniciteit_moe_key, 
                                    by = "geboorte_land_code")

usethis::use_data(
  geboorte_land_hercodering,
  land_code_labels,
  buurt_key,
  geboorte_land_cat_key,
  geboorte_land_cat_labels,
  geboorte_land_western_labels,
  geboorte_land_code_key,
  overwrite = TRUE
)




# alleen te maken met oude levering
make_buurt_koppel_pink <- function(hh){
  select(hh, buurt_code_cipers, buurt_code_cbs, buurt_naam) %>% distinct  
}

# hh is output van bepaal_huishouden (test_new_cipers)
# buurt_koppel_fix_pink <- make_buurt_koppel_pink(hh)
# buurt_koppel_fix_pink$buurt_naam[buurt_koppel_fix_pink$buurt_naam == "Componistenbuurt"] <- "Komponistenbuurt"
#usethis::use_data(buurt_koppel_fix_pink, overwrite = TRUE)


