
filter <-'JPMorgan Chase|@jpmorgan|jpmorgan|jpmorgan chase|#jpmorgan|Citigroup Global Markets Inc.|citigroup global markets|citibank|citigroup|@Citi|@citi|@Bank of America|Bank of America|bank of america|#bankofamerica|@WellsFargo|Wells Fargo|wells fargo|Goldman Sachs & Co. LLC|Goldman Sachs|goldman sachs|goldmansachs|GoldmanSachs'
filterusa<-paste0(filter,'|Morgan Stanley|@MorganStanley|morgan stanley|@usbank|us bank')
filterusan<-paste0(filterusa,'|@PNCBank|@PNCNews|@PNCBank_Help|@BNYMellon|BNY Mellon|bny mellon|bny')

filter_griechen <-'#Grobservations|$NBG|#NBG|#nbg|@Eurobank_Group|@eurobank_group|Eurobank_group|@Alpha_Bank|aplha bank|Alpha Bank|#alphabank'
filter_2griech<-paste0(filter_griechen,'|@PIRAEUSBank|piraeus bank|Piraeus Bank|piraeus bank|#piraeusbank|')

filter_griechen_europa <- paste0(filter_2griech,'|@ecb|ecb|#ezb|European Central Bank|european central bank|@HSBC_UK|HSBC UK|hsbc uk|hsbc bank|#atebank|')
filter_griechen_europa2<- paste0(filter_griechen_europa,'|@bancosantander|Banco Santander|banco santander|#bancosantander|@BNPParibas|BNP Paribas Group|bnp paribas group|#bnpparibas|@ING_news|ING Group|ing group|@ingnl|')

filter_eu<-'|@ecb|ecb|#ezb|European Central Bank|european central bank|@HSBC_UK|HSBC UK|hsbc uk|hsbc bank|#atebank|'
filter_ch<-'@UBSchweiz|UBS Schweiz|ubs schweiz|@UBS|Grobservations|$NBG|#NBG|#nbg|'
filter_gl<-'@PIRAEUSBank|piraeus bank|Piraeus Bank|piraeus bank|#piraeusbank|#Grobservations|$NBG|#NBG|#nbg|@Eurobank_Group|@eurobank_group|Eurobank_group|@Alpha_Bank|aplha bank|Alpha Bank|#alphabank|#atebank|atebank'
filter_gb<-'@AskLloydsBank|Lloyds Banking Group|@LBGNews|@AskHalifaxBank|@AskBankOfScot|lloyds banking group|#barclay|#hsbc|@HSBC_UK|HSBC UK|hsbc uk|hsbc bank'
filter_db <-'@DeutscheBankAG|@DeuBaService|@DeutscheBank|$DB|#deutschebank|@TalkGTB|@DeutscheBankBE|@commerzbank|commerzbank|#commerzbank|@KfW|#kfw|kfw bank'
filter_fr <-'@BNPParibas|@BNPParibas_SAV|bnp |@BNPParibas|BNP Paribas Group|bnp paribas group|#bnpparibas|@SocieteGenerale|Societe Generale|@CreditAgricole|Credit Agricole|#bnp'
filter_sp <-'@bancosantander|bancosatnander|#BancoSantander|Banco Santander|$SAN|@bbva|@CajadeAhorrosPA|Caja de AhorrosP|#CaixaCataluña|caja de ahorrosP|'
filter_i <- '@UniCredit_PR|UniCredit Group|@intesasanpaolo|intesa sanpaolo|@IntesaSP_Help|#intesasanpaolo|'
filter_nl <-'@Rabobank|#Rabobank|rabobank|@rabobank'
filter_europa<- paste0(filter_eu,filter_ch)
filter_europa <-paste0(filter_europa,filter_gb)
filter_europa <-paste0(filter_europa,filter_db)
filter_europa <-paste0(filter_europa,filter_fr)
filter_europa <-paste0(filter_europa,filter_sp)
filter_europa <-paste0(filter_europa,filter_i)
filter_europa <-paste0(filter_europa,filter_nl)

filter_europa





#finance & #economics 
#Finance #Banque