
filter <-'JPMorgan Chase|@jpmorgan|jpmorgan|jpmorgan chase|#jpmorgan|Citigroup Global Markets Inc.|citigroup global markets|citibank|citigroup|@Citi|@Bank of America|Bank of America|bank of america|#bankofamerica|@WellsFargo|Wells Fargo|wells fargo|Goldman Sachs & Co. LLC|GoldmanSachs'
filterusa<-paste0(filter,'|Morgan Stanley|@MorganStanley|@usbank|us bank|@Aetna|@Affiliated_Bank|@Affiliated_Bank')
filterusan<-paste0(filterusa,'|@PNCBank|@PNCNews|@PNCBank_Help|@BNYMellon|BNY Mellon|@MetLife|@Voya|@lincolnfingroup|@principal|@GNWFinancial|#genworth|@Ally|@TheHartford|Hartford financial|@CapitalOne|@AskCapitalOne|@COFInvesting|@hartfordfunds|Hartford Funds|hartford funds|$MBI|@AssociatedBank|Associated Bank|associated bank|@AssociatedBiz|#associatedbank|@CNOFinancial|CNO Financial Group|@CitizensBank|Citizens BankVerified|#citizensbank')

filter_griechen <-'#Grobservations|$NBG|#NBG|#nbg|@Eurobank_Group|@eurobank_group|Eurobank_group|@Alpha_Bank|aplha bank|Alpha Bank|#alphabank'
filter_2griech<-paste0(filter_griechen,'|@PIRAEUSBank|piraeus bank|Piraeus Bank|piraeus bank|#piraeusbank')
filter_griechen_europa <- paste0(filter_2griech,'|@ecb|ecb|#ezb|European Central Bank|european central bank|@HSBC_UK|HSBC UK|hsbc uk|hsbc bank')
filter_griechen_europa2<- paste0(filter_griechen_europa,'|@bancosantander|Banco Santander|banco santander|#bancosantander|@BNPParibas|BNP Paribas Group|bnp paribas group|#bnpparibas|@ING_news|ING Group|ing group|@ingnl|')
filter_ch<-'@UBSchweiz|UBS Schweiz|ubs schweiz|@UBS|'
filter_gb<-'@AskLloydsBank|Lloyds Banking Group|@LBGNews|@AskHalifaxBank|@AskBankOfScot|lloyds banking group|#barclay|#hsbc|@UKFtweets|UK Finance|@triodosuk|Triodos Bank UK|@BankofIrelandUK|Bank of Ireland UK|bank of ireland uk|Royal BankVerified account|@RBS_Help|@StanChart|Standard Chartered|@nexgrp|@OldMutual|Old Mutual plc|@ParagonBankNC|Paragon Bank|BritishBusinessBankVerified account|@BritishBBank|'
filter_db <-'@DeutscheBankAG|@DeuBaService|@DeutscheBank|$DB|#deutschebank|@TalkGTB|@DeutscheBankBE|@commerzbank|commerzbank|#commerzbank|@KfW|#kfw|kfw bank|@wuestenrot_de|@spardanuernberg|Sparda-Bank Nürnberg|sparda-bank|oldenburgische landesbank|@comdirect|comdirect bank AG|comdirect bank|#Allianz|alistra office|#DAXIndex|Deutsche Börse GroupVerifizierter Account|@DES_AG|Deutsche EuroShop AG|#bayernlb|@BremerLB|@LBBW|#landesbank|Landesbank Hessen-Thüringen|#helaba|@Helaba|@nord_lb|@ING_DiBa_Presse|@INGDiBaAustria|'
filter_fr <-'@PIRAEUSBank|piraeus bank|Piraeus Bank|piraeus bank|#piraeusbank|@SocieteGenerale|Societe Generale|@CreditAgricole|Credit Agricole|#bnp|@natixis|Natixis|#natixis|@AXA|#AXA|AXA|'
filter_sp <-'@bancosantander|bancosatnander|#BancoSantander|Banco Santander|$SAN|@bbva|@CajadeAhorrosPA|Caja de AhorrosP|#CaixaCataluña|caja de ahorrosP|@Bankia|Bankia|bankia|@Bankinter|Bankinter|FDN Colombia|@FDNcolombia|'
filter_i <- '@UniCredit_PR|UniCredit Group|@intesasanpaolo|intesa sanpaolo|@IntesaSP_Help|#intesasanpaolo|@BancoBPMSpa|BancoBPM|@UnipolGroup_PR|Unipol Group PRVerified account|@UniCredit_PR|'
filter_nl <-'@Rabobank|#Rabobank|rabobank|@rabobank'
filter_europa<- paste0(filter_griechen_europa2,filter_ch)
filter_europa <-paste0(filter_europa,filter_gb)
filter_europa <-paste0(filter_europa,filter_db)
filter_europa <-paste0(filter_europa,filter_fr)
filter_europa <-paste0(filter_europa,filter_sp)
filter_europa <-paste0(filter_europa,filter_i)
filter_europa <-paste0(filter_europa,filter_nl)

filter_europa





#finance & #economics 
#Finance #Banque