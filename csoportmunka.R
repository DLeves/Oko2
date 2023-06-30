# working directory beállítása
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# libraryk
library(quantmod)  #Yahoo finance adatokhoz
library(gtrendsR)  #Google trends adatokhoz
library(tidyverse) #ggplot2 + más packegek, ha kellenének
library(aTSA) # adf.test-hez
library(lmtest) # bgtest, coeftest...
library(vars) # var modellhez
library(aod) #granger


#Adatok----
#********************************************************************************************
# Adatok összegyűjtése
#********************************************************************************************

# Yahoo finance + Kinfo dummy adatok ---------------------------------------------------------------
# Yahoo finance-rők az árfolyam lekérése 2021. jan 1-től 2022. dec 31-ig
eurxts = getSymbols("EURHUF=X", src = "yahoo", from = "2020-01-01", to = "2023-05-12", auto.assign = F)
usdxts = getSymbols("USDHUF=X", src = "yahoo", from = "2020-01-01", to = "2023-05-12", auto.assign = F)

# Kormányinfó dátumok
kinfodummy = read.csv("Kormanyinfo.csv")

#Google keresések száma
google = read.csv("KormanyinfoGoogleTrend.csv")

# a Yahoo finance-es adatokből dataframe-ként az idő és az 'highest'(ezt közösen beszéljük meg melyik legyen)


arfolyam = data.frame(time = index(eurxts),
                      eurhuf = as.numeric(eurxts$`EURHUF=X.High`),
                      usdhuf = as.numeric(usdxts$`USDHUF=X.High`),
                      kinfodummy = kinfodummy$Kormanyinfo)
rm(eurxts, usdxts)

# EUR/HUF, USD/HUF alap vonaldiagramm, 'theme_minimal'-al
kinfo = arfolyam %>% filter(kinfodummy == 1) # azért van ilyen data frame, hogy tudjam ábrázolni a kormányinfó pontokat

ggplot()+
  geom_line(data = arfolyam, aes(x = time, y = eurhuf, color = "EUR/HUF árfolyam"))+
  geom_line(data = arfolyam, aes(x = time, y = usdhuf, color = "USD/HUF árfolyam"))+
  geom_point(data = kinfo, aes(x = time, y = eurhuf, color = "Kormányinfó"))+
  geom_point(data = kinfo, aes(x = time, y = usdhuf, color = "Kormányinfó"))+
  theme_minimal()

rm(kinfodummy)

# google trends napi adatok------------------------------------------------------------------------
# Google trends-ről 'korányinfó' keresési trend
#   Itt egy olyan probléma lép fel, hogy max ~90 napos intervallumban vannak napi adatok, annál nagyobbakban már hetiek
#   ezt úgy lehetne orvosolni, hogy sokszor kérek le 90 naposat.
#   Azonban ez ugye arányosítva van, ezért minden ~90 napban biztosan van egy 100-as érték -> minimum 8db ilyenünk lesz

# a 90 napos intervallumokat tartalmazó vektor
t = c("2020-01-01 2020-04-01","2020-04-02 2020-06-30","2020-07-01 2020-10-01","2020-10-02 2021-01-01",
      "2021-01-02 2021-04-01","2021-04-02 2021-06-30","2021-07-01 2021-10-01","2021-10-02 2022-01-01",
      "2022-01-02 2022-04-01","2022-04-02 2022-06-30","2022-07-01 2022-10-01","2022-10-02 2023-01-01",
      "2023-01-02 2023-04-01","2023-04-02 2023-05-11")

# 'kinfo' dataframe létrehozása
kinfo = data.frame()

# kinfo dataframe feltöltése
#   itt van egy Sys.sleep(2) függvény meghívva azért, hogy a google ne higyje azt,
#   hogy DDOS-olni szeretném a rövid időn belüli sok lekéréséert,
#   ezért a gépállat pihen minden lekérés után 2 másodpercet(nem, az 1 mp még nem jó)
#                   |
#                   V
#   szóval lassan fog lefutni, ezzel együtt kell élni

for (i in 1:length(t)) {
  kinfo = rbind(kinfo, gtrends(keyword = "kormányinfó", time = t[i])$interest_over_time)
  Sys.sleep(2)
}

# kitörlöm a felesleges oszlopokat, ami marad átnevezem
kinfo = kinfo[,1:2]
colnames(kinfo) =  c("time", "trend")

kinfoSzuk = merge(x = arfolyam, y = kinfo, by = "time", all.x = T)
kinfoSzuk = merge(arfolyam, kinfo)

# lementem a 'kinfo'-t, hogy ne kelljen sokat várni legközelebb
write.csv(kinfoSzuk[,c("time", "trend")], "KormanyinfoGoogleTrend.csv", row.names = F)

kinfo = read.csv("KormanyinfoGoogleTrend.csv")
arfolyam$gtrend = kinfo$trend

# kormányinfó trend alap vonaldiagramm, 'theme_minimal'-al
ggplot(kinfo, aes(x = time))+
  geom_line(aes(y = trend, color = "kormányinfó keresési trend"))+
  theme_minimal()



# google trends heti adatok--------------------------------------------------------------------------
# Eddig voltak az adatok az öko csoportmunkában
# A negyedéves adatokkal az a probléma ebben az esetben, hogy minden Q-ban van egy 100-as érték 
# Ezt azzal lehetne kiküszöbölni, ha az egész időszakra néznénk az adatokat
# Azzal meg az a baj, hogy heti gyakoriságúak
# Meg lehetne csinálni, hogy a heti kereséseket alapul véve a napokkal súlyozok
# Először azonban valahogy egy dataframe-be kellene helyezni őket

google_napi = read.csv("KormanyinfoGoogleTrend.csv")
google_napi$time = as.POSIXct(google_napi$time, tz = "GMT")

google_heti = gtrends(keyword = "kormányinfó", time = "2019-12-29 2023-05-15")$interest_over_time

# van itt egy darab '<1' érték, ezért character-ként van a dataframeben
# ezt megelőlegezem egy 1-el, így már lehet integer-ként kezelni őket

google_heti$hits = as.numeric(google_heti$hits)
google_heti$hits[is.na(google_heti$hits)] = 1

# Most, hogy ez megvan, el lehet kezdeni gondolkodni, hogy mégis hogyan lehetne összetenni a két df-et

google = data.frame(
  time = google_napi$time,
  napi = google_napi$trend,
  heti = 0
)

# Lehetne ennél szofisztikáltabban is, de egyenlőre működik

i = 1
for(j in 1:nrow(google_heti)){
  while (google$time[i] >= google_heti$date[j] && google$time[i] < google_heti$date[j+1]) {
    google$heti[i] = google_heti$hits[j]
    print(google[i,])
    i = i + 1
    if(i > nrow(google)){
      break
    }
  }
}

# Inkább lementem, hogy legközelebb csak be kelljen olvasni

arfolyam$napi_google = google$napi
arfolyam$heti_google = google$heti

write.csv(arfolyam,"ArfolyamKinfo.csv", row.names = F)

# Lényegében megvannak a napi és heti átlagok 
# Már csak azt kellene kitalálni, hogy ezeket hogyan kombináljuk

# ----------------------------------------------------------------------------------------------------
# ezt esetleg bele lehetne tenni breakpointnak
# https://telex.hu/gazdasag/2022/10/14/rendkivuli-bejelentest-tesz-az-mnb-csak-azt-nem-tudni-meg-hogy-mikor
# ----------------------------------------------------------------------------------------------------

arfolyam = read.csv("ArfolyamKinfo.csv")
arfolyam$time = as.Date(arfolyam$time)

#Box-Jenkins----
#********************************************************************************************
# Box-Jenkins módszer
#********************************************************************************************

# 1. lépés --------------------------


adf.test(arfolyam$eurhuf)
adf.test(arfolyam$usdhuf)
# nem stacionerek

# 2. lépés -------------------------

arfolyam$d_eurhuf = c(NA, diff(arfolyam$eurhuf))
arfolyam$d_usdhuf = c(NA, diff(arfolyam$usdhuf))

adf.test(diff(arfolyam$eurhuf))
adf.test(diff(arfolyam$usdhuf))
# stacionerek

# 3. lépés -------------------------

bgtest(diff(arfolyam$eurhuf) ~ 1, order = 29)
bgtest(diff(arfolyam$usdhuf) ~ 1, order = 29)

Box.test(diff(arfolyam$eurhuf), lag = 29, type = "Ljung-Box")
Box.test(diff(arfolyam$usdhuf), lag = 29, type = "Ljung-Box")
# nem fehérzajok még

# 4. lépés -------------------------
acf(arfolyam$d_eurhuf[-1]) # MA(1)
pacf(arfolyam$d_eurhuf[-1]) # AR(1)

acf(arfolyam$d_usdhuf[-1]) # MA(1)
pacf(arfolyam$d_usdhuf[-1]) # AR(1) (de kiugrik a 6. és a 25. lag is)

# 5. lépés -------------------------


# 6. lépés -------------------------
coeftest(forecast::auto.arima(arfolyam$eurhuf))
coeftest(forecast::auto.arima(arfolyam$eurhuf, xreg = arfolyam$kinfodummy))
coeftest(forecast::auto.arima(arfolyam$eurhuf, xreg = arfolyam$napi_google))
eur = forecast::auto.arima(arfolyam$eurhuf, xreg = arfolyam$kinfodummy)

coeftest(forecast::auto.arima(arfolyam$usdhuf))
coeftest(forecast::auto.arima(arfolyam$usdhuf, xreg = arfolyam$kinfodummy))
coeftest(forecast::auto.arima(arfolyam$usdhuf, xreg = arfolyam$napi_google))
usd = forecast::auto.arima(arfolyam$usdhuf, xreg = arfolyam$napi_google)

#----
#********************************************************************************************
# VAR/VACM modell
#********************************************************************************************

# Kikommenteltem ezeket a sorokat, mert valami hiba volt benne, de nem mertem belenyulni - Levi

# #információs kritériumok
# vars::VARselect(arfolyam$[2:nrow(d_eurhuf), 
#                   c("d_eurhuf", "kinfo", "google")], 
#                   lag.max = 24)
#                   
# #Var modell                  
# var_modell <- VAR(arfolyam$[2:nrow(d_eurhuf),
#                   c("d_eurhuf", "kinfo", "google")],
#                   p = ?,
#                   type = "const")
# summary(var_modell)

# #granger
# coef(var_modell$varresult$d_eurhuf) #együtthatók
# coef(var_modell$varresult$d_eruhuf) #mik kerülnek a Grangerbe
# vcov(var_modell$varresult$d_eurhuf) #standardhiba négyzetek
# aod::wald.test(b=coef(var_modell$varresult$d_eurhuf),
#                Sigma=vcov(var_modell$varresult$d_eurhuf), 
#                Terms=c(?,?) )
# 
# #együttes fehérzajság
# var_hibatagok <- as.data.frame(resid(var_modell))
# #egyenkénti fehérzajság
# lapply(var_hibatagok, function(i) bgtest(i ~ 1, order = ?))
# 
# #egységgyökteszt
# if(abs(roots(var_modell))<1, "VAR modell stabil", "VAR modell instabil")
# 
# #Impulzus-válaszfügvény
# plot(irf(var_modell,
#          impulse = "kinfo", 
#          response = "d_eurhuf", 
#          n.ahead = 20, 
#          ortho = TRUE))






