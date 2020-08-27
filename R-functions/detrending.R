
AG <- drop_na(env) %>% 
        filter(region == "Alto Golfo")
TS <- ts(AG$sst, start= c(2004, 1), frequency = 12)
acf(TS)

components.ts = decompose(TS)
plot(components.ts$trend)
AG <- components.ts$trend


SR <- drop_na(env) %>% 
        filter(region == "Santa Rosalia")
TS <- ts(SR$sst, start= c(2004,1), frequency = 12)
acf(TS)

components.ts = decompose(TS)
plot(components.ts$trend)
SR <- components.ts$trend


Lo <- drop_na(env) %>% 
        filter(region == "Loreto")
TS <- ts(Lo$sst, start= c(2004, 1), frequency = 12)
acf(TS)

components.ts = decompose(TS)
plot(components.ts$trend)
Lo <- components.ts$trend


Co <- drop_na(env) %>% 
        filter(region == "Corredor")
TS <- ts(Co$sst, start= c(2004, 1), frequency = 12)
acf(TS)

components.ts = decompose(TS)
plot(components.ts$trend)
Co <- components.ts$trend


Lp <- drop_na(env) %>% 
        filter(region == "La Paz")
TS <- ts(Lp$sst, start= c(2004, 1), frequency = 12)
acf(TS)

components.ts = decompose(TS)
plot(components.ts$trend)
Lp <- components.ts$trend


Lv <- drop_na(env) %>% 
        filter(region == "La Ventana")
TS <- ts(Lv$sst, start= c(2004, 1), frequency = 12)
acf(TS)

components.ts = decompose(TS)
plot(components.ts$trend)
Lv <- components.ts$trend


CP <- drop_na(env) %>% 
        filter(region == "Cabo Pulmo")
TS <- ts(CP$sst, start= c(2004, 1), frequency = 12)
acf(TS)

components.ts = decompose(TS)
plot(components.ts$trend)
CP <- components.ts$trend


CSL <- drop_na(env) %>% 
        filter(region == "Los Cabos")
TS <- ts(CSL$sst, start= c(2004, 1), frequency = 12)
acf(TS)

components.ts = decompose(TS)
plot(components.ts$trend)
CSL <- components.ts$trend



IM <- drop_na(env) %>% 
        filter(region == "Islas Marias")
TS <- ts(IM$sst, start= c(2004, 1), frequency = 12)
acf(TS)

components.ts = decompose(TS)
plot(components.ts$trend)
IM <- components.ts$trend



Revi <- drop_na(env) %>% 
        filter(region == "Revillagigedo")
TS <- ts(Revi$sst, start= c(2004, 1), frequency = 12)
acf(TS)

components.ts = decompose(TS)
plot(components.ts$trend)
Revi <- components.ts$trend




trends <- cbind(AG, SR, Lo, Co, Lp, Lv, CP, CSL, IM, Revi) %>% 
        as.data.frame() %>% 
        mutate(date = seq(as.Date("2004-01-01"), as.Date("2019-01-01"), by="month")) %>% 
        pivot_longer(AG:Revi, names_to = "region", values_to = "sst_trend") 

trends$region <- factor(trends$region, 
                        levels = c("AG", "SR", "Lo", "Co", "Lp", "Lv", "CP", "CSL", "IM", "Revi"), 
                        labels = c("AG"="Alto Golfo", 
                                   "SR"="Santa Rosalia", 
                                   "Lo"="Loreto",
                                   "Co"="Corredor", 
                                   "Lp"="La Paz",
                                   "Lv"="La Ventana",
                                   "CP"="Cabo Pulmo", 
                                   "CSL"="Los Cabos", 
                                   "IM"="Islas Marias", 
                                   "Revi"="Revillagigedo"))


trends %>% 
        ggplot(aes(x=date, y=sst_trend, col=region))+
        geom_line()+
        facet_wrap(~region)
env2 <- merge(env, trends)

