library(tidyverse)
library(lubridate)
library(plotly)
library(patchwork)

ohlc_data <- read.csv('base_data.csv')%>%
  mutate(date = ymd_hms(date))%>%
  mutate(date_date = date(date),date_time = format(date,format = '%H:%M:%S'))%>%
  select(1:9)

tb <- read.csv('tb.csv')%>%
  mutate(Buy_date = ymd(Buy_date))


orb_data <- 
  ohlc_data %>% 
  #mutate(orb_flag = ifelse(date_time >='09:15:00' & date_time <= "09:45:00",T,F))%>%
  filter(date_time >= '09:15:00', date_time <='09:45:00')%>%
  group_by(ticker,date_date)%>%
  mutate(orb_high = max(high),orb_low=min(low),orb_vol = sum(volume),orb_open = first(open),orb_close=last(close))%>%
  select (ticker,date_date,orb_high,orb_low,orb_vol,orb_open,orb_close)%>%
  unique()


ohlc_joined <- ohlc_data %>%
  left_join(.,
            orb_data,
            by = c('ticker'='ticker','date_date'='date_date'))%>%
  arrange(ticker)%>%
  mutate(buy_flag = ifelse(close >= orb_high & close >30 & close < 2000,'Y','N'),
         sell_flag = ifelse(close <= orb_low & close >30 & close < 2000,'Y','N'),
         vol_flag = ifelse(volume >= 2*orb_vol,'Y','N' ))
  

#### to be added to python algo to same data processing - done
ohlc_joined %>% 
  mutate(buy_flag = ifelse(buy_flag == 'Y',1,0))%>%
  group_by(date_date,ticker)%>%
  mutate(n = sum(buy_flag))%>%
  filter(n==0)


### 
ohlc_mb_highlight <- ohlc_joined %>% 
  select(date_date,ticker,orb_close,orb_open)%>%
  mutate(adv_dec = ifelse(orb_close > orb_open,"A","D"))%>%
  unique()%>%
  group_by(date_date,adv_dec)%>%
  summarise(n=n())%>%
  spread(.,key = adv_dec,value = n)%>%
  mutate(mb = A-D)%>%
  select(date_date,mb)


ohlc_joined %>% 
  select(date_date,ticker,orb_close,orb_open)%>%
  mutate(adv_dec = ifelse(orb_close > orb_open,"A","D"))%>%
  unique()%>%
  group_by(date_date,adv_dec)%>%
  summarise(n=n())%>%
  spread(.,key = adv_dec,value = n)%>%
  mutate(mb = A-D)
  filter(n != 'NA',adv_dec == 'A')%>%
  ggplot(aes(date_date,n))+geom_line()+
  geom_point(data= ohlc_mb_highlight, aes(x=date_date,y=n),color='red') ->g

ggplotly(g)

orb_range_pct <-ohlc_joined %>% 
  mutate(orb_range_pct = (orb_high - orb_low)/orb_open)%>%
  select(date_date,ticker,orb_range_pct)%>%
  unique()%>%
  arrange(desc(orb_range_pct))
  
tb %>% 
  left_join(.,orb_range_pct,
            by = c('Buy_date'='date_date','Ticker'='ticker'))%>%
  #filter(orb_range_pct < 0.03)%>%
  summarise(positive = sum(Realised_pnl[Realised_pnl > 0]),negative = sum(Realised_pnl[Realised_pnl < 0]),total = positive+negative)
  
  ggplot(aes(orb_range_pct,Realised_pnl))+geom_point()
  
  

tb %>% 
  left_join(.,ohlc_mb_highlight,by = c('Buy_date'='date_date'))%>%
  group_by(Buy_date)%>%
  summarise(pnl = sum(Realised_pnl),mb=unique(mb))%>%
  #filter(n >=20)%>%
  ggplot(aes(mb,pnl))+geom_point()+geom_smooth()
  

ggplot(aes(Buy_date,pnl,label = n))+geom_col()+geom_text()

tb %>% 
  group_by(Buy_date)%>%
  summarise(pnl = sum(Realised_pnl))%>%
  mutate(cum_pnl = cumsum(pnl))%>%
  ggplot(aes(Buy_date,cum_pnl))+geom_line() ->p1

p1 <- ggplotly(p1)

tb %>% 
  left_join(.,ohlc_mb_highlight,by = c('Buy_date'='date_date'))%>%
  group_by(Buy_date)%>%
  summarise(mb = unique(mb))%>%
  ggplot(aes(Buy_date,mb))+geom_line() ->p2

p2 <- ggplotly(p2)

subplot(p1,p2,nrows=2)



calculate_pnl <- function(min,max){

  tb %>% 
    left_join(.,ohlc_mb_highlight,by = c('Buy_date'='date_date'))%>%
    group_by(Buy_date)%>%
    summarise(pnl = sum(Realised_pnl),mb=unique(mb))%>%
    filter(mb >min,mb <max)%>%
    summarise(sum(pnl))
  
}

calculate_pnl(-10,10)

tb %>% 
  left_join(.,ohlc_mb_highlight,by = c('Buy_date'='date_date'))%>%
  group_by(Buy_date)%>%
  summarise(pnl = sum(Realised_pnl),mb=unique(mb))%>%
  filter(!is.na(mb))%>%
  summarise(min(mb),max(mb))

pnl_scenarios <- crossing(mb_1= -18:22,mb_2=-18:22)%>%
  rowwise()%>%
  mutate(pnl = calculate_pnl(mb_1,mb_2))

pnl_scenarios %>% 
  filter(pnl$`sum(pnl)` !=0)%>%
  #filter(mb_1 < -5,mb_2 > 10)%>%
  ggplot(aes(mb_1,mb_2,fill=pnl$`sum(pnl)`))+geom_tile()



#### Probability of tight opening range resulting in large break out rally ?  1% - 3 % range gives max result 


ohlc_joined %>% 
  group_by(ticker,date_date)%>% 
  mutate(day_high = max(high),day_close = last(close),orb_range_pct = (orb_high - orb_low)/orb_open)%>%
  mutate(day_change_high = (day_high - orb_open)/orb_open,day_change_close = (day_close - orb_open)/orb_open,
         day_orb_open_high = (day_high - orb_high)/day_high )%>%
  ungroup()%>%
  filter(buy_flag=='Y')%>%
  select(date_date,ticker,orb_range_pct, day_change_close,day_change_high,day_orb_open_high)%>%
  unique()%>%
  filter(orb_range_pct < 0.03, orb_range_pct > 0.01 )%>%
  ggplot(aes(orb_range_pct,day_change_close))+geom_point()+
  geom_smooth()


### Time SL - does it make sense to get out of a trade if price does not move after taking a position for n intervals 


ohlc_joined %>% 
  mutate(orb_range_pct = (orb_high - orb_low)/orb_open)%>%
  group_by(ticker,date_date)%>%
  mutate(buy_price= ifelse(buy_flag=='Y',first(close),0),
         sell_price = ifelse(sell_flag =='Y',first(close),0))%>%
  group_by(ticker,date_date)%>%
  mutate(day_high = max(high),day_close = last(close),day_low = min(low),day_open=first(open),
         day_change_close = (day_close - day_open)/day_open)%>%
  group_by(ticker,date_date)%>%
  mutate(remarks = case_when(any(buy_flag =='Y') ~ "Long",
                             any(sell_flag =='Y') ~ "Short",
                             TRUE ~ "No trade"))%>%
  mutate(entry_price = case_when(remarks =='Long' ~ buy_price,
                           remarks =='Short'~sell_price,
                           remarks == "No trade" ~ 0))%>%
  mutate(entry_price_change = case_when(remarks =='Long' ~ (day_close - buy_price)/buy_price,
                                        remarks =='Short'~(day_close - sell_price)/sell_price,
                                        remarks == "No trade" ~ 0))%>%
  filter(orb_range_pct < 0.03, orb_range_pct > 0.01 ,remarks %in% c('Long','Short'))%>%
  select(ticker,remarks,entry_price_change)%>%
  unique()%>%
  ggplot(aes(entry_price_change,color=remarks,fill=remarks))+geom_density(alpha =0.1)+
  geom_vline(xintercept = 0,colr='red',linetype='dotted')+
  facet_wrap(~ticker)

  

ohlc_joined %>% 
  mutate(day_high = max(high),day_close = last(close),orb_range_pct = (orb_high - orb_low)/orb_open)%>%
  mutate(day_change_high = (day_high - orb_open)/orb_open,day_change_close = (day_close - orb_open)/orb_open,
         day_orb_open_high = (day_high - orb_high)/day_high )%>%
  filter(buy_flag=='Y')%>%
  select(date_date,ticker,orb_range_pct, day_change_close,day_change_high,day_orb_open_high)%>%
  mutate(remarks = ifelse(day_change_close>0,'Positive','Negative'))%>%
  mutate(day_change_close = ifelse(day_change_close<0,-1*day_change_close,day_change_close))%>%
  unique()%>%
  filter(orb_range_pct < 0.03, orb_range_pct > 0.01 )%>%
  ggplot(aes(remarks,day_change_close))+geom_boxplot()
  
  




  