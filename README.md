# FTR
## Financial Transmission Right (Transmission Congestion Contrac) Market for the NYISO  
   
Modeling the returns to monthly TCCs from 2010 through 2016 within the NYISO region. Data is first acquired from NYISO and St. Louis Federal Reserve and saved to a local database (user machine). The data is transformed to calculate profits and returns to investment. The transformed data is then analyzed with a CAPM model and GARCH to deteremine the alpha's of monthly TCCs.

### I. Loading Data  

- load_dam_data.r      
Load day ahead market data.   
- load_tcc.data.r   
Load transmission congestion contract data.   
- load_daily_dam_to_DB.r   
Load daily day ahead market data and save to local postgresql database.
- load_transform_data.r    
Load already transformed data (load > transform and save > load transformed)
- Exploratory_DataSources.r  
Scratch space for initial exploratory analysis and work.
   
   
### II. Transforming Data   

- Returns_to_TCC.R   
Calculating profits and returns to TCCs   
   
    
### III. Analysis   
- Garch_modeling.r   
Capital asset returns modeling accounting for time and network effects on congestion.   

   
### Author  
Please contact author with questions or comments.
