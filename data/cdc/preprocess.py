# Preprocess CDC data downloaded from
# https://data.cdc.gov/Case-Surveillance/COVID-19-Case-Surveillance-Public-Use-Data-with-Ge/ynhu-f2s2
# on 2021-10-28 at 14:37 CDT

import pandas as pd
df = pd.read_csv('data/cdc/COVID-19_Case_Surveillance_Public_Use_Data_with_Geography.csv',
                 dtype='category')
df['month'] = pd.to_datetime(df['case_month'], format='%Y-%m', errors='coerce')
df = df.loc[df.month<=pd.to_datetime('2/1/2021'),
            ['case_month','res_state','age_group','sex','race','ethnicity',
             'death_yn']].reset_index(drop=True)
df.to_feather('data/cdc/preprocessed.lz4', compression='lz4')
