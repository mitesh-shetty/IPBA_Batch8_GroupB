#!/usr/bin/env python
# coding: utf-8

# In[1]:


#import libraries here 
# Standard imports 
import os
import pandas as pd
import numpy as np
#import autoviz
import bamboolib as bam
from dataprep.eda import create_report
#from autoviz.AutoViz_Class import AutoViz_Class


# In[2]:


loc = 'C:\\Users\\sumit\\OneDrive\\My Drive\\IPBA course\\Projects\\Sales Prediction\\'
os.chdir(loc)


# In[2]:





# In[3]:


#Lets read the data
#fact_dataset = pd.read_csv('ConsumerElectronics.csv',sep=',', error_bad_lines=False, index_col=False, dtype='unicode') 


# In[6]:


create_report(fact_dataset)


# In[5]:


df = pd.read_csv(r'C:\Users\sumit\OneDrive\My Drive\IPBA course\Projects\Sales Prediction\ConsumerElectronics.csv', sep=',', decimal='.')
df['order_date'] = pd.to_datetime(df['order_date'], infer_datetime_format=True)
df['gmv'] = pd.to_numeric(df['gmv'], downcast='integer', errors='coerce')
df['s1_fact.order_payment_type'] = df['s1_fact.order_payment_type'].astype('string')
df['cust_id'] = df['cust_id'].astype('string')
df['pincode'] = df['pincode'].astype('string')
df['product_analytic_super_category'] = df['product_analytic_super_category'].astype('string')
df['product_analytic_category'] = df['product_analytic_category'].astype('string')
df['product_analytic_sub_category'] = df['product_analytic_sub_category'].astype('string')
df['product_analytic_vertical'] = df['product_analytic_vertical'].astype('string')
df


# In[7]:





# In[4]:


#plot GMV histogram
fig = px.histogram(df.dropna(subset=['gmv']), x='gmv')
fig


# In[10]:


#Plot the Month on histogram
fig = px.histogram(df, x='Month')
fig


# In[4]:


set(fact_dataset.product_analytic_category.to_list())


# In[9]:


set(fact_dataset.product_analytic_sub_category.to_list())


# In[ ]:


fig = px.line(df.dropna(subset=['gmv']), x='order_date', y='gmv')
fig


# In[9]:


import plotly.express as px
fig = px.box(df, x='gmv', y='product_analytic_category')
fig


# In[10]:


fig = px.box(df, x='gmv', y='Month', orientation='h')
fig

