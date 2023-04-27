import pandas as pd
import os
import numpy as np
from numpy import mean, arange
from numpy import std
import pprint
import matplotlib.pyplot as plt
from matplotlib import pyplot
import seaborn as sns
from sklearn.model_selection import train_test_split
import statsmodels.api as sm
from sklearn.metrics import mean_squared_error
from sklearn.metrics import mean_absolute_error
from sklearn.metrics import accuracy_score
from sklearn.metrics import log_loss
from sklearn.model_selection import RepeatedKFold, RepeatedStratifiedKFold
from sklearn import ensemble, datasets, tree
from sklearn.tree import plot_tree
from sklearn.model_selection import cross_val_score
from sklearn.model_selection import GridSearchCV
from collections import OrderedDict
from sklearn.ensemble import RandomForestRegressor
from sklearn.inspection import permutation_importance

data = pd.read_csv("C:\\Users\\Ellie\\Documents\\UMass\\SNiP\\RS_Fusion\\modis30_Pitt_matchup_pull.csv")

data.rename(columns = {'green_mean':'green_M', 'blue_mean':'blue_M', 
                     'red_mean':'red_M', 'swir1_mean':'swir1_M', 'swir2_mean':'swir2_M', 
                     'nir_mean':'nir_M'}, inplace = True)

#Add band ratios
data_ratio = data.assign(NR = data['nir_M']/data['red_M'],
                BR_M = data['blue_M']/data['red_M'],
                GR_M = data['green_M']/data['red_M'],
                SR_M = data['swir1_M']/data['red_M'],
                BG_M = data['blue_M']/data['green_M'],
                RG_M = data['red_M']/data['green_M'],
                NG_M = data['nir_M']/data['green_M'],
                SG_M = data['swir1_M']/data['green_M'],
                BN_M = data['blue_M']/data['nir_M'],
                GN_M = data['green_M']/data['nir_M'],
                RN_M = data['red_M']/data['nir_M'],
                SN_M = data['swir1_M']/data['nir_M'],
                BS_M = data['blue_M']/data['swir1_M'],
                GS_M = data['green_M']/data['swir1_M'],
                RS_M = data['red_M']/data['swir1_M'],
                NS_M = data['nir_M']/data['swir1_M'],
                R_GN_M = data['red_M']/ (data['green_M'] + data['nir_M']),
                R_GB_M = data['red_M']/ (data['green_M'] + data['blue_M']),
                R_GS_M = data['red_M']/ (data['green_M'] + data['swir1_M']),
                R_BN_M = data['red_M']/ (data['blue_M'] + data['nir_M']),
                R_BS_M = data['red_M']/ (data['blue_M'] + data['swir1_M']),
                R_NS_M = data['red_M']/ (data['nir_M'] + data['swir1_M']),
                G_BR_M = data['green_M']/ (data['blue_M'] + data['red_M']),
                G_BN_M = data['green_M'] / (data['blue_M'] + data['nir_M']),
                G_BS_M = data['green_M'] / (data['blue_M'] + data['swir1_M']),
                G_RN_M = data['green_M'] / (data['red_M'] + data['nir_M']),
                G_RB_M = data['green_M'] / (data['red_M'] + data['blue_M']),
                G_NS_M = data['green_M'] / (data['nir_M'] + data['swir1_M']),
                B_RG_M = data['blue_M'] / (data['red_M'] + data['green_M']),
                B_RN_M = data['blue_M'] / (data['red_M'] + data['nir_M']),
                B_RS_M = data['blue_M'] / (data['red_M'] + data['swir1_M']),
                B_GN_M = data['blue_M'] / (data['green_M'] + data['nir_M']),
                B_GS_M = data['blue_M'] / (data['green_M'] + data['swir1_M']),
                B_NS_M = data['blue_M'] / (data['nir_M'] + data['swir1_M']),
                N_RG_M = data['nir_M'] / (data['red_M'] + data['green_M']),
                N_RB_M = data['nir_M'] / (data['red_M'] + data['blue_M']),
                N_RS_M = data['nir_M'] / (data['red_M'] + data['swir1_M']),
                N_GB_M = data['nir_M'] / (data['green_M'] + data['blue_M']),
                N_GS_M = data['nir_M'] / (data['green_M'] + data['swir1_M']),
                N_BS_M = data['nir_M'] / (data['blue_M']  + data['swir1_M']),
                
                GR2_M = (data['green_M'] + data['red_M']) / 2,
                GN2_M = (data['green_M'] + data['nir_M']) / 2,
                #blooms
                BR_G_M = (data['blue_M'] - data['red_M']) / data['green_M'],
                NS_NR_M = (data['nir_M'] - data['swir1_M']) / (data['red_M'] - data['swir1_M']),
                fai_M = data['nir_M'] - (data['red_M'] + (data['swir1_M']-data['red_M'])*((830-660)/(1650-660))),
                # fai = (nir_M - red_M) + (red_M -swir) * (830-660)/(1648-660)
                N_S_M= data['nir_M'] - data['swir1_M'],
                N_R_M = data['nir_M']- data['red_M'],
                #
                ndvi_M = ((data['nir_M']-data['red_M'])/(data['nir_M']+data['red_M'])),
                ndwi_M = ((data['green_M']- data['swir1_M'])/(data['green_M'] + data['swir1_M'])),
                ndssi_M = ((data['blue_M'] - data['nir_M'])/ (data['blue_M'] + data['nir_M'])),
                gn_gn_M= ((data['green_M']- data['nir_M'])/ (data['green_M'] + data['nir_M'])),
                Matchup_M = 'Fusion')

import plotly.express as px
dfScatter = data[['blue_M', 'green_M','nir_M', 'red_M','swir1_M','swir2_M']].dropna(axis=0) #dropping na just in case here again
fig = px.scatter_matrix(dfScatter)
fig.show()