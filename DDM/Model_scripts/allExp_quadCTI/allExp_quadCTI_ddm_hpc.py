#! /usr/bin/env python
# code for ddm

from time import sleep

import os
import sys
import hddm
import kabuki
import pandas as pd

#%% define model parameters 
# get input from command line and check its length
cl = sys.argv[1:]
assert len(cl) == 1

# assign to some parameters
experiment = sys.argv[1:]

# define and(or) make all the folders
DATA_DIR = "/data/gent/438/vsc43896/tasktran_ddm/allExp_quadCTI"
folder_name   = "{0}".format(experiment)

# the full path
NEWDIR   = os.path.join(DATA_DIR, folder_name)
if not os.path.exists(NEWDIR):
    os.makedirs(NEWDIR)

#%% importing data
data = hddm.load_csv('raw_data/allExp_CTIquad_ddm_data.csv')

#%% running the ddm model

ddm_models = []
ddm_postsamples = []

# defining the effect of interest
effect = {"t ~ 1 + (CTI_linear + CTI_quad) * C(block_type, Treatment('RG'))",
          "v ~ 1 + (CTI_linear + CTI_quad) * C(block_type, Treatment('RG'))",
          "a ~ 1 + (CTI_linear + CTI_quad) * C(block_type, Treatment('RG'))"}


for i in range(4): # running 4 chains
    # fit the ddm model
    m = hddm.HDDMRegressor(data.query('exp == @experiment'),
                           effect,
                           p_outlier = 0.05,
                           group_only_regressors=False)
    
    print("the experiment is:", experiment)
    ## m.find_starting_values()  ## the commmand may cause error
    # append database and 
    dbn = "{0}_{1:1d}.db".format(experiment, int(i))
    dbn_fullname = os.path.join(NEWDIR, dbn)

    m.sample(5000, burn=1000, dbname=dbn_fullname, db='pickle')
    ddm_models.append(m)
    
    # extract the posterior distribution of all parameters and concatenate them into a array
    postsample = m.get_traces()
    ddm_postsamples.append(postsample)
    
    # extract ppc and append
    # ppc = hddm.utils.post_pred_gen(m, samples = 5)
    # stimtran_ppcs.append(ppc)
    
gelman = hddm.analyze.gelman_rubin(ddm_models)
gelmanDF = pd.DataFrame.from_dict(gelman, orient = 'index')
gelmanDF_name = "gelman_rubin_{0}.csv".format(experiment)
gelmanDF_fullpath = os.path.join(NEWDIR, gelmanDF_name)
pd.DataFrame.to_csv(gelmanDF, gelmanDF_fullpath)

ddm_cat_models = kabuki.utils.concat_models(ddm_models)
ddm_cat_models_name = "hddm_model_{0}".format(experiment)
ddm_cat_models_pullpath = os.path.join(NEWDIR, ddm_cat_models_name)
ddm_cat_models.save(ddm_cat_models_pullpath)

postsamples = pd.concat(ddm_postsamples)
postsamples_name = "postsamples_{0}.csv".format(experiment)
postsamples_pullpath = os.path.join(NEWDIR, postsamples_name)
postsamples.to_csv(postsamples_pullpath, encoding='utf-8', index=False)

# ppc_samples = pd.concat(stimtran_ppcs)
# ppc_samples.to_csv('ppc/ppc_samples.csv', encoding='utf-8', index=False)

sleep(10)

