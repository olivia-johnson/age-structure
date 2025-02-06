#!/usr/bin/env python3
# -*- coding: utf-8 -*-


path='/Users/olj5016/age-structure/'
import os
import sys
import pandas as pd

sim_type=sys.argv[1] ## define simulation type
pset=sys.argv[2]
results_dir=sys.argv[3]
sim_run=sys.argv[4]

#sim_type="JAsine"
#pset=1
#results_dir="/storage/home/olj5016/work/yuseob/"
#sim_run=1

params=pd.read_csv("{0}parameters.txt".format(path),index_col=[0])
parameters=params.iloc[pset]
j = parameters['J']
aS = parameters['as']
bS = parameters['bs']
aL = parameters['al']
bL = parameters['bl']
mr = parameters['u']
rr = parameters['c']
lam = parameters['lambda']
acutoff = parameters['maxAge']
k0 = parameters['K0']
eS = parameters['es']


results = "outpath='" + str(results_dir)+ "'"
cmd = 'slim -d "' +str(results)+ '" -d J='+ str(int(j))+" -d as=" + str(aS) + " -d bs=" + str(bS)+" -d al=" + str(aL)+" -d bl=" + str(bL)+" -d sim_run=" + str(sim_run) + " -d u=" + str(mr) + " -d c=" + str(rr)+ " -d lambda=" + str(lam) + " -d agecutoff=" + str(int(acutoff)) + " -d K0=" + str(k0) + " -d es="+ str(eS) + " /storage/home/olj5016/work/age-structure/"+str(sim_type)+".slim"
print(cmd)
os.system(cmd) ## run command