#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Tue Nov 17 11:21:39 2020

@author: isajarspector
"""

from thinkbayes import Pmf
pmf = Pmf()
for x in [1,2,3,4,5,6]:
 pmf.Set(x,1/6)
 
print (pmf.Prob(1))