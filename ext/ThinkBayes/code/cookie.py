"""This file contains code for use with "Think Bayes",
by Allen B. Downey, available from greenteapress.com

Copyright 2012 Allen B. Downey
License: GNU GPLv3 http://www.gnu.org/licenses/gpl.html
"""
from __future__ import print_function

from thinkbayes import Pmf

pmf = Pmf()
#   Prior
pmf.Set('Bowl 1', 0.5)
pmf.Set('Bowl 2', 0.5)
# Prior * Likelihood
pmf.Mult('Bowl 1', 0.75)
pmf.Mult('Bowl 2', 0.5)
# Normalize (divide by evidence)
pmf.Normalize()

print(pmf.Prob('Bowl 1'))
