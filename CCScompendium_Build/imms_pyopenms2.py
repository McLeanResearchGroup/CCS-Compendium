# -*- coding: utf-8 -*-
""" Exctract and match Ion Mobility data

"""

import sys, time
import pyopenms
import numpy as np
import csv


def main():

    if len(sys.argv) < 3:
       raise RuntimeError("need arguments: input.mzML query.csv")

    input_fname = sys.argv[1]
    query_fname = sys.argv[2]

    input_spectra = pyopenms.MSExperiment()
    fh = pyopenms.FileHandler()

    print(input_fname + " " + query_fname)
    fh.loadExperiment(str.encode(input_fname), input_spectra)

    rt = 0.0
    mslevel = 0

    # read in three column file rt_min, rt_s, m/z
    with open(query_fname, 'r') as f:
        reader = csv.reader(f, quoting=csv.QUOTE_NONNUMERIC)
        rt_mz = list(reader)
        rt_mz.sort(key = lambda x: x[2]) # sort list by m/z

    for i in range(input_spectra.getNrSpectra()):
      s = input_spectra.getSpectrum(i)

      if i == 0: 
        rt = s.getRT()
        mslevel = s.getMSLevel()

      s_drift_time = s.getDriftTime()

      (mz, intens) = s.get_peaks()
      
      for search_rt_min, search_rt, search_mz in rt_mz:
          if abs(s.getRT() - search_rt) < 30.0:
            for m in mz:
                if abs(search_mz - m) < 5*1e-6*search_mz:
                    print str(s.getRT()) + "\t" + str(m) +"\t" + str(s_drift_time) 


# toto store last (partial) block 

if __name__ == "__main__":
  main()
