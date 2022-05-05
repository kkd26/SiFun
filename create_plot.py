#!/usr/bin/python3

import glob
import csv
import numpy as np
import matplotlib.pyplot as plt
import scipy.optimize

def readlog(log):
  with open(log, newline='') as csvfile:
    spamreader = csv.reader(csvfile, delimiter=',')
    arr=[[],[],[]]
    for row in spamreader:
      for i in range(len(row)):
        arr[i].append(float(row[i]))
    return arr

def mean (arr):
  return np.mean(arr, axis=0)

def stdErr (arr):
  return np.std(arr, axis=0) / np.sqrt(np.size(arr))

def removeIOTime (arr):
  m = np.sort(arr)
  return (m - m[0])

def monoExp(x, m, t, b):
    return m * np.exp(t * x) + b

def fit(xs, ys):
  p0 = (0, 1, 1) 
  params, cv = scipy.optimize.curve_fit(monoExp, xs, ys, p0)
  m, t, b = params
  return (m,t,b)

def fitSeries(arr, yerr, lab=""):
  n = len(arr)
  xs = np.arange(n)
  ys = arr
  (m,t,b) = fit(xs, ys)
  plt.errorbar(xs, ys, yerr=yerr, fmt = '.', label=lab+" data")
  xss = np.linspace(0,(n-1)*1.01)
  plt.plot(xss, monoExp(xss, m, t, b), '--', label=lab+" fitted")


logfiles = glob.glob("./*.log")

sifun = [readlog(log)[0] for log in logfiles]
haskell = [readlog(log)[1] for log in logfiles]

sifunMean = mean(sifun)
haskellMean = mean(haskell)

sifunStdErr = stdErr(sifun)
haskellStdErr = stdErr(haskell)

sifunMeanNoIO = removeIOTime(sifunMean)
haskellMeanNoIO = removeIOTime(haskellMean)

fitSeries(sifunMeanNoIO, sifunStdErr, "sifun")
fitSeries(haskellMeanNoIO, haskellStdErr, "haskell")

plt.legend(loc="upper left")
plt.title("Fitted Exponential Curves for SiFun and Haskell")
plt.xlabel("test size - binary tree height")
plt.ylabel("execution time [s]")
plt.show()