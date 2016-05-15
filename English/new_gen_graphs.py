from nltk import data
from nltk.corpus.reader import CHILDESCorpusReader
from nltk.probability import FreqDist
import entropies7.entropies7 as Ent
from numpy import *
import scipy as sp
import numpy as np
import re
import os as os

from pandas import *
import matplotlib.pyplot as plt
from matplotlib.font_manager import FontProperties

def get_files(corpus):
    # Inputs the name of the corpus 
    # Outputs:
    # corpus_root: string local path to corpora /.../nltk_data/corpora/childes/[corpus]
    # eng: CHILDESCorpusReader tool
    # files: lsit of name of files in corpus_root
    # data_directory: string location of future output data
    corpus_root = data.find('corpora/childes/English-UK-MOR')
    eng = CHILDESCorpusReader(corpus_root, 'Manchester/.*.xml')
#     if(corpus_root == "/Users/dspoka/nltk_data/corpora/childes/English-UK-MOR"):
        # CHANGE THIS
        #data_directory = "/Users/dspoka/Desktop/moscoso/nltk_childes/NLTKCHILDES/"
#     else:
    data_directory = "/Users/jeremyirvin/Desktop/SeniorThesis/Childes/nltk_childes/English/data_directory/"
    files = eng.fileids()
    return corpus_root, eng, files, data_directory
# %timeit files = get_files('childes')
corpus_root, eng, files, data_directory = get_files('manchester')
nmmfile = data_directory + "morph-eng.csv"
nmsfile = data_directory + "syntax-eng.csv"
# childes has 804 files

# Read data as a ssv (space-separated file)

df = pandas.read_csv(nmmfile, delimiter= ' ')

# Sort data by children alphabetically, then age increasing
sorted_df = df.sort(columns=['Child', 'Age'])

sorted_df.to_csv(data_directory + "sorted-morph-eng.csv", sep = ' ');

# Get list of the names of the children
name_list = Series(sorted_df['Child']).unique()

# Sort the name list
sorted_name_list = sorted(name_list)

# Get list of the names of the columns
column_list = list(sorted_df.columns.values)

# Remove the column name 'Child' because it contains strings
column_list.remove('Child')

# Convert all other columns to floats
sorted_df[column_list] = sorted_df[column_list].astype(float)

# Create a list of data frames corresponding to each child
child_split_df = {}
for child in sorted_name_list:
    child_split_df[child] = sorted_df[sorted_df['Child'] == child]

color_list = [(1.0, 0.0, 0.0), (0.0, 1.0, 0.0), (0.0, 0.0, 1.0), (1.0, 1.0, 0.0), (1.0, 0.0, 1.0), (0.0, 1.0, 1.0), (0.0, 0.0, 0.0), (0.0, 0.5019607843137255, 0.0), (0.5019607843137255, 0.0, 0.5019607843137255), (0.5019607843137255, 0.5019607843137255, 0.5019607843137255), (0.7529411764705882, 0.0, 0.0), (1.0, 0.6470588235294118, 0.0), (1.0, 0.6470588235294118, 0.0), (1.0, 0.6470588235294118, 0.0), (1.0, 0.6470588235294118, 0.0), (1.0, 0.6470588235294118, 0.0), (1.0, 0.6470588235294118, 0.0), (1.0, 0.6470588235294118, 0.0), (1.0, 0.6470588235294118, 0.0), (1.0, 0.6470588235294118, 0.0), (1.0, 0.6470588235294118, 0.0), (1.0, 0.6470588235294118, 0.0), (1.0, 0.6470588235294118, 0.0)]
# Plot the data nicely    
for x in range(4):
	if(x == 0):
		measurement_child = "N.child"
		measurement_mother = "N.mother"
		measurement_name = "Number of Words Used"
	elif(x == 1):
		measurement_child = "H.child.S"
		measurement_mother = "H.mother.S"
		measurement_name = "Lexical Diversity"
	elif(x == 2):
		measurement_child = "H.child.I"
		measurement_mother = "H.mother.I"
		measurement_name = "Inflectional Diversity"
	elif(x == 3):
		measurement_child = "Schild"
		measurement_mother = "Smother"
		measurement_name = "MLU"
	measurement_list = [measurement_mother, measurement_child]
	title = "Evolution of " + measurement_name
	# i = 0
	plt.figure(figsize=(25,15))
	plt.title(title, fontsize = 35, y = 1.03)
	xlab= plt.xlabel('Age (days)', fontsize = 25, labelpad = 15)
	ylab = plt.ylabel(measurement_name, fontsize = 25, labelpad = 15)

	lines = []
	for j in range(len(measurement_list)):
		i = 0
		for child in sorted_name_list:
		    line, = plt.plot(child_split_df[child]["Age"], child_split_df[child][measurement_list[j]], linestyle= '-' if j == 0 else '-', color=color_list[i % len(color_list)], label=child if j == 1 else child + " mother", linewidth = 3.5)
		    lines.append(line)
		    i += 1;

	plt.tick_params(axis='both', which='major', labelsize=23, pad = 15)   
	plt.legend(handles=lines, loc = 4, prop={'size':17})

	if(x == 0):
		plt.savefig('nwords_evolution.png')
	elif(x == 1):
		plt.savefig('lexical_evolution.png')
	elif(x == 2):
		plt.savefig('inflectional_evolution.png')
	elif(x == 3):
		plt.savefig('syntactic_evolution.png')

# 5s to run