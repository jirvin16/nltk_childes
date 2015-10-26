from numpy import *
import scipy as sp
from pandas import *
import matplotlib.pyplot as plt
# from rpy2.robjects.packages import importr
# import rpy2.robjects as ro
# from rpy2.robjects import pandas2ri
# from rpy2.robjects import r
# pandas2ri.activate()

# # df_iris = com.load_data('iris') replaced with
# r.data('iris')
# # df_iris = pandas2ri.ri2py(r[name])

# # com.convert_to_r_dataframe(df) replaced with
# # pandas2ri.py2ri(df)

# # com.convert_robj(rdf) replaced with 
# # pandas2ri.ri2py(rdf)

# from nltk import data
# from nltk.corpus.reader import CHILDESCorpusReader
# from nltk.probability import FreqDist
# corpus_root = data.find('corpora/childes/Eng-USA-MOR')
# eng = CHILDESCorpusReader(corpus_root, 'Manchester/.*.xml')
nmmfile = "NLTKCHILDES/morph-eng.res"
# nmsfile = "NLTKCHILDES/syntax-eng.res"
# files = eng.fileids()
# # Group files by age, child, and corpus
# resDict = {}
# for f in files:
#     age = eng.age(f,month=True)[0] 
#     child = f.split("/")[1]
#     if resDict.has_key((child,age)):
#         resDict[child,age] += [f]
#     else:
#         resDict[child,age] = [f]

# import entropies7.entropies7 as Ent

# fout = open(nmmfile,"w")
# print >> fout, "Child Age N.child H.child H.child.S N.mother H.mother H.mother.S" 
# for child,age in resDict.keys():
#     ff = resDict[child,age]
#     # Frequency dists of child and mother for this particular child and age (raw) 
#     # Bottleneck of runtime
#     fchild = FreqDist(eng.words(ff,speaker="CHI",replace=True))
#     fmother = FreqDist(eng.words(ff,speaker="MOT",replace=True))
#     # Frequency dists of child and mother for this particular child and age (stemmed) # Notice that we split words on "-" to transform "be-3S" to "be"
#     # Bottleneck of runtime
#     fchildS = FreqDist([w.split("-")[0] for w in eng.words(ff,speaker="CHI",replace=True)])
#     fmotherS = FreqDist([w.split("-")[0] for w in eng.words(ff,speaker="MOT",replace=True)])
#     # Statistics
#     nchild = sum(array(fchild.values()))
#     nmother = sum(array(fmother.values()))
#     # Entropies
#     Hchild = Ent.Entropy(fchild,method="CWJ")
#     Hmother = Ent.Entropy(fmother,method="CWJ")
#     # Entropies (stemmed)
#     HchildS = Ent.Entropy(fchildS,method="CWJ")
#     HmotherS = Ent.Entropy(fmotherS,method="CWJ")
#     print >> fout, child,age,nchild,Hchild,HchildS,nmother,Hmother,HmotherS
    
# fout.close()

df = pandas.read_csv("NLTKCHILDES/morph-eng.csv", delimiter= ' ')
sorted_df = df.sort(columns=['Age'])
name_list = Series(sorted_df['Child']).unique()
column_list = list(sorted_df.columns.values)
column_list.remove('Child')
sorted_df[column_list] = sorted_df[column_list].astype(float)
child_split_df = {}
for child in name_list:
    child_split_df[child] = sorted_df[sorted_df['Child'] == child]
    plt.plot((child_split_df[child])['Age'],  (child_split_df[child])['H.child'], linestyle = '-'   )
plt.show()
