#!python

from numpy import sum,arange,log,array,sqrt,concatenate,ceil
from scipy.special import polygamma
#from nsb_entropy import S,dS,make_nxkx
from numpy.random import choice,random_integers
from nltk import FreqDist
from scipy.stats import binom

def Extrapolate(imatrix,r,method="Chao2",k=10):
	if method =="Chao2":
		Sest = S_Chao2(imatrix)
	elif method=="ICE":
		Sest = S_ICE(imatrix,k=k)
	else:
		Sest = S_JK2(imatrix)
	R = float((imatrix.shape)[1])
	fd = sum(imatrix,1)
	Sobs = len([f for f in fd if f>0])
	Q1 = float(len([f for f in fd if f==1.0]))
	Q0 = Sest - Sobs
	return Sobs + Q0*(1.0-(1.0-Q1/(Q1+R*Q0))**r)

def S_JK2(imatrix):
	_,R = imatrix.shape
	fd = sum(imatrix,1) # sums the rows
	fd = fd[fd>0]
	Q1 = float(len(fd[fd==1]))
	Q2 = float(len(fd[fd==2]))
	Sobs = len(fd)
	res = Sobs + ((2*R-3.0)/R)*Q1-((R-2.)**2/(R*(R-1.)))*Q2
	return res

def S_Chao2(imatrix):
	_,R = imatrix.shape
	fd = sum(imatrix,1) # sums the rows
	fd = fd[fd>0]
	Q1 = float(len(fd[fd==1]))
	Q2 = float(len(fd[fd==2]))
	Sobs = len(fd)
	if Q2>0.0:
		res = Sobs + ((R-1.0)/R)*((Q1**2.0)/(2.0*Q2))
	else:
		res = Sobs + ((R-1.0)/R)*Q1*(Q1-1)/2.0
	return res

def S_ICE(imatrix,k=10):
	S,R = imatrix.shape
	fd = sum(imatrix,1) # sums the rows
	rareP = [i for i in range(S) if fd[i]<=k]
	Ri = 0.
	for i in range(R):
		for j in rareP:
			if imatrix[j,i]:
				Ri+=1.
				break
	li = [f for f in fd if f<=k]
	lf = [f for f in fd if f>k]
	fi = FreqDist(li)
	ff = FreqDist(lf)
	Si = len(li)
	Sf = len(lf)
	Q1 = float(len([f for f in li if f==1]))
	ssi = float(sum(li))
	Ci = 1.0 - Q1/ssi
	g2 = (Si/Ci) * (Ri/(Ri-1.0))\
				*float(sum([f*(f-1)*fi[f] for f in fi]))/\
				(ssi*(ssi-1.0))- 1.0
	if g2<0:
		g2 = 0
	return Sf + Si/Ci + Q1*g2/Ci

def UnseenCov(fd):
	f1 = float(len([f for f in fd if f==1]))
	f2 = float(len([f for f in fd if f==2]))
	N = float(sum(fd))
	# Undetected species & Coverage
	if f2>0.:
		f0 = ceil(((N-1.)/N)*(f1**2.)/(2.*f2))
		C = 1.-f1/N * (N-1.)*f1/((N-1.)*f1 + 2.*f2)
	else:
		f0 = ceil(((N-1.)/N)*f1*(f1-1.)/2.)
		C = 1.-f1/N * (N-1.)*f1/((N-1.)*f1 + 2.)
	return (f0,C)

# BootstrapFD: Implements the procedure for bootstrapping
#							 frequency distributions from a sample described
#						   by SI2 of Chao, Wang, & Jost (2013)
# Parameters:
#			samp : original sample
# Value:
#			an infinite iterator, the 1st distribution returned is the 
#     frequency distribution of the original sample
#			the following ones are bootstrap resamples
def BootstrapFD(samp):
	fd = FreqDist(samp)
	f1 = float(fd.Nr(1))
	f2 = float(fd.Nr(2))
	N = float(fd.N())
	B = fd.B()
	# Undetected species & Coverage
	if f2>0.:
		f0 = ceil(((N-1.)/N)*(f1**2.)/(2.*f2))
		C = 1.-f1/N * (N-1.)*f1/((N-1.)*f1 + 2.*f2)
	else:
		f0 = ceil(((N-1.)/N)*f1*(f1-1.)/2.)
		C = 1.-f1/N * (N-1.)*f1/((N-1.)*f1 + 2.)
	#Correct abundances
	probs = array(fd.values())/N
	lambdah = (1-C)/sum(probs*(1-probs)**N)
	probs = probs*(1-lambdah*(1-probs)**N)
	#P for unseen
	#paux = (1-C)/f0
	yield fd.values()
	popO = arange(B)
	dist = binom(n=N,p=1-C)
	probsA = probs/sum(probs)
	while True:
		ns2 = dist.rvs()
		ns1 = int(N)-ns2
		if ns1>0:
			samp1 = list(choice(popO,size=ns1,replace=True,p=probsA))
		else:
			samp2 = []
		if ns2>0:
			samp2 = list(random_integers(B,B+int(f0)-1,ns2))
		else:
			samp2 = []
		yield FreqDist(samp1+samp2).values()

# Entropy: Approximates entropy for a list of frequency counts
# Parameters:
# 	counts : list with the counts obseved for each outcome. Can contain zeros
# 	method : possible methods (defaults to "ML")
#			"ML": Maximum likelihood (i.e., unsmoothed) entropy estimate (severely underestimates)
#			"JamesStein": James-Stein Shrinkage method. (plugin method)
#			"ChaoShen": Chao-Shen method
#			"CWJ":	Chao-Wang-Joost method
#			"NBRS":	Nemenman-Bialek-de Ruyter van Setveninck method (NSB for N<<M)
#			"NSB":	Nemenman-Shafee-Bialek method
#	K : Total number of possible outcomes (relevant for NSB and JamesStein methods). If None, it is taken to be the
#		length of the counts vector
#	std : Should the method return the standard error of the entropy counts (works only for NSB). Defaults to false
#	t : Only relevant for the "JamesStein" method. Alternative prior distribution (array of floats)
# Value
# Usually it returns just an entropy estimate. If std is set to True and method is set to "NSB", it returns a pair whose first
# element is the entropy and the second element is the standard error
def Entropy(raw_counts, method="ML", K=None, std=False, lambdaF=None, t=None):
	counts = array([float(elem) for elem in raw_counts.values()])
	if len(counts) == 0:
		return 0.0
	elif method == "ChaoShen":
		return ChaoShen(counts)
	elif method == "CWJ":
		return ChaoWangJoost(counts)
	elif method == "NBRS":
		s,ds = NBRS(counts)
		if std:
			return (s,ds) 
		else:
			return s
	elif method == "JamesStein":
		c2 = counts
		if K>len(counts):
			c2 = counts + [0 for i in range(K-len(counts))]
		return JamesSteinShrink(c2,t=t,lambdaF=lambdaF)
	elif method == "JamesStein2" and len(counts)<K:
		return JamesSteinShrink2(counts,K=K)
	elif method == "NSB":
		if K==None:
			K = len(counts)
		ccounts = array(counts)
		NK = make_nxkx(ccounts, K)
		s = S(NK, ccounts.sum(), K)
		ds = sqrt(dS(NK, ccounts.sum(), K)-s**2.0)
		if std:
			return (s,ds)
		else:
			return s
	else:
		return EntropyML(counts)

# FreqShrink: James-Stein shrinkage frequency smoothing
# Parameters:
# 	counts : list with the counts obseved for each outcome. Can contain zeros
#	lambdaF: Allows to set the shrinkage proportion to a prespecified value. If set to None, the optimal value is used.
#	t : Alternative prior distribution (array of floats). If set to None (default), it uses the uniform distribution
#	rCounts: Should the output probabilities be rescaled to the original scale of the counts (i.e., get smoothed counts)
# Value:
# An array with the smoothed probabilities(if rCounts = False) or smoothed counts (if rCounts = True)
def FreqShrink(counts,lambdaF=None,rCounts=False,t=None):
	if t==None:
		t = 1. / float(len(counts))
	N = float(sum(counts))
	p = array(counts)/N
	if lambdaF==None:
		if N <= 1.:
			lambdaF = 1.
		else:
			lambdaF = (1. - sum(p**2.))/((N-1.)*sum((t-p)**2.))
		if lambdaF<0. or lambdaF>1.:
			lambdaF = 1.
	p_shrink = lambdaF*t + (1.-lambdaF)*p
	if rCounts:
		p_shrink *= N
	return p_shrink

# JS_KullbackLeibler: Kullback-Leibler dovergence between James-Stein smoothed counts
# Parameters:
# 	counts1 : list with the counts obseved for each outcome. Can contain zeros
# 	counts2 : list with the counts obseved for each outcome. Can contain zeros. Must have the same length as counts2
#	lambdaF: Allows to set the shrinkage proportion to a prespecified value. If set to None, the optimal value is used.
#	t : Alternative prior distribution (array of floats). If set to None (default), it uses the uniform distribution
def JS_KullbackLeibler(counts1,counts2,lambdaF=None,t=None):
	p1 = FreqShrink(counts1,lambdaF=lambdaF,t=t)
	p2 = FreqShrink(counts2,lambdaF=lambdaF,t=t)
	return sum(p1*log(p1/p2))
	
# JS_JensenShannon: Jensen-Shannon divergence between James-Stein smoothed counts
# Parameters:
# 	counts1 : list with the counts obseved for each outcome. Can contain zeros
# 	counts2 : list with the counts obseved for each outcome. Can contain zeros. Must have the same length as counts2
#	lambdaF: Allows to set the shrinkage proportion to a prespecified value. If set to None, the optimal value is used.
#	t : Alternative prior distribution (array of floats). If set to None (default), it uses the uniform distribution
def JS_JensenShannon(counts1,counts2,lambdaF=None,t=None):
	p1 = FreqShrink(counts1,lambdaF=lambdaF,t=t)
	p2 = FreqShrink(counts2,lambdaF=lambdaF,t=t)
	mid = (p1+p2)/2.
	return (sum(p1*log(p1/mid)) + sum(p2*log(p2/mid)))/2.

def EntropyML(counts):
	freqs = [c for c in counts if c>0]
	n = sum(freqs)
	p = array(freqs)/float(n)
	return -sum(p*log(p))

def JamesSteinShrink(counts,t=None,lambdaF=None):
	p = FreqShrink(counts,t=t,lambdaF=lambdaF)
	return -sum(p[p>0]*log(p[p>0]))

def JamesSteinShrink2(counts,K=None):
	N = float(sum(counts))
	K0 = float(len(counts))
	p = array(counts)/N
	t = 1.0/K
	if N <= 1.:
		lambdaF = 1.
	else:
		lambdaF = (1. - sum(p**2.))/((N-1.)*(sum((t-p)**2.)+(K-K0)*t**2))
	if lambdaF<0. or lambdaF>1.:
		lambdaF = 1.
	p = lambdaF*t + (1.-lambdaF)*p
	return -sum(p*log(p))-(K-K0)*lambdaF*t*log(lambdaF*t)
	
def ChaoShen(counts):
	freqs = [c for c in counts if c>0]
	f1 = float(len([x for x in freqs if x==1]))
	n  = float(sum(freqs))
	C = 1. - f1/n
	# ML probabilities
	p = array(freqs)/n
	# Coverage adjustment
	if C>0.:#(can only be done if the coverage is positive, otherwise probs become zero)
		p = C*p
	return -sum(p * log(p)/(1. - (1.-p)**n))

def ChaoWangJoost(counts):
	freqs = [c for c in counts if c>0]
	n  = float(sum(freqs))
	f1 = float(len([x for x in freqs if x==1]))
	f2 = float(len([x for x in freqs if x==2]))
	if f2>0:
		A = 2. * f2 / ((n - 1.)*f1 + 2. * f2)
	elif f1>0:
		A = 2. / ((n - 1.)*(f1 - 1.) + 2.)
	else:
		A = 1.
	v = array(freqs)
	v = v[v<n]
	R = sum([_CWJ_aux(x,n) for x in v])
	r = arange(1,n)
	if A != 1.0:
		R -= f1*(log(A)+sum(((1.-A)**r)/r))*((1.-A)**(1.-n))/n
	return R
	
# Global chart to avoid computing many times the same term, speeds things up dramatically
_CWJ_Chart = {}

def _CWJ_aux(Xi,n):
	if not _CWJ_Chart.has_key((Xi,n)):
		_CWJ_Chart[Xi,n] = Xi * sum(1./arange(Xi,n))/n
	return _CWJ_Chart[Xi,n]


################ NSB for very large K (Nemenman-Bialek-de Ruyter van Steveninck)

#Nemenman, Ilya, Bialek, William & de Ruyter van Steveninck, Rob (2004) "Entropy and information in neural spike trains: Progress on the sampling problem". Phys. Rev. E 69, 056111

# Euler-Mascheroni constant
Euler = -polygamma(0,1.)

def NBRS(counts):
	N = float(sum(counts))
	freqs = [c for c in counts if c>0]
	f1 = sum([x for x in freqs if x==1])
	Delt = N - f1
	if Delt>0.:#(can only be done if there are repetitions, psi(Delta) becomes infinite)
		S = Euler - log(2.) + 2.*log(N) - polygamma(0,Delt)
		dS = sqrt(polygamma(1,Delt))
		return (S,dS)
	else: # defaults back to ML
		return EntropyML(counts)  
