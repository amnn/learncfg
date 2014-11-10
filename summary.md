---
title: Summary of Papers
author: Ashok Menon
date: November 10, 2014
...

\subsection{Learning Context-Free Grammars with a Simplicity Bias\\
  {\small Langley \& Stromsten, 2000}}

This algorithm works only from positive examples. At its heart it is a
heuristic driven search through the space of context-free grammars that
can generate the strings in the sample set.

Starting from a trivial grammar: The one in which there is a rule
$S \rightarrow w$, for all $w$ in the sample, we produce successively more
general grammars, stopping when the grammar cannot be made more general (using
the operations described below).

The heuristic driven search is a beam search, with beam $b$ (where $b = 3$ in
the original paper), with a heuristic of description length of the
grammar, as well as the length of the derivations of the sample in that
grammar.

At each iteration, the algorithm applies one of two operations: _MERGE_ or
_EXTRACT_, both of which increase the size of the language.

 * _MERGE_ takes two non-terminals, and combines them into one, with the rules
   of both.
 * _EXTRACT_ takes a sequence that appears multiple times in the right hand
   sides of the rules of the grammar, and creates a new non-terminal mapping
   to that sequence.

### Restrictions

Implicit in the definition of this algorithm is, the restriction on
$G = (\Sigma,V,\mathcal{P},S)$ that:

$$
\forall \alpha, \beta, \gamma \in \Sigma^*.
\forall A, B \in V.
  A \Rightarrow^* \alpha\beta\gamma
  \land B \Rightarrow^* \beta
  \Leftrightarrow A \Rightarrow^* \alpha B \gamma
$$

Which is exactly the definition of an NTS language as used in [Clark2006] below.

An additional restriction of this algorithm is that we cannot provide negative
examples to deal with underfitting. We rely on the fact that the operations do
not "generalise too much".

### Algorithm

\begin{algorithm}
\caption{Learning with a simplicity bias}
\begin{algorithmic}
\Function{Learn}{$R$}
  \LineComment{\textbf{input} $R \subseteq \Sigma^{ * }$, a subset of positive examples}
  \LineComment{\textbf{output} $P$, a set of productions}

  \State $\mathcal{P} \gets
            \{\{S \rightarrow w | w \in R\} \cup
            \{\alpha \rightarrow \alpha | \alpha \in \Sigma\}\}$

  \State
  \State $changed \gets \mathbf{true}$
  \While{changed}
    \State $(changed_m,\mathcal{P}) \gets \Call{Merge}{\mathcal{P}}$
    \State $(changed_e,\mathcal{P}) \gets \Call{Extract}{\mathcal{P}}$
    \State $changed \gets changed_m \vee changed_e$
  \EndWhile

  \State
  \State \Return $\min_{P \in \mathcal{P}}{\{P | \Call{Cost}{P} \}}$
\EndFunction
\end{algorithmic}
\end{algorithm}

\begin{algorithm}
\caption{Complexity of the grammar in terms of description length}
\begin{algorithmic}
\Function{Cost}{$G$}
  \LineComment{\textbf{input} $G$ a potential grammar.}
  \LineComment{\textbf{output} The complexity of the grammar w.r.t $R$.}

  \State
  \State $C \gets log(|\Call{Non-Terminals}{G}||\Call{Terminals}{G}|)$
  \ForAll{$w \in R$}
    \State $C \gets C + |\Call{Rules}{G}|\Call{Derivation-Length}{G, w}$
  \EndFor
  \State
  \State \Return $C$
\EndFunction
\end{algorithmic}
\end{algorithm}

The _MERGE_ and _EXTRACT_ methods used in _LEARN_ repeatedly perform their
respective operations on all pairs of non-terminals and substrings respectively
until all such operations yield no better potential grammars.

At each stage, the set of potential grammars is limited in size to the beam
width $B = 3$.

\subsection{A method for inferring context-free grammars\\
  {\small Knobe \& Knobe, 1976}}

### Restrictions

### Algorithm

\subsection{Learning k-bounded context-free grammars\\
  {\small Angluin, 1987}}

### Restrictions

### Algorithm

\subsection{PAC-Learning Unambiguous NTS Languages\\
  {\small Clark, 2006}}

### Restrictions

### Algorithm

\subsection{Efficient learning of context-free grammars from positive structural examples\\
  {\small Sakakibara, 1992}}

### Restrictions

### Algorithm
