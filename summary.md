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

The heuristic driven search is a beam search, with beam width $b$ (where
$b = 3$ in the original paper), and a heuristic of description length of the
grammar, as well as the length of the derivations of the sample in that
grammar.

At each iteration, the algorithm applies one of two operations: _MERGE_ or
_EXTRACT_, both of which increase the size of the language.

 * _MERGE_ takes two non-terminals, and combines them into one, with the rules
   of both. Replacing instances of both non-terminals with that of the new
   one.
 * _EXTRACT_ takes a sequence that appears multiple times in the right hand
   sides of the rules of the grammar, and creates a new non-terminal mapping
   to that sequence. Replacing the sequences in the right-hand sides of the
   existing rules with the new non-terminal.

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

Which is exactly the definition of an NTS language as used in [Clark06]
below.

An additional restriction of this algorithm is that we cannot provide negative
examples to deal with underfitting. We rely on the fact that the operations do
not "generalise too much".

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
  \State \Return $\min _ {P \in \mathcal{P}}{\{P | \Call{Cost}{P} \}}$
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
respective operations on all pairs of non-terminals and substrings in rules
respectively until all such operations yield no better potential grammars.

At each stage, the set of potential grammars is limited in size to the beam
width $b = 3$.

The cost function here rewards compactly defined grammars with short derivation
trees for the positive samples given. Such a grammar represents a good
generalisation of the sample given.

\subsection{Learning k-bounded context-free grammars\\
  {\small Angluin, 1987}}

### Restrictions

There is only one restriction on the context-free grammars in this algorithm,
and that is that they must be _k-bounded_. A grammar
$G = (\Sigma, V, \mathcal{P}, S)$ is _k-bounded_ if for all
$(A \rightarrow \gamma) \in \mathcal{P}$ there are at most $k$ non-terminals
in $\gamma$.

This algorithm is restricted further, however, by its requirements of the
teacher. The teacher must be able to perform the following queries (In each
case, the teacher either returns _yes_ or provides a counter-example):

 * **Non-terminal Membership**, for a given $w \in \Sigma^{ * }$ and $A \in V$,
   does $A \Rightarrow^{ * } w$ hold?
 * **Equivalence** Given a grammar $G^\prime$ is $L(G) = L(G^\prime)$?

The last query in particular poses a difficulty in that it is not a decidable
problem, although the paper suggests altering the algorithm to uses a form of
probabilistic equivalence to get around this issue.

### Algorithm

In the algorithm, _PARSE_ returns the parse tree of a grammar for a given
word.

Additionally, any procedures handled by the oracle/teacher are starred.

\begin{algorithm}
\caption{Learning k-bounded grammars}
\begin{algorithmic}
\Function{Learn}{}
  \State $P^\prime \gets \emptyset$
  \While{$\lnot\Call{Equal$^\ast$}{P^\prime}$}
    \State $w^\prime \gets \Call{Counter-example$^\ast$}{}$
    \If {$w^\prime \in L(G^\prime)$}
      \State $tree \gets \Call{Parse}{G^\prime, w^\prime}$
      \State $P \gets P - \{\Call{Diagnose}{tree}\}$
    \Else
      \State $P \gets P \cup \Call{Candidate}{w^\prime}$
    \EndIf
  \EndWhile
  \State \Return $G^\prime$
\EndFunction
\end{algorithmic}
\end{algorithm}

\begin{algorithm}
\caption{Diagnose a bad parse}
\begin{algorithmic}
\Function{Diagnose}{T}
  \LineComment{\textbf{input} A parse tree, for a false-positive string.}
  \LineComment{\textbf{output} A bad production in $G^\prime$}
  \ForAll{children $(T^\prime, x)$ of $T$}
    \If {$\lnot\Call{Member$^\ast$}{T^\prime, x}$}
      \Comment{is the child bad?}
      \State \Return $\Call{Diagnose}{T^\prime}$
    \EndIf
  \EndFor

  \State \Return $T$
\EndFunction
\end{algorithmic}
\end{algorithm}

\begin{algorithm}
\caption{Candidate rules for generating the missing string.}
\begin{algorithmic}
\Function{Candidate}{w}
  \LineComment{\textbf{input} A string not currently in $L(G^\prime)$}
  \LineComment{\textbf{output} A set of candidate productions}
  \State $C \gets \emptyset$
  \ForAll{substrings $y$ of $w$}
    \For{$m = 0 \ldots k$}
      \ForAll{$y = x_0y_0 \ldots x_my_mx_{m+1}$}
        \ForAll{$(A, A_0,\ldots,A_m) \in V^{m+1}$}
          \State $C \gets C \cup \{A \rightarrow x_0A_0 \ldots x_mA_mx_{m+1}\}$
        \EndFor
      \EndFor
    \EndFor
  \EndFor
  \State \Return $C$
\EndFunction
\end{algorithmic}
\end{algorithm}

\subsection{PAC-Learning Unambiguous NTS Languages\\
  {\small Clark, 2006}}

### Restrictions
As well as the NTS restriction as found (implicitly) in [Langley00], there is
a restriction that the target language be unambiguous.

Additionally, in inspecting the algorithm, we see that it produces rules in
Chomsky normal form. Whilst this does not affect the expressibility, it could
have implications on real world performance.

Furthermore, the polynomial bounds this algorithm guarantees rely on the
samples given to it coming from a probabilistic context-free grammar based
on the target language.

### Definitions

\begin{align*}
\cdot \sqsubseteq \cdot &\eqdef \text{substring relation} \\
L _ \infty(F) &\eqdef \max _ {x \in X}{|F(x)|} \text{, for function $F$, countable set $X$.} \\
[w] &\eqdef \text{component containing $w$ in $(U,E)$ (see algorithm)} \\
\hat{C_u} &\eqdef \text{multiset of contexts containing $u$.} \\
\mu_3 & = \frac{\nu\mu_2}{16}
\end{align*}

### Algorithm

Intuitively, the algorithm finds commonly occurring substrings in the sample
set, and the contexts they occur in. Then, produces a graph with edges between
substrings that appear in similar contexts. From this graph, we create the
grammar:

 * Each connected component in the graph is considered to be a
   non-terminal
 * A rule is added for every member of the terminal alphabet
 * For every substring $w \in U$ we find, we add a rule for each splitting of
   $w = uv$ from its component $[w]$ to the components of the splitting
   $[u][v]$.
 * The starting non-terminals are precisely the components of the strings in
   the sample set.

\begin{algorithm}
\caption{\textbf{PACCFG} algorithm}
\begin{algorithmic}
\Function{Learn}{$W$, $m_0$, $\nu$, $\mu_2$, $\Sigma$}
  \LineComment{\textbf{input} $W$ a set of positive examples}
  \LineComment{\textbf{input} $m_0$, $\nu$, $\mu_2$ paramaters for the PAC algorithm}
  \LineComment{\textbf{input} $\Sigma$ the terminal alphabet}
  \LineComment{\textbf{output} $\hat{G}$ the grammar}
  \State
  \State $U \gets \{u \in \Sigma^+ : |\{w_i:u\sqsubseteq w_i\}| \geq m_0\}$
  \ForAll{$u \in U$}
    \State $\hat{C_u} \gets \emptyset$
    \ForAll{$w_i \in W$}
      \If{$u \sqsubseteq w_i$}
        \ForAll{$l, r : lur = w_i$}
          \State $\hat{C_u} \gets \hat{C_u} \cup \{(l,r)\}$
        \EndFor
      \EndIf
    \EndFor
  \EndFor
  \State
  \State $U_c \gets \{u \in U : L _ \infty(\hat{C _ u}) > \frac{\mu_2}{2}\}$
  \State $E \gets \{(u_i,u_j) \in U _ c^2 :
                    L_\infty(\hat{C_{u_i}} - \hat{C_{u_j}}) < 2\mu_3 \}$
  \State $\hat{V} \gets \text{the connected components of graph } (U,E)$
  \State $\hat{P} \gets \{[\alpha] \rightarrow \alpha : \alpha \in \Sigma\} \cup
                        \{[w] \rightarrow [u][v] : w \in U, w = uv\}$
  \State $\hat{S} \gets \{[w] : w \in W\}$
  \State
  \State \Return{$(\Sigma, \hat{V}, \hat{P}, \hat{S})$}
\EndFunction
\end{algorithmic}
\end{algorithm}
