% Lab: First-Order Logic
% CSC 261 
%
% File
%   grandpa.pl
%
% Summary
%   A collection of familial relationships defined through the given
%   four primitive relationships ( child, male, female, married)
%
% Provides
%   Definitions and rule for all the necessary relationships


% Part A

parent(P,C) :- child(C,P).
%Part B, Step I
parent(P,C) :- child(C,X), married(X,P).
parent(P,C) :- child(C,X), married(P,X).
  
father(F,C) :- male(F), parent(F,C).
mother(M,C) :- female(M), parent(M,C).
son(S,P)    :- male(S), child(S,P).
daughter(D,P) :- female(D), child(D,P).
grandfather(G,C) :- male(G), parent(G,X), parent(X,C).
grandmother(G,C) :- female(G), parent(G,X), parent(X,C).
grandson(S,G) :-male(S), child(S,X), child(X,G).
granddaughter(D,G) :- female(D), child(D,X), child(X,G).
wife(W,H) :- married(H,W).
husband(H,W) :- married(H,W).
sibling(X,Y) :- mother(M,X),mother(M,Y),father(F,X), father(F,Y).
brother(B,X) :- male(B), sibling(B,X).
sister(S,X) :- female(S), sibling(S,X).
uncle(U,X) :- male(U), sibling(U,Y), parent(Y,X).
aunt(A,X) :- female(A), sibling(A,Y), parent(Y,X).



% Part B, Step II
soninlaw(S,P) :- male(S), married(S,D), parent(P,D).

               
% Part B, Step III

male(n).
male(f).
male(s1).
male(s2).
    
female(d).    
female(w).
    
married(n,w).
married(f,d).
    
child(s1,n).
child(s1,w).
child(s2,f).
child(s2,d).
child(n,f).
child(d,w).
    
