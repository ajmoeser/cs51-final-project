# cs51-final-project
SAT solver implementation for CS51

Guide to Repository Files:

main.ml : The mail DPLL algorithm and high-level helper functions

variable.ml: The interface and implementation for variables

clause.ml : The interface and implemenation for clauses

formula.ml : The interface and implementation for formulas

full_file.ml : A temporary file encompassing all definitions created due
    to yet-to-be-resolved problems with the interaction among modules.


Sources:

The following sources provided pseudo-code or discussions that were helpful in writing this project:

Javier Larrosa et al., “Satisfiability: Algorithms, Applications and 
Extensions” (2010), available at http://sat.inesc-id.pt/~ines/sac10.pdf

Paolo Liberatore, Lecture Notes for Automated Reasoning (2014/2015), 
available at http://www.dis.uniroma1.it/~liberato/ar/dpll/dpll.html

Viorica Sofronie-Stokkermans, Lecture Notes for Selected Topics In 
Automated Reasoning (May 14, 2009), available at 
http://people.mpi-inf.mpg.de/~sofronie/lecture-ar-09/slides/lecture-14-may.pdf