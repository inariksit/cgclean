All numbers are using gold standard where punctuation is marked <Correct!>

Original grammar, with Inari's typofixes:  
 iteration 0, R=92.305, P=68.449, F=78.606  
 iteration 1, R=94.036, P=69.754, F=80.095  
 iteration 4, R=93.731, P=72.698, F=81.885  

Original-with-typofixes grammar sorted (shorter contexts first):  
* All in one section  
iteration 0, R=89.057, P=65.073, F=75.198  
iteration 1, R=94.162, P=69.724, F=80.12  
iteration 2, R=93.355, P=73.04,  F=81.957  
iteration 3, R=93.999, P=72.485, F=81.851  
iteration 4, R=94.065, P=72.367, F=81.801  
iteration 5, R=94.133, P=72.271, F=81.765  

* Different sections for different targets  
 iteration 0, R=89.008, P=65.114, F=75.208  
 iteration 1, R=94.162, P=69.724, F=80.12  
 iteration 2, R=93.48,  P=73.036, F=82.002  
 iteration 3, R=94.168, P=72.382, F=81.85  
 iteration 4, R=94.033, P=72.566, F=81.916  
 iteration 5, R=94.076, P=72.466, F=81.868   
 
Original grammar sorted (longer contexts first):
* All in one section -- takes less time to run  
 iteration 0, R=89.057, P=65.073, F=75.198  
 iteration 1, R=94.162, P=69.724, F=80.12  
 iteration 2, R=93.355, P=73.04,  F=81.957  
 iteration 3, R=93.999, P=72.485, F=81.851  
 iteration 4, R=94.065, P=72.367, F=81.801  
 iteration 5, R=94.133, P=72.271, F=81.765  

* Different sections for different targets  
 iteration 0, R=89.008, P=65.114, F=75.208  
 iteration 1, R=94.162, P=69.724, F=80.12  

***

Initial cleanup by JM & Itziar:  
 iteration 0, R=92.707, P=64.017, F=75.735  
 iteration 1, R=94.406, P=64.221, F=76.441  
 iteration 2, R=93.538, P=66.966, F=78.052  
 iteration 3, R=94.102, P=66.673, F=78.047  
 
 TODO: get more mature cleanup, sort + delexicalise that

***

Delexicalised original grammar:  
 iteration 0, R=58.716, P=50.705, F=54.417  
 iteration 1, R=82.053, P=70.181, F=75.654  
 iteration 3, R=82.872, P=70.955, F=76.451  

Delexicalised only those that were ("lex" + nonlex):  
 iteration 0, R=88.275, P=67.051, F=76.212  
 iteration 1, R=91.32, P=69.44, F=78.891  
 iteration 4, R=91.791, P=71.126, F=80.147  

Delexicalised ("lex" + nonlex) and sorted (shorter contexts first):  
 iteration 0, R=89.2, P=64.784, F=75.056  
 iteration 1, R=94.123, P=68.776, F=79.477  
 iteration 2, R=93.361, P=72.119, F=81.376  
