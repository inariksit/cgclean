# Cleaning up the Basque grammar: a work in progress

## Introduction


The Basque CG was originally written in 19XX, using the CG-1
formalism. Since then, it has undergone many changes, by many
grammarians. The file consists of 8600 lines, out of which 2600 are
rules or tagsets--rest are comments and commented out rules or sets.
During the two decades of development, the Basque morphological
analyser has also been updated several times, and not always
synchronised with the CG. As a result, the Basque grammar needs
serious attention.

In the present paper, we describe the ongoing process of
cleaning up the Basque grammar. We use a variety of tools and
methods, ranging from simple string replacements to SAT-based symbolic
evaluation, introduced in (Listenmaa & Claessen 2016), and grammar
tuning by Bick (2014). We present our experiences in combining all these
tools, along with a few modest additions to the simpler end of the scale.

## Previous work

We use the tools presented in Bick (2014) and Listenmaa & Claessen
(2016). 

Bick (2014) presents a method for tuning a grammar, based on machine
learning. Bick reports an improvement of ... when tested on ... grammar.

Listenmaa and Claessen (2016) present a method for detecting
contradictions in a grammar, using SAT-based symbolic
evaluation. They report that the tool detects rule conflicts in a few small
grammars, but no further evaluation on the effects of the accuracy of
the grammar.

We see both tools complementing each other.

## Pipeline

Draw a picture: `[simple] --> [sort&group] --> [SAT & linguist check] --> [ML-tuning]`

(and possibly back to SAT and linguist check from ML-tuning)

### Simple tools

(Some of these feel like too simple to mention, but maybe it can be
useful for someone?)

#### String replacements/otherwise extremely simple
* Fix typos

* Remove unused sets (vislcg3 --show-unused-sets)

* Transform word forms into case-insensitive & remove duplicates. There were many occurrences of identical rules, of the  form `REMOVE ("<x>") IF ...` and `REMOVE ("<X>") IF ...`. We replaced those rules into the form `REMOVE ("<x>"i)`, and removed duplicate rules after that.

* Tagset compacting

Original definition:  
`ADPOSAGADJ = ("ageri" ADJ ABS MG) ("herabe" ADJ ABS MG) ... ("atsegin" ADJ ABS MG) ;`  
Compact version:  
`ADPOSAGADJ = ("ageri"|"herabe"|"maite"|"nabari"|"gaitzi"|"bizi"|"haizu"|"laket"|"atsegin") + (ADJ ABS MG) ;`  

This transformation is not automated -- we just output it for inspiration.

#### Compare all tags againts a current lexicon

Fairly simple process: get all the tags from an up-to-date morphological lexicon, then
gather all tags from the grammar, and compare against the list. Output all the tags that are not found in the morphological lexicon.

#### Check if a set name is used in place of a tag name

For example:

```
LIST ADI/ADT = ADI ADT ;
LIST ADIZEHAR = ("galde" ADI) ("jakin" ADI/ADT)
```

We read the grammar line by line, and keep track of the previously read LIST names and
definitions. When we read a new LIST definition (or an inline list),
we check if the tag that appears inside parentheses appears /only/ as
a LIST name. If there is no occurrence on the right hand side of LIST
definitions, we output a warning.

### Sort, group and give to a linguist

After we have done these transformations, we group the rules by their
targets, and sort them by the complexity of their contextual
texts. For instance, the following 5 rules that target ADOIN will be
in the following order:

```
SELECT ADOIN IF (1 ARAZI) ;                           # line 7412
REMOVE ADOIN IF (0 IZE) (1C ADJ) ;                    # line 6423
REMOVE ADOIN IF (0 IZE) (1 DET | ADJ | IZE) ;         # line 6433
REMOVE ADOIN IF (0 EZEZAG + ADJ + GEN) (-2C IZE) ;    # line 6319
REMOVE ADOIN IF (0 IZE) (-1C IZE) (1C ADJ) ;          # line 6422
```

When the rules are in this order, we run the SAT-based symbolic
evaluation on them. If it says that some rule with a more complex condition is
superfluous because of another rule earlier in the list, then that is
a hint for the grammar writer: why are these two rules in the grammar,
if the simpler would do?

Even if the SAT-method wouldn't find a conflict, we give the rules to
a grammarian in any case. The grammarian works with this list, having
the original grammar on the side to see the comments, or other
original context of any given rule. Seeing all the rules grouped helps
with the situation where different grammarians have written rules
independent of each other. The grammarian can do their manual
modification either on the original grammar, or to this grouped
version.

After grouping and sorting, a grammarian may notice some weirdness in
the grammar, and remove or change some rules (or introduce new
ones). While grouping rules by target may help the grammar writer, it
may not be the optimal order to run the grammar. For this, we can use
the ML-tuning tool by Bick (2014), and the SAT-based tools by
Listenmaa and Claessen (2016).

This sorting and grouping is not meant to be the final order, it is
only to help a human grammarian to make decisions regarding all the
rules that target the same tagsets.

### ML-tuning

Once the grammar is cleaned up a bit, we give it to the ML-tuning tool
by Bick (2014), with the purpose of finding an optimal order.

TODO: run these experiments!

After this order, we can run the SAT-based tool again, and notice if
the ML-tuning has introduced new conflicts or superfluous rules. 


### Why the pipeline and not only ML-tuning?

Let us give a motivating example. 

(line 0023) `LIST DEK = ABL ABS ... PAR PRO SOZ ;`  
(line 1167) `#LIST DEK = DEK ;`  
(line 3094) `MAP (@KM>) TARGET (ZERO) IF ... (1 DEK + (MG)) ;`  
(line 4711) `SELECT DEK IF (0 ("<bat>")) (NOT -1 DEK) (NOT 1 (DEK ABS)) ;`  

These lines tell a story. Once upon a time, there used to be a tag
called DEK, which grammarian 1 included in the grammar, along with the
rule `SELECT (DEK)` on line 4711. Sometime later in the development,
grammarian 2 came along, commented out the obsolete list definition
for DEK, and used the name DEK for another list. The rule on line 3094
is written with DEK as a list name, not as a tag name.

Our initial hypothesis was: "This grammar is in too bad a shape to
give it to Eckhard. We want to clean it up ourselves first."

TODO: test ML-tuning with original grammar vs. ML-tuning with pre-cleaned grammar!


## Evaluation

Evaluation criteria:
* F-score
* Human evaluation of readability


Original grammar
Original grammar + ML-tuning
Original grammar + sort-group-(SAT)-linguist
Original grammar + sort-group-(SAT)-linguist + ML-tuning

