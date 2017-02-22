# Cleaning up the Basque grammar: a work in progress

## Simple tools

### Tagset compacting

Original definition:  
`ADPOSAGADJ = ("ageri" ADJ ABS MG) ("herabe" ADJ ABS MG) ("maite" ADJ ABS MG) ("nabari" ADJ ABS MG) ("gaitzi" ADJ ABS MG) ("bizi" ADJ ABS MG) ("haizu" ADJ ABS MG) ("laket" ADJ ABS MG) ("atsegin" ADJ ABS MG) ;`  
Compact version:  
`ADPOSAGADJ = ("ageri"|"herabe"|"maite"|"nabari"|"gaitzi"|"bizi"|"haizu"|"laket"|"atsegin") + (ADJ ABS MG) ;`  

This transformation is purely for human eyes.

***

Original definition:  
`BADA = ("<bada>") ("<Bada>"<HAS_MAI>") ("<BADA>"<DEN_MAI>") ;`  
Compact version:  
`BADA = ("<bada(>\"<..._MAI)?>"ir) ;`  

If we apply this transformation to all wordforms, not just those that have a HAS_MAI/DEN_MAI variant in the grammar, 
we may improve the coverage: transforming a set like `WORD = "<word>"` into the case-insensitive version `WORD = ("<word(>\"<..._MAI)?>"ir)` makes any rule that uses `WORD` to match all orthographical varieties of *word*. Downside is that maybe sometimes the grammar writer actually meant only uppercase or only lowercase.

After we have transformed all sets into regular expressions, including inline sets in rules, we can get rid of some redundant rules. For instance, the following three rules

```
SELECT (ADB) IF (0 ("<berez>") LINK 0 DET);
SELECT (ADB) IF (0 ("<Berez>\"<HAS_MAI>") LINK 0 DET);
SELECT (ADB) IF (0 ("<BEREZ>\"<DEN_MAI>") LINK 0 DET);
```

can all be compacted to a single rule: `SELECT (ADB) IF (0 ("<berez(>\"<..._MAI)?>"ir) LINK 0 DET);`.

***

### Grouping and sorting rules

After these steps, we group all the rules that have roughly the same
targets, and sort them by their contextual tests: first by the
number of contexts, and then by the length of the tagsets in contexts.
Below is an example result of such an operation:

```
SELECT ADOIN IF (1 ARAZI) ;
REMOVE ADOIN IF (0 ADJ) (1C ADJ) ;
REMOVE ADOIN IF (0 IZE) (1C ADJ) ;
REMOVE ADOIN IF (0 ADJ) (1C ADI) ;
SELECT ADOIN IF (0 IZE) (1C ADL1) ;
REMOVE ADOIN IF (0 IZE) (1 DET | ADJ | IZE) ;
REMOVE ADOIN IF (0 EZEZAG + ADJ + GEN) (-2C IZE) ;
REMOVE ADOIN IF (0 IZE) (-1C IZE) (1C ADJ) ;
...
```

#### Add some SAT to it

Then we run the conflict checking tool on just these lists of rules, in
the given order. If it says that some rule with a more complex condition is
superfluous, because of another rule earlier in the list, then that's
a hint for the grammar writer to think why did they write these two rules
in the first place. I think this is actually more helpful than checking 
the rules in order, if the aim is to create a grammar that is nicer to read.

#### Generate looser or stricter versions of the rules

In the spirit of Bick 2014, we can manipulate the rules: remove or add a C into the contexts, or remove some context altogether. Any further manipulation of rules is probably not useful, and may confuse the grammarian who revises the grammar.

## Complex tools

After grouping and sorting, a grammarian may notice some weirdness in the grammar, and remove or change some rules (or introduce new ones). While grouping rules by target may help the grammar writer, it may not be the optimal order to run the grammar. For this, we can use the ML-tuning tool by Bick (2014), and the SAT-based tools by Listenmaa and Claessen (2016).

Run Bick's tool, get a new order; run SAT-based conflict checker to that order, and see if it introduced something weird.
