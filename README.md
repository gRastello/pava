# pava
Pava is a tool that checks correctness of derivations in propositional logic.
It uses a derivation system similar to that presented in Andrea Iacona's lecture notes (that can be found somewhere [here](https://andreaiacona.academia.edu/)); see below for details.

The name is a joke on the name of a guy that likes formal derivations to a bit of an extreme extent.
This joke is way more obscure than your average 4chan meme (I estimate that something like less than 10 people can understand it) so if you happen to find a nice acronym (such as Propositional Assistent *something something*) please submit it.

## Installation instructions
Currently the only way of getting pava is to clone this repo and build it with stack (or cabal).
It may also be a good idea to checkout a particular version of pava (the latest) since development (if any) is done on the master branch.
```
git clone https://github.com/grastello/pava
cd pava
git checkout v0.1.0.0
stack build
```

## Using pava
Once you have a file `derivation.pava` (see below) checking it with pava is as simple as running
```
pava derivation.pava
```
or, if you do not feel like having pava in your PATH, you can run it through stack as follows.
```
stack exec pava derivation.pava
```

## The system
Pava supports all standard connectives (but negation (that will be added lated)) with rules for their introduction and elimination.

### Formulæ
Formulæ in the system are written in the typical way.
The following examples should clarify any doubt you might have.
```
¬(A∧¬B)
FOO∧BAR
foo∧bar
A⇒¬B
```
Pava is case sensitive and requires you to type the `∧`, `⇒` and `¬` as is (no `not`, `implies` or `and` for now) so you might want to rely on your editor's macro system to avoid a lot of copying and pasting.

### Steps
A derivation is a sequence of steps.
A step is not merely a formula but it is a combination of an id, a formula and a justification for the step.
It looks like this:
```
ID FORMULA RULE DEPENDENCIES;
```
Formulæ fill the `FORMULA` hole while `ID` is just a positive integer.
It makes sense to use increasing positive integers to label the steps of a derivation, however pava does not check that.
The only relevant thing is that there are no duplicate steps.

### Rules
Rules provide justification for steps.
They can take other steps' ids as arguments and make the step depend on others in different ways.

#### Assumption (`a`)
An assumption is just a formula that we say holds.
Assumption steps do not take arguments and do not depend on other steps (of course).
So if we want to assume that `A∧B` holds we write
```
n A∧B a;
```
where `n` is the step's id.

#### And introduction (`I∧`)
You can take the conjunction of two steps' formulæ. Suppose that, in your derivation, you have
```
n A R1 d1;
m B R2 d2;
```
where `A` and `B` stand for formulæ, `R1` and `R2` for rules, `d1` and `d2` for the list of dependencies of the two steps.
We can use the and introduction rule as
```
k A∧B I∧(n, m) d3;
```
where `k` is a new id and `d3` is a list that is the union of `d1` and `d2` plus it contains `n` if step `n` is an assumption and `m` is step `m` is an assumption.

##### Example
We show that form the assumption of `A` and `B` we can derive `A∧B`.
```
1 A a;
2 B a;
3 A∧B I∧(1, 2) 1, 2;
```

#### And elimination (`E∧`)
You can take apart a conjunction. If we have a step like
```
n A∧B R1 d1;
```
where `n`, `R1` and `d1` are interpreted as before then we can write a new step
```
k A E∧(n) d1;
```
or a new step
```
k B E∧(n) d1;
```
Notice that the new step, `k`,  depends on exactly the same assumptions as step `n`.

###### Example
From the assumption `A∧B` both `A` and `B` follows.
```
1 A∧B a;
2 A E∧(1) 1;
3 B E∧(1) 1;
```

#### Not introduction (`I¬`)
This is the most "complicated" rule and also the only one that allow us to discard an assumption.
Suppose we have the following three steps in a derivation
```
i A a;
j B R1 d1;
k ¬B R2 d2;
```
with `i`, `j`, `k`, `R1`, `R2`, `d1`, `d2` interpreted as before.
We can write down the following step
```
n ¬A I¬(i, j, k) d3;
```
where `n` is a new id and `d3` contains is union of `d1` and `d2` plus `j` or `k` if steps `j` or `k` happen to be assumptions *but* it does not contain `i`.
This last condition is the one that allow us to discard the assumption.

##### Example
We derive that `¬(A∧¬A)` is a tautology.
```
1 A∧¬A a;
2 A E∧(1) 1;
3 ¬A E∧(1) 1;
4 ¬(A∧¬A) I¬(1, 2, 3);
```
#### Not elimination (`E¬`)
This rule gets rid of double negations.
If we have a step
```
n ¬(¬A) R1 d1;
```
we can derive
```
m A E¬(n) d1;
```
that depends on the same assumptions of the staring step `n`.

##### Example
We derive `B` from a falsity `A∧¬A`.
```
1 A∧¬A a;
2 A E∧(1) 1;
3 ¬A E∧(1) 1;
4 ¬B a;
5 ¬(¬B) I¬(4, 2, 3) 1;
6 B E¬(5) 1;
```

#### Implication introduction (`I⇒`)
An assumption can be discarded into an implication.
Suppose we have
```
n A a;
m B R1 d1;
```
then the following is a valid step
```
k A⇒B I⇒(n, m) d2;
```
where `d2` is `d1` *minus* `n`.

##### Example
We can discard the assumption in the previous example to obtain a tautology (note that such tautology does not depend on *any* assumption).
```
1 A∧¬A a;
2 A E∧(1) 1;
3 ¬A E∧(1) 1;
4 ¬B a;
5 ¬(¬B) I¬(4, 2, 3) 1;
6 B E¬(5) 1;
7 A∧¬A ⇒ B I⇒(1, 6);
```
#### Implication elimination (`E⇒`)
An implication can be eliminated via the standard modus ponens rule.
If we have
```
n A⇒B R1 d1;
m A R2 d2;
```
then the step
```
k B E⇒(n, m) d3;
```
where `d3` is the is the union of `d1` and `d2` (plus `n` or `m` if they happen to be assumptions) is justified.

##### Example
```
1 A∧(A⇒B) a;
2 A E∧(1) 1;
3 A⇒B E∧(1) 1;
4 B E⇒(3, 2) 1;
5 A∧(A⇒B) ⇒ B I⇒(1, 4);
```

#### Or Introduction (`I∨`)
The simplest rule.
Given a step
```
n A R1 d1;
```
we can set a new step:
```
m A∨B I∨(n) d1;
```
where `B` is *any* formula.

##### Example
We derive one of the De Morgan's Laws (in one direction only).
```
1  ¬(A∧B) a;
2  ¬(¬A∨¬B) a;
3  ¬A a;
4  ¬A∨¬B I∨(3) 3;
5  ¬(¬A) I¬(3, 2, 4) 2;
6  A E¬(5) 2;
7  ¬B a;
8  ¬A∨¬B I∨(7) 7;
9  ¬(¬B) I¬(7, 2, 8) 2;
10 B E¬(9) 2;
11 A∧B I∧(6, 10) 2;
12 ¬(¬(¬A∨¬B)) I¬(2, 1, 11) 1;
13 ¬A∨¬B E¬(12) 1;
14 ¬(A∧B) ⇒ ¬A∨¬B I⇒(1, 13);
```

#### Or Elimination (`E∨`)
Suppose we have steps
```
n A∨B R1 d1;
m ¬A  R2 d2;
```
then the step
```
k B E∨(n, m) d3;
```
where `d3` is the union of `d1` and `d2` plus `n` and/or `m` if the associated steps are assumptions is justified.

##### Example
```
1 A∨B a;
2 ¬A a;
3 B E∨(1, 2) 1, 2;
```

## Known Issues
- The not-introduction and implication-introduction rules are bugged in version 0.1.0.0; everything is already fixed in this branch (and in the next version as well).
