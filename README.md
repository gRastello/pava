# pava
Pava is a tool that checks correctness of derivations in propositional logic.
It uses a derivation system similar to that presented in Andrea Iacona's lecture notes (that can be found somewhere [here](https://andreaiacona.academia.edu/)); see below for details.

The name is a joke on the name of a guy that likes formal derivations to a bit of an extreme extent.
This joke is way more obscure than your average 4chan meme (I estimate that something like less than 10 people can understand it) so if you happen to find a nice acronym (such as Propositional Assistent *something something*) please submit it.

## Installation instructions
Currently the only way of getting pava is to clone this repo and build it with stack (or cabal).
```
git clone https://github.com/grastello/pava
cd pava
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
Since pava is still pretty young the system is hands-down brutal: only two connectives (negation and conjunction) and only four rules; theoretically you do not need more.

### Formulæ
Formulæ in the system are written in the typical way.
The following examples should clarify any doubt you might have.
```
¬(A∧¬B)
FOO∧BAR
foo∧bar
```
Pava is case sensitive and requires you to type the `∧` and `¬` as is (no `not` or `and` for now) so you might want to rely on your editor's macro system to avoid
copypasting a lot.

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
