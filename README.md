# COOL Generator

This program generates a subset of COOL programs.

Specifically, it cannot express:
- Complicated subtype usage. It does its best (cases & branches, for example), but I have not been able to concretely verify full coverage. In many cases, the program generates expressions with *exactly* the correct type.
- Voids. A very useful feature would be the ability to introduce void bindings

On the generation side, it also has a some shortcomings:
- No RNG seeding, so you must save the entire program output to reproduce it
- Inefficient, multi-round tree generation. I reckon it can be done in one pass
- Lack of rigorous testing means that some % of your runs will probably produce invalid COOL code :(

That being said, I find that the program covers lots of common cases. I built it for use as a fuzzer in PA4 of Stanford's CS143 Spring 2025 offering, so some "assignment code" shakiness is to be expected ;)
