This file details the strategies employed by the team to ensure the correct-
ness of the implementation of the ICD simulation.

(1) REQUIREMENTS SPECIFICATION

The initial week of the project was allocated solely towards enumerating
and scoping the requirements from the specification document. The team
applied judgement in interpreting any ambiguities, but - importantly -
agreed to a common understanding based on an understanding of the problem
domain. Requirements in scope were then listed in a summary document under
the following categories: (a) ICD calculations; (b) handling network
messages, (c) handling looping, and (d) quality requirements.

(2) PAIR PROGRAMMING ON EXECUTABLE ARCHITECTURE

A high-level executable architecture (i.e. implementation of the Tick
methods, with stubs on lower-level functions) was implemented by pairing
This was meant to (a) facilitate division of labour (i.e. allocate 
implementation of stubbed functions between team members), (b) homogenize
coding style, and (c) ensure the that architecture covered all requirements 
from (2), and (d) ensure common understanding of the solution.

(4) CODE REVIEWS ON IMPLEMENTATIONS

All functionality implemented individually was reviewed by the other in a
walkthrough. This was to ensure that implementations were both correct and
dealt with input corner-cases. Attention was also given to checking quality
requirements (e.g. low coupling between components, consistent style).

(5) UNIT, INTEGRATION, & ACCEPTANCE TESTING

Each team member unit-tested the other's code on manually generated test
cases (to cover corner-cases). Integration testing and requirements tracing
(from the list generated in (2) to functionality) was done as a pair.
