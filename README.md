# Intercoder-Reliability-Rec

Global Adaptation Mapping Initiative (GAMI)

Coder Reliability Check and Intercoder Reconciliation

Rationale for Reliability Check

Over 100 coders contributed to the GAMI project. They coded almost 3000 articles. 

To enable cross-article comparisons, it is important to make sure coders are following the coding guidelines and answering all 
(or almost all) questions for each article. The quality control (also called a reliability check) examines the responses given by 
each coder and identifies coders who do not provide complete codes (answer all questions). 

Completeness

Coder responses to 16 questions are examined. These questions are considered important for the analysis, are not dependent
on any other questions, and are relatively straightforward (e.g., TRUE/FALSE, name a continent). Coders who left more than 10% of these 
answers blank (for articles that they said should be included in the analysis and marked as sufficient to code) are considered
"unreliable." The code allows this 10% mark to be changed to another cutoff level. Coders with too many blanks are given a chance to complete their codes. 

Questions used for check: Geography (continent), Sector (drop-down menu), Local knowledge (True/False), Indigenous Knowledge (True/False), 
Actors undertaking adaptation (drop-down menu), Type of adaptation response (drop-down), Implementation tools (open text), Hazards
(drop-down menu), Exposure (open text), Implementation (drop-down menu), Depth (open text), Scope (open text), Speed (open text), 
Evidence of reduced risk (True/False), Indicators of Success (True/False), Limits approached (True/False)

Note: We originally checked over- or under-inclusion rates relative to those of other team members, but this presented too much uncertainty as to the "true" answer, so we removed this step.

Rationale for Intercoder Reconciliation 

Articles were divided into 4 cases depending on whether they were coded by one or more coders and whether those coders were reliable
or unreliable. 

Case 1: Article coded by one unreliable coder (not included in reconciled data)

Case 2: Article coded by two unreliable coders (not included unless a third person reviews)

Case 3: Article coded by one reliable coder

Case 4: Article coded by two coders (at least one reliable)

In Case 3, there is only one coder so there is no need for intercoder reconciliation. 

For Case 4, the R script uses a series of if/then statements to reconcile the multiple coder responses into a single answer for each article
(a single line in the final database). For any question that asked the coder to provide quotes or evidence, all quotes and evidence are
compiled. For True/False questions, if either coder marks true, the answer is coded as True because these questions ask about the 
absence or presence of certain topics in each article, and it is more likely that one coder overlooked the presence of an item that is
actually included (gave a false negative) than that a coder imagined the presence of something not actually present (gave a false
positive). For questions with multiple responses (e.g., hazards addressed), similar logic says to take everyone's answers because 
false positives are more likely than false negatives and because this means our analysis is conservative (it gives more credit rather 
than less whenever a question arises).

Include:              If either coder says the article should be included, it is included

Sufficient:           If either coder says the article is sufficient for analysis, it is included

Geography (Continent): All answers are accepted

Geography (Nation):   All answers are accepted

Sector:               All answers are accepted

Cross-cutting topics: All answers are accepted

Indigenous knowledge: If either coder says it was present, marked as present

Local knowledge:      If either coder says it was present, marked as present

Actors:               All answers are accepted

Equity in planning:   If either coder says it was present, marked as present

Equity in targeting:  If either coder says it was present, marked as present

Type of response:     All answers are accepted

Implementation tools: All answers are accepted

Hazard:               All answers are accepted

Exposure:             All answers are accepted

Links to Risk:        All answers are accepted

Adaptation Finance:   If either coder says it was present, marked as present

Finance Costs:        All answers are accepted

Depth:                All answers are accepted

Scope:                All answers are accepted

Speed:                All answers are accepted

Evidence of Reduced Risk: If either coder says it was present, marked as present

Indicators:           If either coder says it was present, marked as present

Maladaptation:        All answers are accepted

Cobenefits:           All answers are accepted

Approach Limits:      If either coder says true, marked as true

For Implementation Stage - describing how widespread the adaptation response is described - our analysis requires a single answer. 
Coders selected a ersponse form one of 5 drop-down options: 

"Vulnerability assessment and/or early planning" 

"Adaptation planning & early implementation"

"Implementation expanding"

"Implementation widespread"

"Evidence of risk reduction associated with adaptation" 

These are ranked 1-5 (1 being "vulnerability assessment" and 5 being "evidence of risk reduction"). When two coders differ, the code
takes the higher answer IF the two responses are only one stage apart (e.g., rank 2 and rank 3). By taking the higher value, the
reconciliation gives adaptation the benefit of the doubt. We may over-estimate how advanced some adaptations are, but we are 
systematically erring on the conservative side. 

If two coders give answers that are farther apart (e.g., one says "vulnerability assessment"[1] and the other "implementation 
widespread"[4]), we take the average of the two answers and round up (e.g., 1+4=5/2=2.5 rd up=3, so the code "implementation expanding" 
would be assigned"). Several coders mistakenly assigned "evidence of risk reduction" in this question because a separate question
asked whether the article provides evidence of risk reduction. We therefore disregard this code unless both coders provide it as a
response. (The median response is 1 "vulnerability assessment"; the average is 1.3 - between "vulnerability assessment" and "adaptation planning".)

These reconciliation steps are systematically biased to include more than they exclude. In this way, we provide the most detail 
possible about the stage and type of adaptation currently being practiced and documented in the adaptation literature.
