extensions [nw]
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;     Innovation Adoption with Outgroup Aversion on a Network
;;    BGM Version with notes and enhancements
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
turtles-own [
  group  ;; defines in-group or out-group - two values
  adopted ; defines if the turtle has adopted the innovation
  share-in ; share in
  share-out ; share out
  nr-in ; number in
  nr-out ; number out
  newlyadopted
  cumnr-in  ;cumulative nr-in
  cumnr-out ; cumulative nr-out
  cumshare-in ; cumulative share out
  cumshare-out ; cumulative share out
]
patches-own [
  policy
  share
  patchnumber
]
globals [
  polarization
]
to setup
  clear-all
  create-network
  assign-groups
  seed
  if Environment-type != "lattice" [calculate-network-metrics]
  reset-ticks
  ask patches [set policy 1];;this sets all the patches' policies to one (not sure what this is)
  ask turtles [set cumnr-in 0 set cumnr-out 0]
  save-matrix
end

to create-network
  if Environment-type = "lattice" [create-lattice]
  if Environment-type = "random-network" [create-random-network]
  if Environment-type = "scale-free-network" [create-scale-free-network]
  if Environment-type = "small-world-network" [create-small-world-network]
end
to calculate-network-metrics
  let nw-modularity modularity
  let nw-avg-local-clustering-coeff avg-local-clustering-coeff
  let nw-avg-betweenness-centrality avg-betweenness-centrality
  let nw-mean-path-length mean-path-length
  let nw-global-clustering-coefficient global-clustering-coefficient

end
to-report modularity
  report nw:modularity (list (turtles with [group = 0]) (turtles with [group = 1]))
end
to-report avg-local-clustering-coeff
  report mean [ nw:clustering-coefficient ] of turtles
end
to-report avg-betweenness-centrality
  report mean [nw:betweenness-centrality] of turtles
end
to-report mean-path-length
  nw:set-context turtles links
  report nw:mean-path-length
end
to-report average-degree
  report 2 * count links /  count turtles
end
to-report eigenvector-centrality
  report mean [nw:eigenvector-centrality] of turtles
end
to-report density
  report 2 * count links / (count turtles * (count turtles - 1))
end


to-report global-clustering-coefficient
  let closed-triplets sum [nw:clustering-coefficient * count my-links * (count my-links - 1) ] of turtles
    let triplets sum [ count my-links * (count my-links - 1) ] of turtles
    report closed-triplets / triplets
end

to create-lattice
  set-default-shape turtles "circle"
  ask patches[
    set pcolor white
    let i 0
    let j 0
    set patchnumber pxcor + pycor * 6
    ;q is the frequency of the group members A or B (0 or 1) in this case - see formula in article
    ;Q is the proportion of group A in the entire grid.  Q=.9 means 90% were group A where Q=.5 means the each patch has equal number of members.
    let qj (patchnumber - 1) / (49 - 1) + (1 - 2 * (patchnumber - 1) / (49 - 1)) * Q ; define qj - code was changed to match articl patchnumber - 1 (was +)
    set qj qj * 36 ;multiply qj by 36
    set qj int qj ; convert to integer type
    sprout 36 [
      set xcor xcor - 0.35 + 0.14 * i
      set ycor ycor - .35 + 0.14 * j
      set size 0.1 ; easier to see
      set adopted 0
      ifelse j * 6 + i < qj [set color 17 set group 0] [set color 97 set group 1]
      set i i + 1
      if i mod 6 = 0 [set j j + 1 set i 0]
    ]
  ]
end
to create-random-network-ER
  nw:generate-random turtles links number-of-nodes connection-probability [
    setxy (random-xcor * .85) (random-ycor * 0.85)
    set color blue
    set size .2
  ]
  repeat 10
  [
    layout-spring turtles links 0.2 (world-width / (2 * (sqrt number-of-nodes))) 1
  ]
  ask turtles with [ not any? my-links ] [ die ];kill any turtles without any links
end
to create-random-network
  set-default-shape turtles "circle"
  create-turtles number-of-nodes
  [
    setxy (random-xcor * .85) (random-ycor * 0.85)
    set color blue
    set size .2
  ]
  let num-links (average-node-degree * number-of-nodes) / 2
  while [count links < num-links ]
  [
    ask one-of turtles
    [ let choice (min-one-of (other turtles with [not link-neighbor? myself])
      [distance myself])
      if choice != nobody [ create-link-with choice ]
    ]
  ]
  repeat 10
  [
    layout-spring turtles links 0.2 (world-width / (2 * (sqrt number-of-nodes))) 1
  ]
  ask turtles with [ not any? my-links ] [ die ];kill any turtles without any links
end
;; Small world network creation
to create-small-world-network ; using Watts-strogatz method with neighborhood-size = to local links setting and rewire-probability (from Interface)
  set-default-shape turtles "circle"
  nw:generate-watts-strogatz turtles links number-of-nodes local-links rewire-probability [
    setxy (random-xcor * .85) (random-ycor * 0.85)
    set color blue
    set size .2
  ]
  repeat 10
  [
    layout-spring turtles links 0.2 (world-width / (2 * (sqrt number-of-nodes))) 1
  ]
  ask turtles with [ not any? my-links ] [ die ];kill any turtles without any links
end
;; scale-free network creation
to create-scale-free-network ; use preferential attachment command in network extension with minimum degree = 1 to keep network connected
  set-default-shape turtles "circle"
  nw:generate-preferential-attachment turtles links number-of-nodes 1 [
    setxy (random-xcor * .85) (random-ycor * 0.85)
    set color blue
    set size .2
  ]
  repeat 10
  [
    layout-spring turtles links 0.2 (world-width / (2 * (sqrt number-of-nodes))) 1
  ]
  ask turtles with [ not any? my-links ] [ die ];kill any turtles without any links
end

to seed
  if Environment-type = "lattice" [
    if startpatch = "random" [
      ask one-of patches [
        set policy 1
        ask n-of seeds turtles-here [
          set adopted 1 ifelse group = 0 [set color 14][set color 94]
        ]
      ]
    ]
    if startpatch = "equal" [
      ask min-one-of patches [abs(count turtles-here with [group = 1] - count turtles-here with [group = 0])] [
        set policy 1
        ask n-of seeds turtles-here [
          set adopted 1 ifelse group = 0 [
            set color 14][
            set color 94]
        ]
      ]
    ]
    if startpatch = "skewed" [
      ask max-one-of patches [count turtles-here with [group = 1]] [
        set policy 1
        ask n-of seeds turtles-here [
          set adopted 1 ifelse group = 0 [set color 14][set color 94]
        ]
      ]
    ]
  ]
  if (Environment-type = "random-network") or (Environment-type = "small-world-network") or (Environment-type = "scale-free-network") [
    ;;to start out the seeds are randomly distributed around the network.
    ask n-of seeds turtles [
      set adopted 1 ifelse group = 0 [set color 14][set color 94]
      ]
    ]
end

to assign-groups
  ;; for network only, randomly assign groups to turtles
  if (Environment-type = "random-network") or (Environment-type = "small-world-network") or (Environment-type = "scale-free-network") [
    ask turtles [
      ifelse random-float 1 > .5 [
        set group 0
        set color 17
      ][
        set group  1
        set color  97
      ]
    ]
  ]
end
to go
  ;if global-adoption = .90 [stop]
  ;if group0-adoption = .95 [stop]
  ;if group1-adoption = .95 [stop]
  if ticks = 2000 [stop]
  ask turtles [
    let i 0
    let didotheradopt? 0
    set newlyadopted 0
    let groupother? 0
    set share-in 0
    set share-out 0
    set nr-in 0
    set nr-out 0
    if random-float 1 < prob-m [
      while [i < observed]
      [
        ifelse random-float 1 < prob-f [
            if Environment-type = "lattice" [
              ;;select a turtle from the immediate patch to sample in the observations
              ask one-of other turtles-here [set didotheradopt? adopted set groupother? group]
              ifelse groupother? = group [
                if didotheradopt? = 1 [
                  set share-in share-in + 1]
                set nr-in nr-in + 1
              ]
              [
                if didotheradopt? = 1[
                  set share-out share-out + 1]
                set nr-out nr-out + 1
              ]
            ]
            if (Environment-type = "random-network") or (Environment-type = "scale-free-network") or (Environment-type = "small-world-network") [
           ;; select from turtles within immediate network neighborhood
           ; ask one-of nw:turtles-in-radius local-links [set didotheradopt? adopted set groupother? group]
            let ts one-of nw:turtles-in-radius local-links
            if ts != nobody [ask ts [set didotheradopt? adopted set groupother? group]
            ]
          ]
        ]
          [
          if infores = "local" [
            if Environment-type = "lattice" [
              ;lower left corner
              if pxcor = 0 and pycor = 0 [ask one-of turtles-on patches with [((pxcor = 1) and (pycor = 0))] [set didotheradopt? adopted set groupother? group]]
              ;upper right corner
              if pxcor = 6 and pycor = 6 [ask one-of turtles-on patches with [((pxcor = 5) and (pycor = 6))] [set didotheradopt? adopted set groupother? group]]
              ;left hand column (including top left corner)
              if pxcor = 0 and pycor > 0 and pycor <= 6 [let ycord pycor ask one-of turtles-on patches with [((pxcor = 6) and (pycor = (ycord - 1))) or ((pxcor = 1) and (pycor = (ycord + 1)))] [set didotheradopt? adopted set groupother? group]]
              ; right hand column of patches (including lower right corner)
              if pxcor = 6 and pycor >= 0 and pycor < 6 [let ycord pycor ask one-of turtles-on patches with [((pxcor = 5) and (pycor = ycord)) or ((pxcor = 0) and (pycor = (ycord + 1)))] [set didotheradopt? adopted set groupother? group]]
              ; middle region (non edges)
              if pxcor > 0 and pxcor < 6 and pycor > 0 and pycor < 6 [let ycord pycor let xcord pxcor ask one-of turtles-on patches with [((pxcor = (xcord - 1)) and (pycor = ycord)) or ((pxcor = (xcord + 1)) and (pycor = ycord))] [set didotheradopt? adopted set groupother? group]]
              ;bottom row between corners
              if pxcor > 0 and pxcor < 6 and pycor = 0 [let xcord pxcor ask one-of turtles-on patches with [((pxcor = (xcord - 1)) and (pycor = 0)) or ((pxcor = (xcord + 1)) and (pycor = 0))] [set didotheradopt? adopted set groupother? group]]
              ; top row between corners
              if pxcor > 0 and pxcor < 6 and pycor = 6 [let xcord pxcor ask one-of turtles-on patches with [((pxcor = (xcord - 1)) and (pycor = 6)) or ((pxcor = (xcord + 1)) and (pycor = 6))] [set didotheradopt? adopted set groupother? group]]
            ]
            if (Environment-type = "random-network") or (Environment-type = "scale-free-network") or (Environment-type = "small-world-network") [
              ;sample from a neighborhood number of links, not sure how to exclude the neighborhood links
              let ts one-of nw:turtles-in-radius neighborhood-links
              if ts != nobody [ask ts [set didotheradopt? adopted set groupother? group]]]
          ]
          if infores = "global" [ask one-of turtles [set didotheradopt? adopted set groupother? group]]

            ; the following applies to either global or local/adjacent observation:
          ifelse groupother? = group[
            if didotheradopt? = 1 [ set share-in share-in + 1]
            set nr-in nr-in + 1
          ]
          [
            if didotheradopt? = 1 [set share-out share-out + 1]
            set nr-out nr-out + 1
          ]
        ]
        ;if the focal turtle has already adopted, then also increment number in and share-in, otherwise only increment number-in.
        ifelse adopted = 1[
          set nr-in nr-in + 1
          set share-in share-in + 1
        ]
        [
          set nr-in nr-in + 1
        ]
        set i i + 1
      ]
      ; if policy > 1 [show share-in]
      ;cumulative can include "memory" by adding to previous cumulative or just be number in current observation set (memory = 0)
      ;adding memory will often cause the stabilization to take longer and will make exp() value exceed netLogo total
      ;in the original code, "memory" was also labelled "lambda" so this was modified by re-labeling it as a memory factor.
      set cumnr-in cumnr-in * memory + (1 - memory) * nr-in
      ;set cumnr-in cumnr-in + nr-in ; modified to remove lambda term in all four of these.
      set cumnr-out cumnr-out * memory + (1 - memory) * nr-out
      ;set cumnr-out cumnr-out + nr-out
      set cumshare-in cumshare-in * memory + (1 - memory) * share-in
      ;set cumshare-in cumshare-in + share-in
      set cumshare-out cumshare-out * memory + (1 - memory) * share-out
      ;set cumshare-out cumshare-out + share-out

      set share (cumshare-in + cumshare-out) / (cumnr-in + cumnr-out)
      let share-ingroup 0
      set share-ingroup (cumshare-in - cumshare-out)
      ; if policy > o, show share-outgroup
      if cumnr-in > 0[set share-in cumshare-in / cumnr-in]
      if cumnr-out > 0 [set share-out cumshare-out / cumnr-out]
;;
;;     Calculate the Combined probability of imitation (share^lambda) and outgroup aversion/ingroup affinity
;;
      let prob 0
      if [policy] of patch-here > 0[
        ;share is a patch-level variable (should it be turtle level, given it is calculated by using variable on the focal turtle being looked at?
        set prob (share ^ lambda) * ((1 - ingroupBias) + (ingroupBias / (1 + exp (0 - beta * (share-ingroup - y-value)))))
        ;replace? y-value should be zero to be consistent with the article  beta is also not mentioned in the article, so it is set to one.
        ;show share
        ;show share-in
        ;show prob
        ;show prob
      ]
      ifelse random-float 1 < prob [
        set newlyadopted 1 ifelse group = 0 [set color 14][set color 94]
      ]
      [
        set newlyadopted 0 - 1 ifelse group = 0 [set color 17][set color 97]
      ]
    ]
  ]
  ;; This step makes the final adoption decision based on the combined probability above.
  ask turtles [
    if newlyadopted = 1 [set adopted 1]
    if newlyadopted < 0 [set adopted 0]
  ]
  ;; this section calculate polarization
  set polarization 0
  ask patches [
    let shareneighbors 0
    if pxcor = 0 and pycor = 0 [ask turtles-on patches with [((pxcor = 1) and (pycor = 0))] [if share > shareneighbors [set shareneighbors share]]]
    if pxcor = 6 and pycor = 6 [ask turtles-on patches with [((pxcor = 5) and (pycor = 6))] [if share > shareneighbors [set shareneighbors share]]]
    if pxcor = 0 and pycor > 0 and pycor < 6 [let ycord pycor ask turtles-on patches with [((pxcor = 6) and (pycor = (ycord - 1))) or ((pxcor = 1) and (pycor = ycord))] [if share > shareneighbors [set shareneighbors share]]]
    if pxcor = 6 and pycor > 0 and pycor < 6 [let ycord pycor ask turtles-on patches with [((pxcor = 5) and (pycor = ycord)) or ((pxcor = 0) and (pycor = (ycord + 1)))] [if share > shareneighbors [set shareneighbors share]]]
    if pxcor > 0 and pxcor < 6 and pycor > 0 and pycor < 6 [let ycord pycor let xcord pxcor ask one-of turtles-on patches with [((pxcor = (xcord - 1)) and (pycor = ycord)) or ((pxcor = (xcord + 1)) and (pycor = ycord))] [if share > shareneighbors [set shareneighbors share]]]
    if pxcor > 0 and pxcor < 6 and pycor = 0 [let xcord pxcor ask turtles-on patches with[((pxcor = (xcord - 1)) and (pycor = 0)) or ((pxcor = (xcord + 1)) and (pycor = 0))] [if share > shareneighbors [set shareneighbors share]]]
    if pxcor > 0 and pxcor < 6 and pycor = 6 [let xcord pxcor ask turtles-on patches with [((pxcor = (xcord - 1)) and (pycor = 6)) or ((pxcor = (xcord + 1)) and (pycor = 6))] [if share > shareneighbors [set shareneighbors share]]]

    if count neighbors with [policy = 1 and share > share-min] > 0 [
      set policy 1
    ]
    ;not sure why line below divides by 18??
    ;if count turtles-here with [adopted = 1] > 0 [set polarization polarization + abs ((count turtles-here with [group = 0 and adopted = 1] * (count turtles-here with [group = 0] / 18) - count turtles-here with [group = 1 and adopted = 1] * (count turtles-here with [group = 1] / 18)) / count turtles-here with [adopted = 1])]
    if count turtles-here with [adopted = 1] > 0 and (count turtles-here with [group = 1] > 0) and (count turtles-here with [group = 0] > 0)[
      set polarization polarization + abs ((count turtles-here with [group = 0 and adopted = 1] / (count turtles-here with [group = 0]) - count turtles-here with [group = 1 and adopted = 1] / (count turtles-here with [group = 1])))]
    ]
  ;;Note: this is local polarization; however for the network study we will not be using this because it is referencing patches.  Left in for lattice scenario
  set polarization polarization / count patches
  if (count turtles with [group = 0 and adopted = 1] + count turtles with [group = 1 and adopted = 1]) > 0 [
    let glob-polarization abs ((count turtles with [group = 0 and adopted = 1] - (count turtles with [group = 1 and adopted = 1])) / (count turtles with [group = 0 and adopted = 1] + count turtles with [group = 1 and adopted = 1]))
  ]
  tick
end

to-report global-polarization
  ;report abs((sum[share-in] of turtles - sum[share-out] of turtles)) / (sum[nr-in] of turtles + sum[nr-out]of turtles)
  if (count turtles with [group = 0 and adopted = 1] + count turtles with [group = 1 and adopted = 1]) > 0 [
  report abs ((count turtles with [group = 0 and adopted = 1] - (count turtles with [group = 1 and adopted = 1])) / (count turtles with [group = 0 and adopted = 1] + count turtles with [group = 1 and adopted = 1]))
  ]
end
to-report global-adoption
  report count turtles with [adopted = 1] / count turtles
end
to-report group0-adoption
  report count turtles with [adopted = 1 and group = 0] / count turtles with [group = 0]
end
to-report group1-adoption
  report count turtles with [adopted = 1 and group = 1] / count turtles with [group = 1]
end
to save-matrix
  nw:set-context turtles links
  nw:save-matrix filename
end
@#$#@#$#@
GRAPHICS-WINDOW
410
10
791
392
-1
-1
53.3
1
10
1
1
1
0
1
1
1
0
6
0
6
0
0
1
ticks
30.0

BUTTON
10
19
83
52
setup
setup
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
107
17
170
50
go
go
T
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

SLIDER
12
78
184
111
prob-f
prob-f
0
1
0.7
.01
1
NIL
HORIZONTAL

SLIDER
18
128
190
161
prob-m
prob-m
0
1
0.05
.01
1
NIL
HORIZONTAL

SLIDER
14
174
186
207
seeds
seeds
1
10
5.0
1
1
NIL
HORIZONTAL

SLIDER
16
224
188
257
Q
Q
0
1
0.9
.01
1
NIL
HORIZONTAL

SLIDER
224
78
396
111
observed
observed
0
36
30.0
1
1
NIL
HORIZONTAL

SLIDER
223
133
395
166
lambda
lambda
0
1
0.3
.01
1
NIL
HORIZONTAL

SLIDER
222
175
394
208
beta
beta
0
20
1.0
1
1
NIL
HORIZONTAL

SLIDER
219
222
391
255
y-value
y-value
0
1
0.0
.01
1
NIL
HORIZONTAL

SLIDER
14
276
186
309
share-min
share-min
0
1
0.0
.01
1
NIL
HORIZONTAL

CHOOSER
220
275
358
320
infores
infores
"local" "global"
0

CHOOSER
214
338
352
383
startpatch
startpatch
"random" "skewed" "equal"
2

SLIDER
14
337
186
370
ingroupBias
ingroupBias
0
1
1.0
.01
1
NIL
HORIZONTAL

SLIDER
18
389
190
422
memory
memory
0
1
0.0
.01
1
NIL
HORIZONTAL

PLOT
797
10
1127
219
Adopted
NIL
NIL
0.0
10.0
0.0
1.0
true
true
"" ""
PENS
"Total Adoption" 1.0 0 -16777216 true "" "plot count turtles with [adopted = 1] / count turtles"
"Group 0 Adoption" 1.0 0 -2674135 true "" "plot count turtles with [adopted = 1 and group = 0] / count turtles with [group = 0]"
"Group 1 Adoption" 1.0 0 -13345367 true "" "plot count turtles with [adopted = 1 and group = 1] / count turtles with [group = 1]"

PLOT
797
223
1131
396
Polarization
NIL
NIL
0.0
10.0
0.0
1.0
true
true
"" ""
PENS
"Local Polarization" 1.0 0 -16777216 true "" "plot polarization"
"Global Polarization" 1.0 0 -10899396 true "" "plot global-polarization"

SLIDER
19
433
297
466
number-of-nodes
number-of-nodes
20
500
250.0
1
1
NIL
HORIZONTAL

SLIDER
20
472
291
505
average-node-degree
average-node-degree
0
50
2.0
1
1
NIL
HORIZONTAL

CHOOSER
212
17
391
62
Environment-type
Environment-type
"lattice" "random-network" "scale-free-network" "small-world-network"
3

SLIDER
355
422
533
455
neighborhood-links
neighborhood-links
1
3
1.0
1
1
NIL
HORIZONTAL

SLIDER
361
471
533
504
local-links
local-links
neighborhood-links + 1
10
2.0
1
1
NIL
HORIZONTAL

MONITOR
556
408
689
453
Network Modularity
modularity
3
1
11

MONITOR
559
461
797
506
Average Local Clustering Coefficient
avg-local-clustering-coeff
3
1
11

MONITOR
710
408
919
453
Average Betweenness Centrality
avg-betweenness-centrality
3
1
11

MONITOR
930
409
1049
454
Mean path length
mean-path-length
3
1
11

MONITOR
809
461
999
506
Global Clustering Coefficient
global-clustering-coefficient
3
1
11

SLIDER
223
390
419
423
connection-probability
connection-probability
0
.1
0.06
.001
1
NIL
HORIZONTAL

MONITOR
1026
464
1115
509
Turtle count
count turtles
0
1
11

MONITOR
1065
412
1129
457
Avg deg
average-degree
3
1
11

INPUTBOX
302
517
531
577
filename
matrix-smworldnw
1
0
String

MONITOR
614
533
763
578
Eigenvector Centrality
eigenvector-centrality
3
1
11

MONITOR
838
532
897
577
Density
density
3
1
11

SLIDER
52
529
224
562
rewire-probability
rewire-probability
0
.2
0.01
.01
1
NIL
HORIZONTAL

@#$#@#$#@
## WHAT IS IT?

(a general understanding of what the model is trying to show or explain)

## HOW IT WORKS

(what rules the agents use to create the overall behavior of the model)

## HOW TO USE IT

(how to use the model, including a description of each of the items in the Interface tab)

## THINGS TO NOTICE

(suggested things for the user to notice while running the model)

## THINGS TO TRY

(suggested things for the user to try to do (move sliders, switches, etc.) with the model)

## EXTENDING THE MODEL

(suggested things to add or change in the Code tab to make the model more complicated, detailed, accurate, etc.)

## NETLOGO FEATURES

(interesting or unusual features of NetLogo that the model uses, particularly in the Code tab; or where workarounds were needed for missing features)

## RELATED MODELS

(models in the NetLogo Models Library and elsewhere which are of related interest)

## CREDITS AND REFERENCES

(a reference to the model's URL on the web if it has one, as well as any other necessary credits, citations, and links)
@#$#@#$#@
default
true
0
Polygon -7500403 true true 150 5 40 250 150 205 260 250

airplane
true
0
Polygon -7500403 true true 150 0 135 15 120 60 120 105 15 165 15 195 120 180 135 240 105 270 120 285 150 270 180 285 210 270 165 240 180 180 285 195 285 165 180 105 180 60 165 15

arrow
true
0
Polygon -7500403 true true 150 0 0 150 105 150 105 293 195 293 195 150 300 150

box
false
0
Polygon -7500403 true true 150 285 285 225 285 75 150 135
Polygon -7500403 true true 150 135 15 75 150 15 285 75
Polygon -7500403 true true 15 75 15 225 150 285 150 135
Line -16777216 false 150 285 150 135
Line -16777216 false 150 135 15 75
Line -16777216 false 150 135 285 75

bug
true
0
Circle -7500403 true true 96 182 108
Circle -7500403 true true 110 127 80
Circle -7500403 true true 110 75 80
Line -7500403 true 150 100 80 30
Line -7500403 true 150 100 220 30

butterfly
true
0
Polygon -7500403 true true 150 165 209 199 225 225 225 255 195 270 165 255 150 240
Polygon -7500403 true true 150 165 89 198 75 225 75 255 105 270 135 255 150 240
Polygon -7500403 true true 139 148 100 105 55 90 25 90 10 105 10 135 25 180 40 195 85 194 139 163
Polygon -7500403 true true 162 150 200 105 245 90 275 90 290 105 290 135 275 180 260 195 215 195 162 165
Polygon -16777216 true false 150 255 135 225 120 150 135 120 150 105 165 120 180 150 165 225
Circle -16777216 true false 135 90 30
Line -16777216 false 150 105 195 60
Line -16777216 false 150 105 105 60

car
false
0
Polygon -7500403 true true 300 180 279 164 261 144 240 135 226 132 213 106 203 84 185 63 159 50 135 50 75 60 0 150 0 165 0 225 300 225 300 180
Circle -16777216 true false 180 180 90
Circle -16777216 true false 30 180 90
Polygon -16777216 true false 162 80 132 78 134 135 209 135 194 105 189 96 180 89
Circle -7500403 true true 47 195 58
Circle -7500403 true true 195 195 58

circle
false
0
Circle -7500403 true true 0 0 300

circle 2
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240

cow
false
0
Polygon -7500403 true true 200 193 197 249 179 249 177 196 166 187 140 189 93 191 78 179 72 211 49 209 48 181 37 149 25 120 25 89 45 72 103 84 179 75 198 76 252 64 272 81 293 103 285 121 255 121 242 118 224 167
Polygon -7500403 true true 73 210 86 251 62 249 48 208
Polygon -7500403 true true 25 114 16 195 9 204 23 213 25 200 39 123

cylinder
false
0
Circle -7500403 true true 0 0 300

dot
false
0
Circle -7500403 true true 90 90 120

face happy
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 255 90 239 62 213 47 191 67 179 90 203 109 218 150 225 192 218 210 203 227 181 251 194 236 217 212 240

face neutral
false
0
Circle -7500403 true true 8 7 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Rectangle -16777216 true false 60 195 240 225

face sad
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 168 90 184 62 210 47 232 67 244 90 220 109 205 150 198 192 205 210 220 227 242 251 229 236 206 212 183

fish
false
0
Polygon -1 true false 44 131 21 87 15 86 0 120 15 150 0 180 13 214 20 212 45 166
Polygon -1 true false 135 195 119 235 95 218 76 210 46 204 60 165
Polygon -1 true false 75 45 83 77 71 103 86 114 166 78 135 60
Polygon -7500403 true true 30 136 151 77 226 81 280 119 292 146 292 160 287 170 270 195 195 210 151 212 30 166
Circle -16777216 true false 215 106 30

flag
false
0
Rectangle -7500403 true true 60 15 75 300
Polygon -7500403 true true 90 150 270 90 90 30
Line -7500403 true 75 135 90 135
Line -7500403 true 75 45 90 45

flower
false
0
Polygon -10899396 true false 135 120 165 165 180 210 180 240 150 300 165 300 195 240 195 195 165 135
Circle -7500403 true true 85 132 38
Circle -7500403 true true 130 147 38
Circle -7500403 true true 192 85 38
Circle -7500403 true true 85 40 38
Circle -7500403 true true 177 40 38
Circle -7500403 true true 177 132 38
Circle -7500403 true true 70 85 38
Circle -7500403 true true 130 25 38
Circle -7500403 true true 96 51 108
Circle -16777216 true false 113 68 74
Polygon -10899396 true false 189 233 219 188 249 173 279 188 234 218
Polygon -10899396 true false 180 255 150 210 105 210 75 240 135 240

house
false
0
Rectangle -7500403 true true 45 120 255 285
Rectangle -16777216 true false 120 210 180 285
Polygon -7500403 true true 15 120 150 15 285 120
Line -16777216 false 30 120 270 120

leaf
false
0
Polygon -7500403 true true 150 210 135 195 120 210 60 210 30 195 60 180 60 165 15 135 30 120 15 105 40 104 45 90 60 90 90 105 105 120 120 120 105 60 120 60 135 30 150 15 165 30 180 60 195 60 180 120 195 120 210 105 240 90 255 90 263 104 285 105 270 120 285 135 240 165 240 180 270 195 240 210 180 210 165 195
Polygon -7500403 true true 135 195 135 240 120 255 105 255 105 285 135 285 165 240 165 195

line
true
0
Line -7500403 true 150 0 150 300

line half
true
0
Line -7500403 true 150 0 150 150

pentagon
false
0
Polygon -7500403 true true 150 15 15 120 60 285 240 285 285 120

person
false
0
Circle -7500403 true true 110 5 80
Polygon -7500403 true true 105 90 120 195 90 285 105 300 135 300 150 225 165 300 195 300 210 285 180 195 195 90
Rectangle -7500403 true true 127 79 172 94
Polygon -7500403 true true 195 90 240 150 225 180 165 105
Polygon -7500403 true true 105 90 60 150 75 180 135 105

plant
false
0
Rectangle -7500403 true true 135 90 165 300
Polygon -7500403 true true 135 255 90 210 45 195 75 255 135 285
Polygon -7500403 true true 165 255 210 210 255 195 225 255 165 285
Polygon -7500403 true true 135 180 90 135 45 120 75 180 135 210
Polygon -7500403 true true 165 180 165 210 225 180 255 120 210 135
Polygon -7500403 true true 135 105 90 60 45 45 75 105 135 135
Polygon -7500403 true true 165 105 165 135 225 105 255 45 210 60
Polygon -7500403 true true 135 90 120 45 150 15 180 45 165 90

sheep
false
15
Circle -1 true true 203 65 88
Circle -1 true true 70 65 162
Circle -1 true true 150 105 120
Polygon -7500403 true false 218 120 240 165 255 165 278 120
Circle -7500403 true false 214 72 67
Rectangle -1 true true 164 223 179 298
Polygon -1 true true 45 285 30 285 30 240 15 195 45 210
Circle -1 true true 3 83 150
Rectangle -1 true true 65 221 80 296
Polygon -1 true true 195 285 210 285 210 240 240 210 195 210
Polygon -7500403 true false 276 85 285 105 302 99 294 83
Polygon -7500403 true false 219 85 210 105 193 99 201 83

square
false
0
Rectangle -7500403 true true 30 30 270 270

square 2
false
0
Rectangle -7500403 true true 30 30 270 270
Rectangle -16777216 true false 60 60 240 240

star
false
0
Polygon -7500403 true true 151 1 185 108 298 108 207 175 242 282 151 216 59 282 94 175 3 108 116 108

target
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240
Circle -7500403 true true 60 60 180
Circle -16777216 true false 90 90 120
Circle -7500403 true true 120 120 60

tree
false
0
Circle -7500403 true true 118 3 94
Rectangle -6459832 true false 120 195 180 300
Circle -7500403 true true 65 21 108
Circle -7500403 true true 116 41 127
Circle -7500403 true true 45 90 120
Circle -7500403 true true 104 74 152

triangle
false
0
Polygon -7500403 true true 150 30 15 255 285 255

triangle 2
false
0
Polygon -7500403 true true 150 30 15 255 285 255
Polygon -16777216 true false 151 99 225 223 75 224

truck
false
0
Rectangle -7500403 true true 4 45 195 187
Polygon -7500403 true true 296 193 296 150 259 134 244 104 208 104 207 194
Rectangle -1 true false 195 60 195 105
Polygon -16777216 true false 238 112 252 141 219 141 218 112
Circle -16777216 true false 234 174 42
Rectangle -7500403 true true 181 185 214 194
Circle -16777216 true false 144 174 42
Circle -16777216 true false 24 174 42
Circle -7500403 false true 24 174 42
Circle -7500403 false true 144 174 42
Circle -7500403 false true 234 174 42

turtle
true
0
Polygon -10899396 true false 215 204 240 233 246 254 228 266 215 252 193 210
Polygon -10899396 true false 195 90 225 75 245 75 260 89 269 108 261 124 240 105 225 105 210 105
Polygon -10899396 true false 105 90 75 75 55 75 40 89 31 108 39 124 60 105 75 105 90 105
Polygon -10899396 true false 132 85 134 64 107 51 108 17 150 2 192 18 192 52 169 65 172 87
Polygon -10899396 true false 85 204 60 233 54 254 72 266 85 252 107 210
Polygon -7500403 true true 119 75 179 75 209 101 224 135 220 225 175 261 128 261 81 224 74 135 88 99

wheel
false
0
Circle -7500403 true true 3 3 294
Circle -16777216 true false 30 30 240
Line -7500403 true 150 285 150 15
Line -7500403 true 15 150 285 150
Circle -7500403 true true 120 120 60
Line -7500403 true 216 40 79 269
Line -7500403 true 40 84 269 221
Line -7500403 true 40 216 269 79
Line -7500403 true 84 40 221 269

wolf
false
0
Polygon -16777216 true false 253 133 245 131 245 133
Polygon -7500403 true true 2 194 13 197 30 191 38 193 38 205 20 226 20 257 27 265 38 266 40 260 31 253 31 230 60 206 68 198 75 209 66 228 65 243 82 261 84 268 100 267 103 261 77 239 79 231 100 207 98 196 119 201 143 202 160 195 166 210 172 213 173 238 167 251 160 248 154 265 169 264 178 247 186 240 198 260 200 271 217 271 219 262 207 258 195 230 192 198 210 184 227 164 242 144 259 145 284 151 277 141 293 140 299 134 297 127 273 119 270 105
Polygon -7500403 true true -1 195 14 180 36 166 40 153 53 140 82 131 134 133 159 126 188 115 227 108 236 102 238 98 268 86 269 92 281 87 269 103 269 113

x
false
0
Polygon -7500403 true true 270 75 225 30 30 225 75 270
Polygon -7500403 true true 30 75 75 30 270 225 225 270
@#$#@#$#@
NetLogo 6.2.0
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
<experiments>
  <experiment name="experiment" repetitions="50" sequentialRunOrder="false" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="2000"/>
    <metric>global-adoption</metric>
    <metric>group0-adoption</metric>
    <metric>group1-adoption</metric>
    <metric>global-polarization</metric>
    <metric>density</metric>
    <metric>avg-betweenness-centrality</metric>
    <metric>mean-path-length</metric>
    <metric>average-degree</metric>
    <metric>avg-local-clustering-coeff</metric>
    <metric>eigenvector-centrality</metric>
    <enumeratedValueSet variable="Environment-type">
      <value value="&quot;scale-free-network&quot;"/>
      <value value="&quot;random-network&quot;"/>
      <value value="&quot;small-world-network&quot;"/>
      <value value="&quot;lattice&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Q">
      <value value="0.9"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="seeds">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="lambda">
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="ingroupBias">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-of-nodes">
      <value value="250"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="connection-probability">
      <value value="0.06"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prob-m">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="beta">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="local-links">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="startpatch">
      <value value="&quot;equal&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="share-min">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prob-f">
      <value value="0.7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="average-node-degree">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rewire-probability">
      <value value="0.01"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="neighborhood-links">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="filename">
      <value value="&quot;matrix&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="memory">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="observed">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="y-value">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="infores">
      <value value="&quot;local&quot;"/>
      <value value="&quot;global&quot;"/>
    </enumeratedValueSet>
  </experiment>
</experiments>
@#$#@#$#@
@#$#@#$#@
default
0.0
-0.2 0 0.0 1.0
0.0 1 1.0 0.0
0.2 0 0.0 1.0
link direction
true
0
Line -7500403 true 150 150 90 180
Line -7500403 true 150 150 210 180
@#$#@#$#@
0
@#$#@#$#@
