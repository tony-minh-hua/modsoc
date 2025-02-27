globals [
 observables-weights ;; weight applied to the observable traits
]

breed [identities identity]
breed [individuals individual]

identities-own [
 traits-observable ;; average value of its members' observables
 traits-non-observable ;; average value of its members' nonobservables
 members ;; reference to agents with this identity
 identity-distance ;; distance with members
 member-count ;; number of members
]

individuals-own [
  observables ;; vector of observable traits
  non-observables ;; vector of non-observable traits
  ID ;; reference to agent's singular identity
]

to go

;; Have individuals choose to join other groups
join-group

;; Kill dead identities
remove-empty-identities

;; Update distance measure
evaluate-identity-distance

;; Update the membership count
evaluate-member-count

determine-observable-weights ;; This should occur last for when we need to update the identities

tick
end



;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Action Procedures ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;
to join-group
  ask individuals [
    ;; Get difference between individual's observables and their current ID's observables
    let diff difference-of-lists observables [traits-observable] of ID
    set diff product-of-lists diff observables-weights
    ;; Compare distance from individual's observables with their current identity observable traits
    let a (benefit-bigger-group [member-count] of ID - (difference-euclidean-distance diff + euclidean-distance non-observables [traits-non-observable] of ID) * distance-penalty)
    let temp-ID nobody
    ask identities with [self != [ID] of myself] [
      ;; Calculate utility and decide if it is worth joining the other group.
      ;; Compare distance from the individual's observables with the observable traits of the particular identity group
      let diff2 difference-of-lists [observables] of myself traits-observable
      set diff2 product-of-lists diff2 observables-weights
      let b (benefit-bigger-group (1 + member-count) - (difference-euclidean-distance diff2 + euclidean-distance [non-observables] of myself traits-non-observable) * distance-penalty)
      ;; If it is worth joining, remove individual from current group
      if (b > a) [
        set a b
        set temp-ID self
      ]
    ]
    if (temp-ID != nobody) [
      ;; Remove individual from old identity group
      ask ID [set members remove myself members]
      ;ask ID [if empty? members [die]]
      ;; Add individual to new identity group
      set ID temp-ID
      ;; Adds individual to newly linked ID group
      ask ID [set members (lput myself members)]
    ]
  ]
end

to-report benefit-bigger-group [group-size]
  report ((1 - 1 / group-size) * group-size-utility)
end

;; Kill empty identities
to remove-empty-identities
  ask identities [if empty? members [die]]
end

to evaluate-identity-distance
  ask identities [
    let data-observables[]
    let data-non-observables[]
    ask turtle-set members [
      set data-observables lput observables data-observables
      set data-non-observables lput non-observables data-non-observables
    ]
   set identity-distance (group-distance traits-observable data-observables + group-distance traits-non-observable data-non-observables)
  ]
end

to evaluate-member-count
  ask identities [
   set member-count length members
  ]
end

to determine-observable-weights
  let each-identity-weight[]

  ask identities [
    ;; Get the distance between the group members observable identity and the group's average observable identity
    let members-traits-observable[]
    ;; Get a lists of lists
    ask turtle-set members [
      set members-traits-observable lput observables members-traits-observable
    ]

    let diff-group-member sum-of-differences traits-observable members-traits-observable

    ;; For the observable identity, pick one that has the least distance
    let diff-group-member-select replace-with-one diff-group-member

    ;; Scale up the identity by number of members
    let diff-group-member-select-scaled multiply-list diff-group-member-select length members

    set each-identity-weight lput diff-group-member-select-scaled each-identity-weight
  ]

    ;; Scale down the global weights by proportion
    let scaled-identity-weights multiply-list (sum-of-lists each-identity-weight) (1 / num-nodes)
    ;let scaled-identity-weights sum-of-lists each-identity-weight
    set scaled-identity-weights multiply-list scaled-identity-weights observables-influence
    let list-of-lists list n-values num-observables [ 1 ] scaled-identity-weights
    ;; Add all of the vectors together to form the global observable weights
    set observables-weights sum-of-lists list-of-lists
end

;; Calculates a group's observable identity using group members's observables
to evaluate-identities
  ask identities [
    let data-observables[]
    let data-non-observables[]
    ask turtle-set members [
      set data-observables lput observables data-observables
      set data-non-observables lput non-observables data-non-observables
    ]
    set traits-observable list-averages data-observables
    set traits-non-observable list-averages data-non-observables
  ]
end

;; Create nodes.
to setup
  clear-all
  set observables-weights n-values num-observables [ 1 ]
  ask patches [set pcolor white] ;; make background white
  set-agents ;; initialize the agents
  set-identities ;; initialize the identities
  evaluate-identities ;; determine the observable and non-observables traits of the identities
  evaluate-identity-distance ;; Update distance measure
  evaluate-member-count ;; Update the membership count
  reset-ticks
end

;;;;;;;;;;;;;;;;;;;;;;;;
;;; Setup Procedures ;;;
;;;;;;;;;;;;;;;;;;;;;;;;

;;create identities
to set-identities
  create-identities num-nodes [
    ;set members (turtle-set turtle (who - num-nodes)) ;; match each identity with an initial individual
    set members[]
    set members (lput turtle (who - num-nodes) members)
  ]
  ask individuals [
    set ID turtle (who + num-nodes) ;; match each individual with their initial identity
  ]
end

;;create some agents
to set-agents
  create-individuals num-nodes [
    set shape "circle"
    set color orange
    set size 2
    set observables n-values num-observables [observables-variability - (2 * random-float observables-variability)] ;; create initial vector of observable traits
    set non-observables n-values num-nonobservables [non-observables-variability - (2 * random-float non-observables-variability)] ;; create initial vector of non-observable traits
    ]

  layout-circle (sort turtles) max-pxcor - 8
  ;; space out turtles to see clustering
  ask turtles
  [
    facexy 0 0
    if who mod 2 = 0 [fd 10]
  ]
end

;;;;;;;;;;;;;;;;;;;;;;;;
;;; Helper Functions ;;;
;;;;;;;;;;;;;;;;;;;;;;;;

;; Takes a set of lists and reports a list with the average values in each of the elements
to-report list-averages [list-of-lists]
  let num-lists length list-of-lists
  let list-length length first list-of-lists
  let result n-values list-length [0]

  foreach list-of-lists [
    list-item ->
    foreach range list-length [
      i ->
      set result replace-item i result (item i result + item i list-item)
    ]
  ]

  set result map [x -> x / num-lists] result

  report result
end

;; Compares the elements in the main-list with the differences between the other lists
;; and reports a list with the summed absolute differences between the main-list and other lists
to-report sum-of-differences [main-list set-of-lists]
  let diff-list []
  let i 0 ;; item 0 is the first item in a list
  foreach main-list [
    [ a ] ->
    let sum-diff 0
    foreach set-of-lists [
      [ b ] -> set sum-diff (sum-diff + abs(a - item i b))
    ]
    set i (i + 1)
    set diff-list lput sum-diff diff-list
  ]
  report diff-list
end

;; Takes a list and reports a list where min value index is 1 and all other elements set to 0
to-report replace-with-one [original-list]
  let min-index position (min original-list) original-list
  let new-list n-values length original-list [0]
  set new-list replace-item min-index new-list 1
  report new-list
end

;; Scales up a list by a scalar value
to-report multiply-list [original-list scalar]
  let new-list []
  foreach original-list [
    a ->
      let multiplied-item a * scalar
      set new-list lput multiplied-item new-list
  ]
  report new-list
end

;; Adds up a list of lists and reports a list with their sum
to-report sum-of-lists [set-of-lists]
  let sum-list[]
  let i 0 ;; item 0 is the first item in a list

  repeat num-observables [
    let sum-amount 0
    foreach set-of-lists [
      [ a ] ->
      set sum-amount sum-amount + item i a
    ]
    set i (i + 1)
    set sum-list lput sum-amount sum-list
  ]
  report sum-list
end

;; Multiply values element by element between two lists and reports a list of their product
to-report product-of-lists [list1 list2]
  let product-list[]
  (foreach list1 list2
    [ [a b] -> set product-list lput (a * b) product-list ])
  report product-list
end

;; Subtract values element by element between two lists and reports a list of their absolute difference
to-report difference-of-lists [list1 list2]
  let diff-list[]
  (foreach list1 list2
    [ [a b] -> set diff-list lput abs(a - b) diff-list ])
  report diff-list
end

;; Takes two lists and reports distance between them
to-report euclidean-distance [list1 list2]
  let distance-measure 0
  (foreach list1 list2
    [ [a b] -> set distance-measure distance-measure + (a - b) * (a - b) ])
  report sqrt(distance-measure)
end

;; Takes a difference as an input and reports it as euclidean-distance
to-report difference-euclidean-distance [difference]
  let distance-measure 0
  foreach difference
  [ [ a ] -> set distance-measure distance-measure + a * a]
  report sqrt(distance-measure)
end

;; Takes the sum distance between a main list and a list of lists
to-report group-distance [main-list set-of-lists]
  let distance-measure 0
  foreach set-of-lists [
      [ a ] -> set distance-measure (distance-measure + euclidean-distance main-list a)
    ]
  report (distance-measure / length set-of-lists)
end
@#$#@#$#@
GRAPHICS-WINDOW
11
375
180
545
-1
-1
1.0
1
10
1
1
1
0
0
0
1
-80
80
-80
80
0
0
1
ticks
30.0

SLIDER
5
48
177
81
num-nodes
num-nodes
0
1000
1000.0
1
1
NIL
HORIZONTAL

SLIDER
5
87
177
120
num-observables
num-observables
0
10
10.0
1
1
NIL
HORIZONTAL

SLIDER
5
124
177
157
num-nonobservables
num-nonobservables
0
10
0.0
1
1
NIL
HORIZONTAL

BUTTON
1
10
64
43
NIL
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
141
10
204
43
NIL
go
T
1
T
OBSERVER
NIL
NIL
NIL
NIL
0

BUTTON
65
10
140
43
go once
go
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
0

MONITOR
243
10
369
55
Number of Identities
count identities
1
1
11

PLOT
243
61
767
547
Group Identities
Membership Size
Identity Distance
0.0
25.0
0.0
5.0
true
false
"" ";;erase what was plotted before\nplot-pen-reset"
PENS
"default" 1.0 2 -12345184 true "" "foreach sort identities [ x ->\nplotxy [member-count] of x [identity-distance] of x\n]"

SLIDER
6
162
178
195
observables-influence
observables-influence
0
10
1.0
0.01
1
NIL
HORIZONTAL

SLIDER
7
200
179
233
group-size-utility
group-size-utility
1
2.5
2.5
0.01
1
NIL
HORIZONTAL

SLIDER
7
237
179
270
distance-penalty
distance-penalty
0
2
0.25
0.01
1
NIL
HORIZONTAL

SLIDER
7
275
181
308
observables-variability
observables-variability
0
2
1.0
0.01
1
NIL
HORIZONTAL

SLIDER
7
312
207
345
non-observables-variability
non-observables-variability
0
2
1.0
0.01
1
NIL
HORIZONTAL

MONITOR
376
10
765
55
NIL
observables-weights
17
1
11

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
NetLogo 6.3.0
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
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
