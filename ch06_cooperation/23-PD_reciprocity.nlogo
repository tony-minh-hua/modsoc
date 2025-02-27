;; Iterated prisoner's dilemma game with pure strategies and TFT
;; This model allows randomization to spatial location, which disrupts assortment
;; TFT agents use contingent reciprocity


turtles-own [
  strategy ;0=defector, 1=cooperator, 2=TFT
  payoff ;;payoff
]

;;----SETUP PROCEDURES---------------------

to setup
  clear-all
  make-agents
  reset-ticks
end


;;create some agents, one on each patch. Make some of them cooperators/TFT agents
to make-agents
  let weight (cooperator-weight + defector-weight + tft-weight)
  ask patches [
    let rand (random-float weight)
    sprout 1 [
      (ifelse
      rand < cooperator-weight or weight = 0 ;Default set agents to cooperate if no weights
      [ set strategy 1 ]
      rand < (cooperator-weight + defector-weight)
      [ set strategy 0 ]
      rand < (cooperator-weight + defector-weight + tft-weight)
      [ set strategy 2 ]
        )
      set shape "circle"
      set payoff 0
    ]
  ]
  recolor
end

;;recolor agents based on strategy
;;red = defector, blue = cooperator, green = TFT
to recolor
  ask turtles[
    if strategy = 0
    [ set color red]

    if strategy = 1
    [set color blue]

    if strategy = 2
    [set color green]
  ]
end

;;----DYNAMICS PROCEDURES---------------------

to go
  randomize-locations ;;agents move around to prevent assortment of similar strategies
  play-game ;;agents play the game with neighbors and receive payoffs
  evolve ;;agents copy the strategy of their most successful neighbor
  recolor ;;recolor agents to match their strategy
  tick
end


;;have each turtle switch locations with a random other turtle with some probability
to randomize-locations
  ask turtles [
    if random-float 1 < randomization-prob [
      let my-x xcor
      let my-y ycor
      let partner one-of other turtles
      let their-x [xcor] of partner
      let their-y [ycor] of partner
      set xcor their-x
      set ycor their-y
      ask partner [
        set xcor my-x
        set ycor my-y
      ]
    ]
  ]
end


;;each agent plays the PD game with each of its 4 closest neighbors and accumulates payoff
;;note the calculation must account for iterated game
to play-game
  ;count neighbors with each strategy
  ask turtles [
    set payoff 0
    let neighbors-C count (turtles-on neighbors4) with [strategy = 1]
    let neighbors-D count (turtles-on neighbors4) with [strategy = 0]
    let neighbors-T count (turtles-on neighbors4) with [strategy = 2]

    ; set upper bounds to equal lower bound if upper bound is smaller than lower-bound
    if (num-iterations-low > num-iterations-high)
    [ set num-iterations-high num-iterations-low ]

    let num-iterations ((num-iterations-low + num-iterations-high) / 2)

    ; reassign value based on uniform distribution
    if probabilistic
    [ set num-iterations random (num-iterations-high - num-iterations-low) + num-iterations-low ]

    ;if I'm a cooperator
    if (strategy = 1)
    [ set payoff num-iterations * ((neighbors-C + neighbors-T) * (payoff-benefit - payoff-cost) - neighbors-D * payoff-cost) ]

    ;;if I'm a TFT cooperator (and TFT? = true)
    if (strategy = 2)
    [ set payoff num-iterations * ((neighbors-C + neighbors-T) * (payoff-benefit - payoff-cost))  - (neighbors-D * payoff-cost) ]

    ;if I'm a defector
    if (strategy = 0)
      [ set payoff ((neighbors-T * payoff-benefit) + num-iterations * (neighbors-C * payoff-benefit)) ] ;;exploiting TFT and cooperator agents
  ]
end

;;each agent imitates their most successful neighbor
to evolve
  let weight (cooperator-mutation-weight + defector-mutation-weight + tft-mutation-weight)
  ask turtles[
  let best-neighbor max-one-of (turtles-on neighbors4) [payoff] ;;who's my neighbor with the highest payoff
  if ([payoff] of best-neighbor) > payoff ;;if their payoff is better than mine, imitate them
    [set strategy [strategy] of best-neighbor]

  if random-float 1 < mutation-prob [
      let rand (random-float weight)
    (ifelse
      rand < cooperator-mutation-weight or weight = 0 ;Default set agents to cooperate if no weights
      [ set strategy 1 ]
      rand < (cooperator-mutation-weight + defector-mutation-weight)
      [ set strategy 0 ]
      rand < (cooperator-mutation-weight + defector-mutation-weight + tft-mutation-weight)
      [ set strategy 2 ]
        )
    ]
  ]
end


to-report coop-freq
  let x count turtles with [strategy = 1]
  report x / (count turtles)
end


; Copyright 2023 Paul E. Smaldino.
; See Info tab for full copyright and license.
@#$#@#$#@
GRAPHICS-WINDOW
235
18
562
346
-1
-1
10.3
1
10
1
1
1
0
1
1
1
-15
15
-15
15
0
0
1
ticks
30.0

SLIDER
46
104
225
137
cooperator-weight
cooperator-weight
0
1
0.6
.01
1
NIL
HORIZONTAL

SLIDER
46
217
225
250
payoff-benefit
payoff-benefit
0
1
1.0
.01
1
NIL
HORIZONTAL

SLIDER
46
259
225
292
payoff-cost
payoff-cost
0
1
0.9
.01
1
NIL
HORIZONTAL

BUTTON
47
18
136
51
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
144
18
218
51
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

PLOT
568
19
883
346
Frequency of Cooperation
Time
Cooperator frequency
0.0
10.0
0.0
1.0
true
false
"" ""
PENS
"default" 1.0 0 -13345367 true "" "plot (count turtles with [strategy = 1 or strategy = 2]) / (count turtles)"

BUTTON
91
58
172
91
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
1

SLIDER
47
301
226
334
randomization-prob
randomization-prob
0
1
0.0
.01
1
NIL
HORIZONTAL

SLIDER
46
140
225
173
defector-weight
defector-weight
0
1
0.1
0.01
1
NIL
HORIZONTAL

SLIDER
47
177
225
210
tft-weight
tft-weight
0
1
1.0
0.01
1
NIL
HORIZONTAL

SLIDER
29
366
219
399
cooperator-mutation-weight
cooperator-mutation-weight
0
1
1.0
0.01
1
NIL
HORIZONTAL

SLIDER
28
405
220
438
defector-mutation-weight
defector-mutation-weight
0
1
1.0
0.01
1
NIL
HORIZONTAL

SLIDER
29
443
219
476
tft-mutation-weight
tft-mutation-weight
0
1
1.0
0.01
1
NIL
HORIZONTAL

SLIDER
28
482
200
515
mutation-prob
mutation-prob
0
1
0.05
0.01
1
NIL
HORIZONTAL

PLOT
569
351
884
563
Frequency of Types
NIL
NIL
0.0
10.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 0 -2674135 true "" "plot count turtles with [strategy = 0]"
"pen-1" 1.0 0 -13345367 true "" "plot count turtles with [strategy = 1]"
"pen-2" 1.0 0 -10899396 true "" "plot count turtles with [strategy = 2]"

SWITCH
260
369
376
402
probabilistic
probabilistic
1
1
-1000

SLIDER
384
409
564
442
num-iterations-high
num-iterations-high
0
20
5.0
1
1
NIL
HORIZONTAL

MONITOR
382
490
557
535
num-iterations
(num-iterations-low + num-iterations-high) / 2
17
1
11

SLIDER
382
446
565
479
num-iterations-low
num-iterations-low
0
20
5.0
1
1
NIL
HORIZONTAL

@#$#@#$#@
## Model Information and Materials

This model is original material created by Paul E. Smaldino. 

## References and Citation

For this model:

* Smaldino PE (2023). Prisoner's Dilemma Game: Reciprocity. Modeling Social Behavior.  https://github.com/psmaldino/modsoc/

For the book:

* Smaldino PE (2023). Modeling Social Behavior: Mathematical and Agent-Based Models of Social Dynamics and Cultural Evolution. Princeton University Press. 

For the NetLogo:

* Wilensky, U. (1999). NetLogo. http://ccl.northwestern.edu/netlogo/. Center for Connected Learning and Computer-Based Modeling, Northwestern University, Evanston, IL.


## Licence

![CC BY-NC-SA 4.0](http://ccl.northwestern.edu/images/creativecommons/byncsa.png)

This work is licensed under a Creative Commons Attribution-NonCommercial-ShareAlike 4.0 International License. http://creativecommons.org/licenses/by-nc-sa/4.0/
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
<experiments>
  <experiment name="experiment" repetitions="20" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="100"/>
    <metric>coop-freq</metric>
    <enumeratedValueSet variable="payoff-benefit">
      <value value="1"/>
    </enumeratedValueSet>
    <steppedValueSet variable="num-iterations" first="1" step="1" last="20"/>
    <enumeratedValueSet variable="init-coop-freq">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="payoff-cost">
      <value value="0.2"/>
      <value value="0.4"/>
      <value value="0.6"/>
      <value value="0.8"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="TFT?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="randomization-prob">
      <value value="0"/>
      <value value="1"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="experiment" repetitions="10" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="100"/>
    <metric>count turtles with [strategy = 0]</metric>
    <metric>count turtles with [strategy = 1]</metric>
    <metric>count turtles with [strategy = 2]</metric>
    <metric>count turtles with [strategy = 1 or strategy = 2] / count turtles</metric>
    <enumeratedValueSet variable="cooperator-weight">
      <value value="0.6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="probabilistic">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="num-iterations-high">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="num-iterations-low">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="payoff-benefit">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mutation-prob">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tft-mutation-weight">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="cooperator-mutation-weight">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tft-weight">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="defector-weight">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="payoff-cost">
      <value value="0.6"/>
      <value value="0.9"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="defector-mutation-weight">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="randomization-prob">
      <value value="0"/>
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
