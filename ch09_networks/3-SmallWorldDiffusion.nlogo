;;Simple and complex contagion on small-world networks.


;;;;;;;;;;;;;;;;;;;;;;;;
;;; Model Parameters ;;;
;;;;;;;;;;;;;;;;;;;;;;;;

globals
[
  num-infected ;; how many agents are infected?
  infected-size ;;the size of the infected agents
]

turtles-own
[
  infected? ;; true if agent has been infected
]



;;;;;;;;;;;;;;;;;;;;;;;;
;;; Setup Procedures ;;;
;;;;;;;;;;;;;;;;;;;;;;;;


to setup
  clear-all
  set infected-size 5
  set-default-shape turtles "outlined circle"
  make-turtles ;;create agents
  wire-lattice ;;arrange them in a ring lattice
  rewire-network ;;apply the small-world algorithm
  infect-two ;;infect two agents.
  reset-ticks
end


to make-turtles
  create-turtles num-nodes [ reset-node ]
  ;; arrange turtles in an ordered, staggered circle
  layout-circle (sort turtles) max-pxcor - 8
  ;; space out turtles to see clustering
  ask turtles
  [
    facexy 0 0
    if who mod 2 = 0 [fd 10]
  ]
end

to reset-node
    set color gray + 1.5
    set size 4
    set infected? false
end


;; creates the links for the ring lattice
to wire-lattice
  let degree 4 ;;assert degree 4. This can be modified for arbitrary degree
  ;; iterate over the nodes
  let n 0
  while [n < count turtles]
  [
    ;; iterate over pairs of edges
    let k 1
    while [k <= degree / 2]
    [
      make-edge turtle n turtle ((n + k) mod count turtles)
      set k k + 1
    ]
    set n n + 1
  ]
end

;; connects the two nodes
to make-edge [node1 node2]
  ask node1 [ create-link-with node2 [ set color gray + 1.5] ]
end


;; Rewire the network connections
;; WARNING: the simplified rewiring algorithm does not ensure that the network will be completely connected
;; for large networks this shouldn't be too much of an issue.
to rewire-network
  ask links
  [
    if (random-float 1) < rewiring-probability
    [
      ask end1
      [
        create-link-with one-of other turtles with [not link-neighbor? myself ]
          [set color gray + 1.5]
      ]
      die
    ]
  ]
end


;;infect two neighbors with the contagion
to infect-two
    ask turtles
    [reset-node]
  ask links
    [set color gray + 1.5]

  ;; infect a single agent
  ask one-of turtles
  [
    set infected? true
    set color yellow
    set size infected-size
    ask one-of link-neighbors [
          set infected? true
          set color yellow
          set size infected-size
    ]
  ]
  set num-infected 2
end




;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Dynamics Procedures ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;


to go
  ;; stop if every agent has already been infected
  if all? turtles [infected?]
    [stop]

  ask turtles with [ infected? = true ]
  [
    ;; infect neighbors
    ask link-neighbors with [not infected?]
      [
        ;; Logic Flow
        let yes-infect false ;; by default, do not infect
        ifelse (complex-contagion?)
        [if ( count link-neighbors with [infected? = true] > 1 or random-float 1 <= prob-spread-one ) [set yes-infect true] ] ;; infect with next to 2+ infected OR infect with probability q
        [if ( random-float 1 <= prob-infection ) [set yes-infect true] ] ;; infect with probability p

        if ( yes-infect = true )
        [
          set infected? true
          set color yellow
          set size infected-size

          ;; color the link with the node doing the infection for viz purposes only
          ask link-with myself [set color yellow]

        ]
    ]
  ]
  set num-infected count turtles with [infected? = true]
  tick
end

; Copyright 2023 Paul E. Smaldino.
; See Info tab for full copyright and license.

@#$#@#$#@
GRAPHICS-WINDOW
295
10
777
493
-1
-1
2.9441
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
1
1
1
ticks
30.0

SLIDER
9
125
189
158
num-nodes
num-nodes
100
500
100.0
1
1
NIL
HORIZONTAL

SLIDER
8
161
189
194
rewiring-probability
rewiring-probability
0
1
0.1
0.001
1
NIL
HORIZONTAL

PLOT
8
237
285
431
proportion infected
time
 infected
0.0
1.0
0.0
1.0
true
false
"" ""
PENS
"inf" 1.0 0 -2674135 true "" "plot (num-infected ) / (count turtles)"

SLIDER
8
196
189
229
prob-infection
prob-infection
0
1
0.1
0.01
1
NIL
HORIZONTAL

BUTTON
10
47
131
81
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

MONITOR
194
176
287
221
NIL
num-infected
17
1
11

BUTTON
10
10
131
44
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

SWITCH
11
87
190
120
complex-contagion?
complex-contagion?
0
1
-1000

SLIDER
8
436
137
469
prob-spread-one
prob-spread-one
0
1
0.001
0.001
1
NIL
HORIZONTAL

@#$#@#$#@
## Model Information and Materials

This model is original material created by Paul E. Smaldino, building on work by Centola and Macy (2007). 

* Centola D, Macy M (2007) Complex contagions and the weakness of long ties. American Journal of Sociology 113: 702-734.

## References and Citation

For this model:

* Smaldino PE (2023). Small World Diffusion. Modeling Social Behavior.  https://github.com/psmaldino/modsoc/

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

link
true
0
Line -7500403 true 150 0 150 300

link direction
true
0
Line -7500403 true 150 150 30 225
Line -7500403 true 150 150 270 225

outlined circle
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 false false -1 -1 301

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

x
false
0
Polygon -7500403 true true 270 75 225 30 30 225 75 270
Polygon -7500403 true true 30 75 75 30 270 225 225 270
@#$#@#$#@
NetLogo 6.3.0
@#$#@#$#@
setup
repeat 5 [rewire-one]
@#$#@#$#@
@#$#@#$#@
<experiments>
  <experiment name="time-to-spread-simple" repetitions="100" runMetricsEveryStep="false">
    <setup>generate-topology
infect-two</setup>
    <go>spread</go>
    <timeLimit steps="300"/>
    <exitCondition>rewiring-probability &gt; 1</exitCondition>
    <metric>ticks</metric>
    <metric>pct-infected</metric>
    <enumeratedValueSet variable="rewiring-probability">
      <value value="0"/>
      <value value="0.001"/>
      <value value="0.002"/>
      <value value="0.003"/>
      <value value="0.004"/>
      <value value="0.005"/>
      <value value="0.006"/>
      <value value="0.007"/>
      <value value="0.008"/>
      <value value="0.009"/>
      <value value="0.01"/>
      <value value="0.02"/>
      <value value="0.03"/>
      <value value="0.04"/>
      <value value="0.05"/>
      <value value="0.06"/>
      <value value="0.07"/>
      <value value="0.08"/>
      <value value="0.09"/>
      <value value="0.1"/>
      <value value="0.2"/>
      <value value="0.3"/>
      <value value="0.4"/>
      <value value="0.5"/>
      <value value="0.6"/>
      <value value="0.7"/>
      <value value="0.8"/>
      <value value="0.9"/>
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="num-nodes">
      <value value="500"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prob-infection">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="complex-contagion?">
      <value value="false"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="time-to-spread-complex" repetitions="100" runMetricsEveryStep="false">
    <setup>generate-topology
infect-two</setup>
    <go>spread</go>
    <timeLimit steps="300"/>
    <exitCondition>rewiring-probability &gt; 1</exitCondition>
    <metric>ticks</metric>
    <metric>pct-infected</metric>
    <enumeratedValueSet variable="rewiring-probability">
      <value value="0"/>
      <value value="0.001"/>
      <value value="0.002"/>
      <value value="0.003"/>
      <value value="0.004"/>
      <value value="0.005"/>
      <value value="0.006"/>
      <value value="0.007"/>
      <value value="0.008"/>
      <value value="0.009"/>
      <value value="0.01"/>
      <value value="0.02"/>
      <value value="0.03"/>
      <value value="0.04"/>
      <value value="0.05"/>
      <value value="0.06"/>
      <value value="0.07"/>
      <value value="0.08"/>
      <value value="0.09"/>
      <value value="0.1"/>
      <value value="0.2"/>
      <value value="0.3"/>
      <value value="0.4"/>
      <value value="0.5"/>
      <value value="0.6"/>
      <value value="0.7"/>
      <value value="0.8"/>
      <value value="0.9"/>
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="num-nodes">
      <value value="500"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prob-infection">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="complex-contagion?">
      <value value="true"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="experiment 1" repetitions="100" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="10000"/>
    <metric>num-infected</metric>
    <enumeratedValueSet variable="prob-spread-one">
      <value value="0.001"/>
      <value value="0.01"/>
      <value value="0.1"/>
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rewiring-probability">
      <value value="0.001"/>
      <value value="0.01"/>
      <value value="0.1"/>
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="num-nodes">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="complex-contagion?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prob-infection">
      <value value="0.1"/>
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
