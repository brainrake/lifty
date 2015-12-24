module Main where

import Markdown
import Html exposing (node, div, iframe)
import Html.Attributes exposing (src, width, height, style)

md = Markdown.toHtml

include w h s = iframe [src s, width (w+10), height (h+10), style [("border","0")]] []

main = div [] [ md """

# Exploring Elevators

I was challenged to build an elevator controller. As usual, I started overthinking things a little, and so I got ~~inspired~~ an excuse to build an interactive playground for exploring the problem.

Lifts provide an interesting design space with subtle tradeoffs, that spawned many research papers, just see ["elevator scheduling" on Google Scholar](https://scholar.google.com/scholar?q=elevator+scheduling).
Instead of choosing a favorite algorithm and implementing it, we will focus on building a visual intuition of the logic's behavior through live interaction in the spirit of [Explorable Explanations](http://explorableexplanations.com).

We will also explore the interface between language and computation: types. Types give structure and meaning to otherwise homogenous data and control flow by expressing logical relations and invariants. In languages with strong type systems, programs with valid (meaningful) types will never go wrong (e.g. crash or throw an exception).

[Elm](elm-lang.org) will serve well for this dual purpose, for the following reasons:
- it is a small functional language specifically created for interactive browser apps
- it has a simple but neat strong static type system
- it employs functional reactive programming through `Signals`, a reasonable way to handle interactivity in a (mostly) pure functional setting

Elm's intentional simplicity and parsimony of language constructs worked out remarkably, as evidenced by its blossoming, easy-to-use [library ecosystem](http://package.elm-lang.org). This simplicity comes at a cost though, which I will occasionally point out.


#### Scope

We will limit ourselves to essential aspects of the problem. In particular, we will *not* model the following:

- elevator door and door sensor
- occupancy sensor
- overweight sensor
- emergency features
- lights
- acceleration

We simuate Ideal users, who respect the passenger limit, only enter or exit the cabin when it arrives at a platform, and never have emergencies. Also, the cabin will move at constant speed.


### Simple Controller

We'll start with a very simple controller, like the ones found in old apartment buildings with few floors. There is one call button on every floor and destination buttons for each floor in every lift. A lift can only be called if one is on standby, and the closest one will come. A lift can only be started if it is not busy. Otherwise, commands are ignored. When a lift arrives at a platform, it becomes available after a fixed delay.

We attack the problem at the interface of language and computation: types. We directly model all of the inputs of the controller in a simple algebraic datatype. This includes user input like `call` and `go` button presses, as well as notifications when a lift arrives at a floor, and when it is no longer busy. This type defines a unique language and a universe of possible computation.

```
type Action = Call FloorId
            | Go LiftId FloorId
            | Arrive LiftId FloorId
            | Idle LiftId
```
`LiftId` and `Floorid` mean `Int`. To keep things simple, we don't make a distinction between user generated events and events that "just happen" because the state of the universe evolves. Our controller might be used incorrectly by passing Arrive or Standby events to it that weren't supposed to happen. It's the equivalent of the lift's sensors malfunctioning.

Next, we'll model the state of our controller, that is the stuff it has to remember between events. Our simple controller only cares wether a Lift is in use, and what its destination floor is. It doesn't need to know where the lift is exactly, or where it's coming from, or what is inside, so we won't specify that for now.

```
type alias Lift l = { l | busy : Bool, dest: FloorId }
type alias State s l = { s | lifts : Array (Lift l) }
```

Notice that we left the `Lift` and `State` types polymorphic, using extensible records. This means we can later attach any other properties to the state and to each lift (like passengers carried, or animation data) without having to modify our controller. It simply won't touch anything it doesn't know about. Also, we want to fetch and update lift states by index, so we use an immutable Array that conveniently provides such functions, instead of a List.

Next is our actual controller. The function updates the state in response to actions, and maybe also results in an action to be scheduled for later.

```
update : Action -> State a -> (State a, Maybe (Time.Time, Action))
```

Writing `update` is fairly straightforward, as Elm provides the usual functional tools, with an unassuming and regular syntax that closely resembles SML, plus records. No do syntax, operator sections or other extensions are available. There are few combinators and infix operators in the libraries, as well as few specialized or rarely used functions, even for common data types. This keeps the libraries' surface very small and quickly comprehensible, and it's easy enough to define helpers.

The lack of typeclasses means that common operations like mapping or filtering Sets, Lists, Arrays are implemented (or not) separately for each datatype, sometimes named inconsistently, although arguably more in line with what one would expect from the data type at hand.

In order to run our controller function, we need a machine that feeds it user input as well as the delayed events it produces.

To test the controller, I've built a visual interface using the [elm architecture](https://github.com/evancz/elm-architecture-tutorial/) built on its first-order FRP implentation in [`Signals`]() as well as  `Tasks`, `Effects`, `ports`. You can play with it below. Click the green circle to call an elevator. Click a destination in an elevator shaft to send the elevator there. Floor numbers start from 0 and grow downwards, like most things in computer science. Just imagine an underground building.


""", include 240 240 "out/OneUI.html", md """


#### Simulation

Before improving our controller, let's simulate a a building with passengers queueing up to use the lifts.

In addition to the controller's input, we can also add a passenger to the simulation.

```
type Action = AddPassenger FloorId FloorId
            | Action C.Action
```

Now We compose the simulation state type and the controller state type `C.Type`

```
type alias Passenger p = { p | dest: FloorId }

type alias Lift l p = { l | pax : List (Passenger p) }

type alias State s l p =
  C.State { s | floors : Array (List (Passenger p)) } (Lift l (Passenger p))
```

We need to simulate the user behavior described above, so we need an update function. The code in [OneSim.elm]() gets a bit more complex than this, although it also handles passenger animations, for which we track the indices of passengers in the queue.

In the `update` function, passengers react to lift sensor events: they enter and exit lifts when they `Arrive` at a floor, and send or call a lift when it becomes `Idle`.

Note that animations in reaction to queued events, and keeping track of time are dealt with similarly in [OneSimView.elm](), keeping things nicely separated. [OneSimUI.elm]() puts it all together, hooks up signals and ports and provides a main function.


""", include 360 240 "out/OneSimUI.html", md """


To add a passenger, click the plus sign to the right of the starting floor, and then click the plus sign on the destination floor.

Here, A lift holds at most 2 passengers. At most 4 passengers will wait on a floor, any more would prefer the stairs.


#### Room for improvement

This system is woefully inefficient. In our simple building,
- a lift can only be called when one is empty
- lifts will not stop to pick up more passengers on the way, even if there is room

There is a lot of logic that users need to implement to be able to use the lift at all.
- wait until a lift is in standby to press the call button
- decide which floor to go to next, as a group

On the language side, our nested polymorphic records have gotten somewhat entangled. Also, We can't have two modules reuse each other's type aliases when sharing a record, to avoid circular imports. We could move the types to separate modules which both import. The quick solution is to just leave off the type annotations. When correct, Elm will gladly infer the types for us and everything magically works, but it's hard for the type checker to determine where exactly a type error is in a call graph of unannotated function calls. We could also make the parts completely independent and send messages (like `Action`s) between them, but that is also cumbersome, largely negating the advantages of records.


### Directional Controller

We will now model a more advanced controller:
- two call buttons on each floor, one for going up and one for going down
- the controller remembers call button presses until they are serviced
- each lift remembers selected destinations
- lifts keeps going in one direction until there are no more stops scheduled in that direction
- lifts make stops for each selected destination and each call that matches the direction the lift is going in

This is probably the most commonly found type of controller. It increases efficiency and fairness while maintaining a simple interface. Although there are more buttons (two on each floor), it is simpler to use, so we can remove some of the passenger simulation logic. Specifically, passengers don't need to decide where the lift should go next, they just press the button for their destination when they enter the lift.

```
type Action = CallUp FloorId
            | CallDown FloorId
            | Go LiftId FloorId
            | Approach LiftId FloorId
            | Arrive LiftId FloorId
            | Idle LiftId
```

The inputs are a bit more detailed than before. `Approach` is scheduled before arriving at a floor, in time to schedule a stop and `Arrive` at the floor. The rest should be familiar.

```
type alias Lift l = { l | busy : Bool
                        , up: Bool
                        , next: FloorId
                        , dests: Set FloorId }
```
Our lifts now also remember their directions and set of destinations (button presses inside the cabin).

```
type alias State s l = { s | lifts : Array (Lift l)
                           , calls_up : Set FloorId
                           , calls_down : Set FloorId}
```

The controller remembers call button presses until they are serviced.

The `update` function in [TwoController.elm]() is somewhat more involved this time, with many edge cases to cover.

""", include 240 240 "out/TwoUI.html", md """



""", include 360 240 "out/TwoSimUI.html", md """


This system is still inefficient.
* the lift stops even when it is full
* it is unfair: if the lift always fills up on the ground floor, noone on the first floor can ever go up


### Comparing Controllers

We'd like to evaluate these options side by side, and see how they measure up when subjected to the same load.


""", include 960 600 "out/CompareUI.html", md """


#### What's missing

Live code editing and time-travel debugging, pending updates and a fix in `elm-reactor`.


#### Challenge Ideas

Improve the controllers and simulations by implementing acceleration. Add other features from the Scope section.

Try to optimize wait time by sending empty elevators to predefined positions determined by analyzing traffic, at the expense of power use.

#### Space Challenge

You are building a one million km long space lift that can accelerate to 0.5c. Build a controller that minimizes total experienced (proper) travel time, accounting for relativistic time dilation.
""" ]
