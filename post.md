# Lifty Sim

I was challenged to build an elevator controller. As usual, I started overthinking it a little, and so I got ~~inspired~~ an excuse to build an interactive playground for exploring the problem.

Lifts provide an interesting design space with subtle tradeoffs (for example in time and energy), that spawned many research papers, just see ["elevator scheduling" on Google Scholar](https://scholar.google.com/scholar?q=elevator+scheduling).
Instead of choosing a favorite algorithm and implementing it, we will focus on building a visual intuition of the algorithms' behaviors through live interaction in the spirit of [Explorable Explanations](http://explorableexplanations.com).

We'll use [Elm](elm-lang.org), for the following reasons
- it is a language specifically created for interactive browser apps
- it has a simple but neat static type system that lets us describe and enforce our logic effectively and rules out runtime type errors and exceptions
- it is built on a simple variant of functional reactive programming that lets us create interactive apps in a (mostly) pure functional setting, without callbacks, using immutable data structures

Elm's intentional simplicity and parsimony of language constructs worked out remarkably, as evidenced by its blossoming, easy-to-use [library ecosystem](http://package.elm-lang.org), despite lacking even [type classes](https://en.wikipedia.org/wiki/Type_class), widely considered a basic construct in statically typed functional languages.

As usual, this simplicity comes at a cost, which I will occasionally point out. The general theme is that it's sometimes hard to express static invariants of our program and have the type system enforce it.


### Scope

We will limit ourselves to essential aspects of the problem. In particular, we will *not* model the following:
- elevator door and door sensor
- occupancy sensor
- overweight sensor
- emergency features
- lights
- acceleration
We will deal with the above by simulating ideal users: they respect the passenger limit, only enter or exit the cabin when it arrives at a platform, and never have emergencies. Also, the cabin will move at constant speed.


## Simple Controller

We'll start with a very simple controller that has one call button on every floor and destination buttons for each floor in every lift. A lift can only be called if one is on standby, and the closest one will come. A lift can only be started if it is not currently moving. Otherwise, commands are ignored.

We add some synonyms to make type signatures clearer.

```
  type alias FloorId = Int
  type alias LiftId = Int
```

The commands our controller accpets are `call` and `go` button presses.

```
type alias Lift a = { a | dest : FloorId, moving : Bool }
type alias Model a = Array (Lift a)
```

We model events that passengers will want to react to: a lift making a stop at a floor, and a lift becoming available.

```
```

The state of our controller is just the state of each lift, which is a source and destination floor. The lift is stopped if these are equal. We want to fetch and update lift states by index, so we use an immutable Array instead of a List.

```
```

Next is our actual controller. It is a function that interprets actions and "moves" the cabins when needed by scheduling state updates and events.

```

```

That's all we need for our simple controller. You can play around with it below: click the green circle to call an elevator. Click a floor number in an elevator shaft to send the elevator there.


```embed
```
[TODO] You can edit the code and watch the live debug mode here.



### Problems

This system is woefully inefficient. In our simple building,
* a lift can only be called when one is empty
* lifts will not stop to pick up more passengers on the way, even if there is room

In addition, it requires a "smart" user, ie. there is a lot of logic that users need to implement to be able to use the lift at all.
* wait until a lift is in standby to press the call button
* decide which floor to go to next, as a group, when more than one person is in the lift
* remember which direction the lift came from, and if there are other users in it; only enter if it is going in the right direction


## Simulation

Before improving our controller, let's simulate a a building with passengers queueing up to use the lifts.

```
type Building

type Simulation
```

Unfortunately, we need to simulate the complex user behavior described above. We'll cut some corners and implement the whole getting in and out of the cabin and selecting the next stop in one function that works at the intersection of a floor (representing a queue of passengers) and a lift (). It is called when the `Arrive` event happens and results in either the next Go command for that lift, or a Standby event.


..............code...............


In addition, passengers will react to `Standby` events and call a lift. If there are passengers on multiple floors, each command is sent, here implicitly ordered starting with the lowest floor number.

................code..............

Here's an upgraded UI featuring passengers as blue blobs. To add a passenger, click the plus sign on the left of a floor to select the starting floor, and then click on the destination floor to select it. A user will be added



## Directional Controller

We can now model a more advanced controller. Here's how it works:
- two call buttons on each floor, one for going up and one for going down
- the controller remembers call button presses until they are serviced
- each lift remembers selected destinations
- lifts keeps going in one direction until there are no more stops scheduled in that direction
- lifts make stops for each selected destination and each call that matches the direction the lift is going in

This is probably the most commonly found lift controller. It increases efficiency and fairness while maintaining a simple interface. Although there are more buttons (two on each floor), it is simpler to use, so we can remove some of the passenger simulation logic. Specifically, passengers don't need to decide where the lift should go next, they just press the button for their destination when they enter the lift.


...... code ........





### Problems

This system is still inefficient.
* the lift stops even when it is full
* TODO


## Smart Controller

You live in an engineering college dorm, with thousands of people moving through tens of floors. Being engineers, you and your friends set out to fix the problems above and stop wasting time and power, gaining an eco badge in the process.

You realize that the inefficiencies stem from the knowledge gap between the controller and the passenger. If the lift knew where people want to go, it could calculate a more efficient itinerary. It could also skip pickups when full.

So let's tell it. We change the user interface: instead of directional calling buttons, there are buttons for each destination floor. The passenger presses the button for the floor where they want to go. When a lift arrives, a display shows which destinations it will go to. For each displayed destination, the first person in queue for that destination boards the lift. There are no controls inside the cabin (except emergency stop, which we will not model), you simply get off at your destination.

It sounds a little bit complicated, but in reality it's very easy to get used to. By the way, I'm not making this up. The lift in my university's dorm works like this, saving hours of waiting time and kilowatt-hours of energy each day.


```
........code...........
```

There is little change required to our passenger simulator. We can throw out even more logic

```embed
```

In the spirit of skepticism, I tried to find some drawbacks of this controller. I found that usability problems arise only when the lift is used incorrectly, for example if the passenger:
* presses the wrong destination button
* changes her mind mid-trip
* boards the lift when his destination is not displayed
* forgets to disembark at the destination

Our sims are fortunately "perfectly stupid", that is they can only do very simple things, but they do those perfectly.


## Comparing Controllers

We'd like to evaluate these options side by side, and see how they measure up when subjected to the same load.





## Challenge Ideas

Improve the controllers and simulations by implementing acceleration. Add other features from the Scope section.

Try to optimize wait time by sending empty elevators to predefined positions determined by analyzing traffic, at the expense of power use.

## Space Challenge

You are building a one million km long space lift that can accelerate to 0.5c. Build a controller that minimizes travel time, accounting for relativistic time dilation.
