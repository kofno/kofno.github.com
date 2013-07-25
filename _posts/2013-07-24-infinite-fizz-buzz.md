---
layout: default
title: "Infinite FizzBuzz: Lazy Programming in Ruby"
---

{{ page.title }}
----------------

One of the first things I wrote when I started to learn Haskell was FizzBuzz.
More challenging as a drinking game then a programming exercise, its still a
good exercise for just getting some code to run. And afer some fiddling, I
eventually ended up with something that looked like this:

<script src="https://gist.github.com/kofno/6075856.js?file=FizzBuzz.hs">&nbsp;</script>

My _actual_ first try used a list comprehension, rather then a map, but the
resemblence to a Ruby implementation is undenialble. In fact, here is a Ruby
implementation of FizzBuzz.

<script src="https://gist.github.com/kofno/6075856.js?file=fizz_buzz.rb">&nbsp;</script>

In fact, they are called pretty much the same way

    ghci> fizzBuzz [1..20]

    irb> fizz_buzz 1..20

Well, that was largely unsatisfying. My first working Haskell function was
written in Ruby. Surely, with the lazy goodness of Haskell, we must be able to
build a better FizzBuzz. A FizzBuzz generator. An inifinite stream of Fizzes
and Buzzes. And so I searched, and I found
[this](http://pragprog.com/magazines/2012-08/thinking-functionally-with-haskell)
article by Paul Callaghan, which showed me how I might achieve the Infinite
FizzBuzz. And soon I had this:

<script src="https://gist.github.com/kofno/6075856.js?file=InfiniteFizzBuzz.hs">&nbsp;</script>

This is pretty neat. What we're actually doing here is zipping together three
infinite lists: the Fizzes list, the Buzzes list, and an infinite list of
numbers. If the buzzer function receives an empty string from the fizzes, and
an empty string from the buzzes, then it simply shows the number. Otherewise it
concatenates the fizz string and the buzz string.

With this new version of fizzBuzz (fizzBuzz PRIME!), if we want the first
twenty FizzBuzzes, we simply take the first twenty.

    ghci> take 20 fizzBuzz'

And to get the next twenty, we just drop the first twenty.

    ghci> take 20 . drop 20 $ fizzBuzz'

Yay!

So what about Ruby? Well, Ruby 2.0 has added Lazy enumerators (you could
wrangle your own laziness in 1.9, but I was too... well, never mind). So I
thought to myself, "Self, what would it take to build an Infinite FizzBuzz in
Ruby?" And I answered myself, "I dunno. Let's find out!"

### Lazy? You keep using that word

Lazy refers to lazy evaluation. A system that uses lazy evaluation doesn't
evaluate an expression until the value is needed. Haskell is lazy by default.
Ruby is not. And so it takes additional effort to implement Lazy evaluation in
Ruby.

The key to laziness in Ruby is the Enumerators. Different from the Enumeable
mixin, Enumerators are external references to collections. In this way, they
are similar to the external iterators from Java. They also are a duck type for
anything Enumerable, so they can be used interchangeably with those types in
most places.

I think most Ruby developers have worked on a code base where a dataset is
processed by chaining together a handful of Enumerable methods (map, inject,
select, etc.). I know I've seen systems fall over trying to process enormous
datasets in this way. Maybe you've seen a side-effect laden #each call, with
multiple business cases interleaved into one messy block, simply to avoid the
inefficiency processing the entire dataset multiple times.

In Ruby 2.0, Enumerator::Lazy was added. This is a special enumerator that
allows enumerators to be chained in such a way that as each value is evaluated,
it is passed down the chain the next block. Take this example:

    irb> (1..10).map { |n| n * 3 }.select { |n| n.even? }.first(3)
    [6, 12, 18]

In this example, all 10 numbers are processed by #map, producing a new array.
Then all 10 of those numbers are processed by #select, producing yet another
array. Then, finally, we take first three of that array.

This works fine, provided your set is small. But what if you're dealing with a
vary large data set. For example:

    irb> (1..Float::INFINITY).map { |n| n * 3 }.select { |n| n.even? }.first(3)
    ... yeah, keep waiting...

With this set you will never get out of #map.

You may have seen (or written) a solution like this before:

   results = []
   (1..Float::INFINITY).each do |n|
     n = n * 3
     next unless n.even?
     results << n
     break if results.size >= 3
   end
   results

This works, though it's unpleasant.

If we introduce laziness into the computation, we can then process the infinite
set without interleaving business logic, or resorting to next and break.

    irb> (1..Float::INIFINITY).lazy.map { |n| n * 3 }.select { |n| n.even? }.first(3)
    [6, 12, 18]

When you introduce laziness, Ruby changes from processing all of the data at
each stop to taking each piece of data and passing it through the enumerator
chain.

In other words, we start with number one from the range, we multiply it by
three, then we check to see if it is even. It isn't, so it gets thrown out.
Then we grab number two, multiply it by three and get six. Six is even, so we
keep it and it becomes the first value in our set. This process repeats until
we've collected the first three even values. Then Ruby stops processing the
large set because we already have the result of our computation.

Pat Shaughnessy actually provides [a more detailed description of
Enumerator::Lazy](http://ruby.dzone.com/articles/ruby-20-works-hard-so-you-can).
You should go read it.

### Back to the build

Using our Haskell implementation as a reference, we can begin to assemble the
parts we need for the Ruby implementation. First we need a way to represent our
infinite lists of fizzes. buzzes, and numbers.

    [nil, nil, "Fizz"].cycle
    [nil, nil, nil, nil, "Buzz"].cycle
    1..Float::INFINITY

If we zip these together and apply our concatenation logic, we are all set.

<script src="https://gist.github.com/kofno/6075856.js?file=infinite_fizz_buzz.rb">&nbsp;</script>

And when we call this function just like the Haskell version (only backwards)

    irb> fizz_buzz.first(20)
    irb> fizz_buzz.drop(20).first(20)

One problem with the Ruby solution is that, while the list iteration is indeed
lazy (our zip call is not trapped in an infinate loop), the block in the map
call is always evaluated, even for values that we are dropping.

By adding a #puts call to the block, we can see this problem illustrated.

    irb> fizz_buzz.drop(10).first(10)
    Processing 1
    Processing 2
    Processing 3
    Processing 4
    Processing 5
    Processing 6
    Processing 7
    Processing 8
    Processing 9
    Processing 10
    Processing 11
    Processing 12
    Processing 13
    Processing 14
    Processing 15
    Processing 16
    Processing 17
    Processing 18
    Processing 19
    Processing 20
    => [11, "Fizz", 13, 14, "FizzBuzz", 16, 17, "Fizz", 19, "Buzz"]

Since Ruby isn't lazy by default, we are still paying the cost of executing
block, even for the values we don't need. This may not seem like a problem in
this example, since our calculation is cheap, but if this was an expensive
operation, or a high latency request, we would not want to pay the price for
all Fizzes and Buzzes we are just throwing away.

### Thunk it through

A thunk is an expression created to defer evaluation. In Haskell, this is the
default behavior.

    $> ghci
    ghci> let x = 1 + 1
    gchi> :sprint x
    => x = _

That underscore is telling us that 'x' is a thunk; it hasn't been evaluated
yet. I can force the expression to be evaluated by showing it.

    ghci> show x
    ghci> :sprint x
    => x = 2

Now the expression has been evaluated, and now 'x' will always be treated as 2.
No further evaluation needed.

In lazy evaluated languages, all expressions are treated as thunks. Thunks are
evaluated as their results are needed. This is mostly transparent to the
programmer. Ruby, on the other hand, eagerly evaluates expressions. We have to
explicitly tell Ruby to defer a computation. And when we want to defer a
computation in Ruby, we need a Proc.

We can now change our last example so that the computation in the #map block
produces a thunk, rather then evaluating the expression.

<script src="https://gist.github.com/kofno/6075856.js?file=thunk_fizz_buzz.rb">&nbsp;</script>

Now our infinite fizz buzz defers _all_ fizz buzz computation, even for the
results we want to see. If you run the fizz_buzz now, you'll like see a result
like this:

    [#<Proc:0x007f8cd0829ad8@(irb):81 (lambda)>, #<Proc:0x007f8cd08297e0@(irb):81 (lambda)>,
      #<Proc:0x007f8cd0829330@(irb):81 (lambda)>,...]

Since we've explicitly deferred computation, we now must explicitly trigger
computation for the values we want.

    irb> fizz_buzz.drop(10).first(10).map!(&:call)
    Processing 11
    Processing 12
    Processing 13
    Processing 14
    Processing 15
    Processing 16
    Processing 17
    Processing 18
    Processing 19
    Processing 20
    => [11, "Fizz", 13, 14, "FizzBuzz", 16, 17, "Fizz", 19, "Buzz"]

Jackpot! We can see that our #puts message was only evaluated in the
calculations we cared about. We created a certain amount of overhead by
creating procs for every computation and then throwing some away, but if the
computation is suitably expensive, then the overhead is negligiable, compared
to only performing the computation on the values we care about.

### Conclusion

We've explored lazy evaluation by building an infinite FizzBuzz solution in
Ruby, based on a similar solution developed in Haskell. Hopefully we have a bit
deeper of an understanding of lazy evaluation and how it can be applied in an
eager system, like Ruby.

We've seen how Enumerator::Lazy can help us avoid certain inefficiencies that,
until now, has become common place (dare I say, idiomatic) when dealing with
Enumerable methods in Ruby.

And we've also seen how, even when using Enumerator::Lazy, we must be vigilant
about Ruby's eagerness. There are times when we must explicitly defer
computations, particualrly expensive computations, if we are to enjoy the full
benefits of lazy evaluation.

Lazy evaluation may not be a tool you reach for every day, but it does provide
a simple and elegant solution to a particular set of inefficiencies that are
inherant to processing datasets with Ruby's Enumerable methods.
