util = require 'util'
import Function, Switch, Case, Delay, List, Array, Sequence from util
_ = util.it
require 'std'

local fib
fib = Sequence\Table({1, 1})\appendDelay ->
  fib\zipWith op['+'], fib\tail!

nat = Sequence\iterate (=> @ + 1), 0
twopow = Sequence\iterate0 _ * 2, 1
rnd = Sequence\iterate (-> math.random 9)
seven = Sequence\pure 7

print '# Fibonacci number'
print fib

print '# Natural number'
print nat

print '# 2^k (k = 0, 1, 2, ...)'
print twopow

print '# Random number'
print rnd
print rnd

print '# Sevens'
print seven

print '# Eights'
print seven + 1

print '# Multiples of 7'
print seven\zipWith op['*'], nat

namelist = List\Table {
  {id: 1, name: 'Alice', age: 19}
  {id: 2, name: 'Bob', age: 20}
  {id: 3, name: 'Catherine', age: 18}
  {id: 4, name: 'Dorothy', age: 21}
}

print namelist\filter(=> @age < 20)\orderBy(=> @age)\select(=> @name)

print '# Function combination'
print "(_ + 5 .. _ * 2)(1) ==> #{(_ + 5 .. _ * 2)(1)}"

print '# Partial function'
print nat\map Switch unpack {
  Case (_ % 15)\eq(0), -> 'FizzBuzz'
  Case (_ %  3)\eq(0), -> 'Fizz'
  Case (_ %  5)\eq(0), -> 'Buzz'
  _
}
