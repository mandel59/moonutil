id = (...) -> ...
bang = (x) -> x!
insert = table.insert
first = (x, ...) -> x
second = (x, y, ...) -> y
rest = (x, ...) -> ...
map = (f, ...) ->
  t = {}
  for x in *{...}
    insert t, f x
  unpack t
foldl = (f, x, y, ...) ->
  if y == nil
    return x
  return foldl f, f(x, y), ...
foldr = (f, x, y, ...) ->
  if y == nil
    return x
  return f x, foldr f, y, ...
foldr0 = (f, x, y, ...) ->
  if y == nil
    return
  return f y, foldr0 f, x, ...
compose = (g, f) -> (...) -> g f ...
delayfunc = (f) -> (...) -> f map bang, ...

class Tuple
  new: (...) =>
    for k, v in ipairs {...}
      @[k] = v
  unpack: => unpack @
  next: (i) => next @, i

class Functor extends Tuple
  add: (o) => @map((x) -> x + o)
  sub: (o) => @map((x) -> x - o)
  mul: (o) => @map((x) -> x * o)
  div: (o) => @map((x) -> x / o)
  mod: (o) => @map((x) -> x % o)
  pow: (o) => @map((x) -> x ^ o)
  unm: => @map((x) -> -x)
  cat: => (o) => @map((x) -> x .. o)
  eq: (o) => @map((x) -> x == o)
  lt: (o) => @map((x) -> x < o)
  le: (o) => @map((x) -> x <= o)
  gt: (o) => @map((x) -> x > o)
  ge: (o) => @map((x) -> x >= o)
  inv: => @map((x) -> 1 / x)
  not: => @map((x) -> not x)
  index: (key) => @map((x) -> x[key])
  newindex: (key, value) =>
    @map (x) ->
      x[key] = value
      return x
  call: (...) =>
    a = {...}
    @map((x) -> x unpack a)
  method: (key, ...) => 
    a = {...}
    @map((x) -> x[key] x, unpack a)

class Function extends Functor
  __call: (...) => @[1] ...
  __add: (o) => @add o
  __sub: (o) => @sub o
  __mul: (o) => @mul o
  __div: (o) => @div o
  __mod: (o) => @mod o
  __pow: (o) => @pow o
  __unm: => @unm!
  __concat: (f) =>
    f, fp = f\extract!
    g, gp = @extract!
    @@ compose(g, f), if fp and gp
      (...) -> gp(...) and fp g(...)
    else
      fp or gp
  eqA: (o) => @map((x) -> (y) -> x == y)\apply(o)
  ltA: (o) => @map((x) -> (y) -> x < y)\apply(o)
  leA: (o) => @map((x) -> (y) -> x <= y)\apply(o)
  fst: => @map(first)
  snd: => @map(second)
  rst: => @map(rest)
  @Function: (...) => @ ...
  @Case: (p, f) => @@Function ((...) -> f ... if p ...), p
  @pure: (x) => @Function (-> x)
  @pureDelay: (x) => @Function x
  extract: => unpack @
  map: (f) =>
    @@Function(f)\__concat @
  apply: (other) =>
    f, fp = other\extract!
    g, gp = @extract!
    @@Function ((...) -> g(...) f(...)), if fp and gp
      (...) -> gp(...) and fp(...)
    else
      fp or gp
  isDefinedAt: (...) =>
    if p = @[2], p ~= nil then p(...) else @__call(...) ~= nil
  orElse: (other) =>
    @@Case ((...) -> @isDefinedAt(...) or other\isDefinedAt(...)), (...) ->
      if @isDefinedAt ... then @__call(...) else other(...)

orElse = (x, y) -> x\orElse y
Case = Function\Case
Switch = (t) -> foldr orElse, unpack t

class Lazy extends Function
  __call: => @extract!
  __add: (o) => @add o
  __sub: (o) => @sub o
  __mul: (o) => @mul o
  __div: (o) => @div o
  __mod: (o) => @mod o
  __pow: (o) => @pow o
  __unm: => @unm!
  @Function: (f) => delayfunc f
  map: (f) =>
    f = (x) -> f x!
    @@ -> f @[1]
  extract: =>
    x = @[2]
    if x == nil
      x = @[1]!
      @[2] = x
    return x
  duplicate: => @@ -> @
  extend: (f) => @@ -> f @

class Array extends Functor
  __tostring: => @tostring!
  tostring: => '{' .. table.concat(@, ',') .. '}'
  __add: (o) => @add o
  __sub: (o) => @sub o
  __mul: (o) => @mul o
  __div: (o) => @div o
  __mod: (o) => @mod o
  __pow: (o) => @pow o
  __unm: => @unm!
  __concat: (o) => @append o
  @Nil: => @!
  @Cons: (h, t) => @ h, unpack t
  @ConsDelay: (h, t) => @ h!, unpack t!
  @For: (f, s, var) =>
    with x = @!
      k = 0
      for _, v in f, s, var
        k += 1
        x[k] = v
  @Table: (t) => @For ipairs t
  @List: (t) => @For t\pairs!
  empty: => #@ == 0
  head: => @[1]
  tail: => @@ unpack [x for x in *@], 2
  at: (n) => @[n]
  map: (f) =>
    with a = @@!
      for k, v in ipairs @
        a[k] = f v
  append: (other) =>
    with a = @@!
      for v in *@
        insert a, v
      for v in *other
        insert a, v
  concat: =>
    with a = @@!
      for u in *@
        for v in *u
          insert a, v
  concatMap: (f) =>
    with a = @@!
      for u in *@
        for v in *(f u)
          insert a, v
  @unfoldr: (f, var) =>
    g = (v) =>
      value, key = f v
      key, value
    @For g, nil, var
  foldl: (x, f) =>
    for v in *@
      x = f x, v
    return x
  foldl1: (f) =>
    x = @[1]
    for i = 2, #@
      x = f x, @[i]
    return x
  sort: (comp) =>
    a = Array\For(@pairs!)
    table.sort a, comp
    return a
  select: (k) => @map k
  orderBy: (k) => @sort (x, y) -> k(x) < k(y)

class List extends Functor
  __tostring: => @tostring!
  __add: (o) => @add o
  __sub: (o) => @sub o
  __mul: (o) => @mul o
  __div: (o) => @div o
  __mod: (o) => @mod o
  __pow: (o) => @pow o
  __unm: => @unm!
  __concat: (o) => @append o
  @Nil: => @ nil
  @Cons: (h, t) => @ConsDelay (-> h), (-> t)
  @ConsDelay: (h, t) => @ h, t
  @For: (f, s, var) =>
    key, value = f s, var
    if key == nil
      @Nil!
    else
      @ConsDelay (-> value), (Lazy -> @For f, s, key)
  @Table: (t) => @For ipairs t
  @List: (t) => @For t\pairs!
  @unfoldr: (f, var) =>
    value, key = f var
    if key == nil
      @Nil!
    else
      @ConsDelay (-> value), (Lazy -> @unfoldr f key)
  @repeatDelay: (x) =>
    local p
    p = @ConsDelay x, -> p
    return p
  @iterate0: (f, a) => @Cons a, @iterate f, a
  @iterate: (f, a) => @iterateDelay (delayfunc f), (-> a)
  @iterateDelay: (f, a) =>
    a = f a
    da = (-> a)
    @ConsDelay da, (Lazy -> @iterateDelay f, da)
  empty: => @[1] == nil
  head: => if x = @[1], x ~= nil then x!
  tail: => if x = @[2], x ~= nil then x!
  next: (i) =>
    x = if i == nil then @ else i\tail!
    if x\empty!
      nil
    else
      x, x\head!
  at: (n) =>
    if @empty!
      nil
    elseif n > 1
      @tail!\at n - 1
    else
      @head!
  ellipsize: (n) =>
    n = 10 if n == nil
    if @empty!
      @@Nil!
    elseif n < 1
      @@Cons '...', @@Nil!
    else
      @@ConsDelay (Lazy -> tostring(@head!)), (Lazy -> @tail!\ellipsize(n - 1))
  tostring: =>
    if @empty!
      '[]'
    else
      '[' .. @ellipsize!\foldr1((x, y) -> x .. ',' .. y) .. ']'
  unpack: (i, j) =>
    if @empty!
      return
    elseif j ~= nil
      if j < 1
        return
      elseif i ~= nil and i > 1
        return @tail!\unpack i - 1, j - 1
      else
        return @head!, @tail!\unpack nil, j - 1
    elseif i ~= nil and i > 1
      return @tail!\unpack i - 1
    else
      return @head!, @tail!\unpack!
  pairs: => @next, @
  each: (f) =>
    for _, v in @pairs!
      break if f(v) == false
  map: (f) => @mapDelay delayfunc f
  mapDelay: (f) =>
    if @empty!
      @@Nil!
    else
      @@ConsDelay (Lazy -> f (-> @head!)), (Lazy -> @tail!\mapDelay f)
  append: (other) => @appendDelay (-> other)
  appendDelay: (other) =>
    if @empty!
      other!
    else
      @@ConsDelay (Lazy -> @head!), (Lazy -> @tail!\appendDelay other)
  foldl: (x, f) =>
    z = @
    while not z\empty!
      x = f x, z\head!
      z = z\tail!
    return x
  foldlDelay: (x, f) =>
    if @empty!
      x!
    else
      @tail!\foldlDelay (Lazy -> f (-> x), (Lazy -> @head!)), f
  foldr: (x, f) =>
    if @empty!
      x
    else
      f @head!, @tail!\foldr x, f
  foldrDelay: (x, f) =>
    if @empty!
      x!
    else
      f (Lazy -> @head!), (Lazy -> @tail!\foldrDelay x, f)
  foldl1: (f) =>
    @tail!\foldl @head!, f
  foldr1: (f) =>
    t = @tail!
    if t\empty!
      @head!
    else
      f @head!, @tail!\foldr1 f
  concat: => @concatDelay!
  concatDelay: => @foldrDelay (Lazy -> @@Nil!), (x, y) -> x!\appendDelay y
  concatMap: (f) => @map(f)\concat!
  concatMapDelay: (f) => @mapDelay(f)\concatDelay!
  take: (n) =>
    if n < 1 or @empty!
      @@Nil!
    else
      @@ConsDelay (Lazy -> @head!), (Lazy -> @tail!\take(n - 1))
  filter: (p) =>
    if @empty!
      @@Nil!
    else
      head = @head!
      if p head
        @@ConsDelay (-> head), (Lazy -> @tail!\filter p)
      else
        @tail!\filter p
  collect: (f) =>
    if @empty!
      @@Nil!
    else
      head = @head!
      if x = f head, x ~= nil
        @@ConsDelay (-> x), (Lazy -> @tail!\collect f)
      else
        @tail!\collect f
  takeWhile: (p) =>
    if @empty!
      @@Nil!
    else
      head = @head!
      if p head
        @@ConsDelay (-> head), (Lazy -> @tail!\takeWhile p)
      else
        @@Nil!
  splitAt: (n) =>
    if @empty!
      @, @@Nil!
    elseif n < 1
      @@Nil!, @
    else
      x, y = @tail!\splitAt n - 1
      (@@Cons @head!, x), y
  zipWith: (f, other) => @zipWithDelay(delayfunc(f), Lazy -> other)
  longZipWith: (f, other) => @longZipWithDelay(delayfunc(f), Lazy -> other)
  zipWithDelay: (f, other) =>
    o = other!
    if @empty! or o\empty!
      @@Nil!
    else
      @@ConsDelay (Lazy -> f (Lazy -> @head!), (Lazy -> o\head!)), (Lazy -> @tail!\zipWithDelay(f, Lazy -> o\tail!))
  longZipWithDelay: (f, other) =>
    o = other!
    if @empty!
      o
    elseif o\empty!
      @
    else
      @@ConsDelay (Lazy -> f (Lazy -> @head!), (Lazy -> o\head!)), (Lazy -> @tail!\longZipWithDelay(f, Lazy -> o\tail!))
  zipApply: (other) => @map(delayfunc)\zipApplyDelay(-> other)
  zipApplyDelay: (other) =>
    o = other!
    if @empty! or o\empty!
      @@Nil!
    else
      @@ConsDelay (Lazy -> @head! (Lazy -> o\head!)), (Lazy -> @tail!\zipApplyDelay(Lazy -> o\tail!))
  @pure: (x) => @pureDelay (-> x)
  @pureDelay: (x) => @ConsDelay x, (Lazy -> @Nil!)
  apply: (other) => (@map delayfunc)\applyDelay(-> other)
  applyDelay: (other) => @concatMapDelay (f) -> other!\mapDelay f!
  bind: (f) => @concatMap(f)
  bindDelay: (f) => @concatMapDelay(f)
  extract: => @head!
  extend: (f) => @@ConsDelay (Lazy -> f @), (Lazy -> (@tail! or @@Nil!)\extend f)
  duplicate: => @@ConsDelay (-> @), (Lazy -> (@tail! or @@Nil!)\duplicate!)
  diagonal: => @diagonalDelay!
  diagonalDelay: =>
    if @empty!
      @@Nil!
    else
      @@ConsDelay (Lazy -> @head!\head!), (Lazy -> @tail!\map(=> @tail! or @@Nil!)\diagonal!)
  cycle: =>
    f = (i) ->
      x = i!\tail!
      if x\empty!
        @
      else
        x
    (@@iterateDelay f, (-> @))\mapDelay (l) -> l!\head!
  sort: (comp) =>
    a = Array\For(@pairs!)
    table.sort a, comp
    return List\For ipairs a
  select: (k) => @map k
  orderBy: (k) => @sort (x, y) -> k(x) < k(y)

class Sequence extends List
  __tostring: => @tostring!
  __add: (o) => @add o
  __sub: (o) => @sub o
  __mul: (o) => @mul o
  __div: (o) => @div o
  __mod: (o) => @mod o
  __pow: (o) => @pow o
  __unm: => @unm!
  __concat: (o) => @append o
  concatDelay: => @diagonalDelay!
  @pureDelay: (x) => @repeatDelay x
  applyDelay: (other) => @zipApplyDelay other

return {
  :id
  it: Function id
  :bang
  :insert
  :first
  :second
  :rest
  :map
  :foldl
  :foldr
  :foldr0
  :compose
  :delayfunc
  :Function
  :orElse
  :Switch
  :Case
  :Lazy
  :Tuple
  :List
  :Array
  :Tuple
  :Sequence
}
