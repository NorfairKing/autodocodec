[
  (-1)
  [(-1)]
  [
    (-1)
    2
  ]
  (builtins.toString (-1))
  [(builtins.toString 1)]
  (with builtins;
  toString 1)
  (({ a }:
  a) {
    a = 1;
  })
  (builtins.sub 3 1)
  ((x: x) 1)
  ((x: y: builtins.sub x y) 3 1)
  (builtins.head [(x: x)] 1)
  1
  (let
    a = 1;
    b = builtins.add a 1;
  in
  b)
  (if true then 1 else 2)
  (if false then 1 else if true then 2 else 3)
  ((if true then (x: x) else x: 2) 1)
  (if builtins.isInt 1 then {
    a = 1;
  } else { })
  (({
    a = 1;
  } // {
    a = 2;
    b = 1;
  }) // {
    b = 3;
  })
  ({
    a = 1;
  } // {
    a = 2;
  } // {
    a = 3;
  })
  ((x: x) {
    a = 1;
  } // {
    b = 2;
  })
  ((x: x) {
    a = 1;
  }).a
  ({
    a = 1;
  } // {
    a = 2;
  }).a
  {
    "a b" = 1;
  }."a b"
  {
    a = {
      b = 1;
    };
  }.a.b
  (let
    x = {
      a = 1;
    };
  in
  x).a
  ({
    f = x: x;
  }.f 1)
]
