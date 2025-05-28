interface Z {
  tag: "Z";
}

interface S {
  tag: "S";
  predecessor: Nat;
}

type Nat = Z | S;

function plus(x: Nat, y: Nat): Nat {
  if (y.tag === "Z") {
    return x;
  }

  return {
    tag: "S",
    predecessor: plus(x, y.predecessor),
  };
}
