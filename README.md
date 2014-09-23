# P4 - a "low-level" combinator parsing library for OCaml

This is *not* a successor to P3. It is an alternative that exposes a
more complicated interface. Compared to P3:

  * The interface is more complicated, leading to slightly less
    elegant combinator parsers.

  * The real-world performance often exceeds P3, due to tighter
    control over the Earley phase.
    
  * The interface allows to go beyond context-free grammars. In
    particular, infinitely many non-terminals are supported, as the
    grammar is generated lazily, on-demand.
    

