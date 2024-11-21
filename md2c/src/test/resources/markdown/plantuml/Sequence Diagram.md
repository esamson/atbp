# Sequence Diagram

## Basic Examples

In PlantUML sequence diagrams, the `->` sequence denotes a message sent between
two participants, which are automatically recognized and do not need to be
declared beforehand.

Utilize dotted arrows by employing the `-->` sequence, offering a distinct
visualization in your diagrams.

To improve readability without affecting the visual representation, use reverse
arrows like `<-` or `<--`. However, be aware that this is specifically for
sequence diagrams and the rules differ for other diagram types.

```plantuml
Alice -> Bob: Authentication Request
Bob --> Alice: Authentication Response

Alice -> Bob: Another authentication Request
Alice <-- Bob: Another authentication Response
```

End.
