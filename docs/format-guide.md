# Turing Machine Format Guide

This guide will explain how to define and write Turing Machines using this tool.

## Table of Contents

* [Tape Format](#tape-format)
* [Basic Turing Machines](#basic-turing-machines)
* [Composed Turing Machines](#composed-turing-machines)

## Tape Format

The Turing Machine tape is represented as a sequence of symbols. Each symbol must be one of the following:

* An **uppercase letter** (`A` to `Z`)
* A **digit** (`0` to `9`)

There is one **special reserved symbol**, `B`, which denotes a blank or empty tape cell.
All tape cells will be initialized with `B` unless another symbol is explicitly specified.

The input of a Turing Machine is a string composed of valid symbols.
The machine’s head will start at the **first symbol** of this string.
The output consists of all symbols to the right of the head, including the symbol currently under the head.

### Example

```
B10010XYAA
```

In this example, the input begins with a blank (`B`). This is a common convention when defining Turing Machines,
as it makes it easier to check whether the head is at the beginning of the input string.

## Basic Turing Machines

Turing Machines can be defined in individual files using the `.tm` extension by convention.

### Comments

You can write line comments using `--`, just like in Haskell:

```
-- This is a comment
-- This is another comment
```

### Defining a Turing Machine

A Turing Machine is defined by assigning a name (which must start with an uppercase letter) to a list of transitions enclosed in parentheses.

#### Example: An empty Turing Machine

```tm
EmptyMachine = (

)
```

There is one **reserved name**, `Main`, which refers to the Turing Machine that will be executed when the program runs.

> Why define multiple Turing Machines in the same file?
> You'll find out in the next section: [Composed Turing Machines](#composed-turing-machines).

### Transitions

Each transition follows this format:

```
(<state1>, <pattern1>) -> (<state2>, <pattern2>, <move>)
```

You can list multiple transitions one after another, no separators are required.

During execution, the machine will look for a transition that matches the current state and tape symbol, evaluating transitions in the order they appear.
If no transition matches, the machine halts. Otherwise, the first matching transition is applied: it changes the state to `state2`, writes `pattern2` to the tape, and moves the head in the specified direction.

### States

States are labeled using `q` followed by a number (e.g., `q0`, `q1`, `q42`, ...).
Execution always begins in the **initial state** `q0`.

### Moves

The head movement is controlled by a move directive, which can be:

* `L` — move left
* `R` — move right
* `S` — stay in place

### Patterns

Patterns are used in transitions both to match the current symbol on the tape (`pattern1`) and to define what symbol should be written (`pattern2`).

#### Symbol

A specific symbol (e.g., `A`, `3`, etc.) will match only that especific symbol when used as `pattern1`, and writes that symbol when used as `pattern2`.

#### Wildcard (`_`)

The wildcard matches **any** symbol when used as `pattern1`, and causes **no change** to the tape when used as `pattern2`.

#### Arguments

Although arguments are valid patterns in the language, **they are not supported with this basic Turing Machines**.
They will be covered in the next section.

---

With this knowledge, you're ready to understand the example in [`/examples/AddOne.tm`](/examples/AddOne.tm) in the root folder.

## Composed Turing Machines

In this section, we'll cover how to add arguments to Turing Machines and how to compose them to simplify definitions.

### Composing Machines

Previously, we saw that a Turing Machine consists of a list of transitions. This tool also supports a more expressive syntax for composing Turing Machines.

Inside the parentheses, you can write a sequence of Turing Machines to be executed in order (separated by commas), followed by zero or more conditional branches:

```
| <pattern> -> <turing machine>
```

During execution, the first branch whose pattern matches the current tape head symbol will trigger the execution of the corresponding Turing Machine.

> [!Note]
> When a Turing Machine is invoked:
>
> * The **tape and head** are shared
> * The **state** is reset locally to `q0`

#### Example: Composition and branching

```tm
Right = (
    (q0, _) -> (q1, _, R)
)

Stop = ()

RightUntilBlank = (
    Right
    | B -> Stop
    | _ -> RightUntilBlank
)

Main = (
    RightUntilBlank
)
```

Here:

* `Right` moves the head to the right regardless of the current symbol.
* `RightUntilBlank` calls `Right`, then checks the current symbol:

  * If it’s `B`, it calls `Stop` which does nothing
  * Otherwise, it calls itself recursively
* `Main` simply calls `RightUntilBlank`

For the input `B123B456`, the output will be `B456`.

You can think of each Turing Machine as a "named state" with its own behavior.

### Arguments

You can add arguments to a Turing Machine, which is useful for reducing repetition. Arguments are denoted by a single lowercase letter and can be used in patterns.

Arguments will be replaced with the actual pattern used when the machine is invoked.

To declare an argument, append it to the machine name using an underscore (`_`). You can think of it as suffixing a pattern variable to the machine.

#### Example: Argument usage

```tm
Place_a = (
    (q0, _) -> (q1, a, S)
)

-- Right and Stop definitions omitted for clarity
RightUntil_a = (
    Right
    | a -> Stop
    | _ -> RightUntil_a
)
```

* `Place` replaces the current symbol with the provided argument.
* `RightUntil` generalizes `RightUntilBlank` (defined earlier) to stop at any given symbol.

> [!Note]
> The `Main` machine **cannot** take any arguments.

---

With this knowledge, you're ready to understand the example in [`/examples/SortXY.tm`](/examples/SortXY.tm) in the root folder.
