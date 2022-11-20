

Why `Engine.Cmd` is full of datatypes?
--------------------------------------

Almost each engine instruction is a separate `data` declaration along with
an `instance Cmd`.

At the beginning I followed a classical way of defining instructions as a big
single sum type:

```
data Cmd
  = Init {goto :: Addr}
  | Goto {goto :: Addr}
  | Halt
    ...
```

along with a long `evalCmd` function:

```
evalCmd = \case
  Init{..} -> when (goto > 0) $ jumpTo ?= goto
  Goto{..} -> jumpTo ?= goto
  Halt -> lift $ throwE "Halt!"
  ...
```

Suddenly I realised that it is rather tedious to jump back and forth between
instructions in the datatype and corresponding sections of `evalCmd`. It will be
nice to have in one place everyting related to a single instruction: data
declaration, comments, evaluation steps.

Using `Cmd` class and separate `data` declarations for each instruction allows to group data, code and comments together.
