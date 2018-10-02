# Spx: Lispy limited to SmartParens

Spx is a minor mode which only takes the idea of "automatic mode switching" from
Lispy and restricts it to simple wrappers around SmartParens commands. Automatic
mode switching means that when the point is at parenthesis/brackets/braces, your
regular keys activate commands instead of self-inserting themselves.

## Usage Examples

Enable the mode with spx-mode, place the point before the left parenthesis
(or brace or bracket) of an sexp and use the action keys. Note that for the
left parenthesis you have to place the point before the parenthesis, and for
the right parenthesis you have to place the point after the parenthesis in
order to activate the bindings.

Example 1:
(the point is marked with |)
```
    (1 2 |(3 4) 5 6)
    pressing k produces
    (1 2 5 6)
```

Example 2:
```    
    (1 2 (3 4)| 5 6)
    pressing k produces
    (1 2 5 6)
```
