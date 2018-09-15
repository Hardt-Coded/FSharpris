# FSharpris
Tetris Clone in F#

Currently there is only a console version. Use the key A, D, S and Spacebar to play it.

It's planned, that I build a Fabulous (Xamarin.ELmish) and a Fable-Version.

### How it works

I basically use 2 layers. One layer for the Gamingfield (type **Gamefield**) which is a 2d list. And the **BrickLayer**, this is the same layer with the same dimensions as the Gamefield. It contains only the current Brick.

For collision testing, I build the next move down and flatten it and the **Gamefield** from a 2D-Lists to a normal list and compare if one of the entry overlaps with the entry of the other.
If collide some of that, i copy the current **Bricklayer**, not the **Next-Move-BrickLayer**, which I used for collision testing, to the **Gamefield** with a "poor mans" or-function.

It's all immutable. And yeah I could have uses arrays instead of a (linked)list. But this is only a proof of knowlegde for me.

The Bricks itself are small 2D Lists. For every rotation and type I had build a 2D-List.

To render the small bricks into the Bricklayer, I add empty rows for ever y-coordinate minus the height of the current brick (the bricks have different heights per rotation), then I map the list of brick rows (Type: int list list) to a full row for the BrickLayer. For that I use the x-coordinate ( \[list of zeros] @ \[brickrow] @ \[rest to fullfill the fieldwidth] ). And than the rest of empty rows.

For every move and rotation I build a new BrickLayer.

### Changelog

2018-09-12 Some performance improvements for the fabulous version. Thanks alot to https://twitter.com/jindraivanek
2018-09-11 Simple Fabulous (Xamarin.Elmish) Version added (currently slow!)
