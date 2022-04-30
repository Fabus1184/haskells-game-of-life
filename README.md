# haskells-game-of-life

Conways "Game Of Life" in Haskell

![](./haskells_game_of_life.gif)


# Binary releases

- Precompiled binaries can be found [here](https://github.com/Fabus1184/haskells-game-of-life/releases)

## Requirements
- Depending on the exact build libgmp may be required
- Terminal emulator with unicode support

# Building from Source

## Requirements
- Recent versions of Cabal and GHC

## Building
1. clone repository:
    ```
    git clone https://github.com/Fabus1184/haskells-game-of-life.git && cd haskells-game-of-life
    ```

2. build & run cabal project
    ```
    cabal build && cabal run
    ```